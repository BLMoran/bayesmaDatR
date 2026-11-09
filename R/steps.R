#' Import mapped columns from a file into an `ma_data` object
#'
#' Reads `path` and fills the sections named in `spec`, the same way the data
#' manager's **Import mapped columns** and **Load recognised sheets as-is**
#' buttons do. The script downloaded from the data manager uses this function
#' to repeat each import.
#'
#' `spec` is a named list with one element per section to import (`studies`,
#' `outcomes`, `risk_of_bias`, `inspect_sr`). Each section gives the `sheet`
#' and `header_row` to read, and `columns`, a named list mapping each field to
#' one or more source columns: when several are given, the first non-empty
#' value in each row is used. If `study_id` is not mapped it is built from
#' `author` and `year`.
#'
#' * `studies` may also give `extra`, source columns kept under their own name.
#' * `outcomes` is a list of blocks, one per outcome, each with a `type`
#'   (`"binary"`, `"continuous"` or `"count"`) and optionally a fixed
#'   `outcome_id` and `timepoint` used when no column is mapped. Continuous
#'   blocks may map `iqr_int`/`iqr_ctrl` and `range_int`/`range_ctrl` to cells
#'   holding both values (see [split_iqr()]).
#' * `risk_of_bias` gives the `tool` (see [rob_tools()]).
#' * A section with `as_is = TRUE` is read without mapping: the sheet already
#'   uses bayesmaDatR column names.
#'
#' Imported outcomes replace existing rows with the same outcome and time
#' point; imported risk-of-bias rows replace existing rows for the same tool;
#' other imported sections replace the section.
#'
#' @param data An `ma_data` object.
#' @param path Path to an XLSX, CSV, RDA or RDS file.
#' @param spec Import specification; see Details.
#' @return `data` with the imported sections.
#' @examples
#' path <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(Author = c("Smith", "Jones"), Year = c(2020, 2021)),
#'                  path, row.names = FALSE)
#' spec <- list(studies = list(sheet = tools::file_path_sans_ext(basename(path)),
#'                             header_row = 1,
#'                             columns = list(author = "Author", year = "Year")))
#' import_ma_data(template_ma_data(), path, spec)$studies
#' @export
import_ma_data <- function(data, path, spec) {
  check_ma_data(data)
  if (!is.list(spec) || !length(spec) || !all(names(spec) %in% names(data))) {
    cli::cli_abort("{.arg spec} must be a named list with elements from {.val {names(data)}}.")
  }
  sheets <- read_upload_sheets(path, basename(path))
  read_sheet <- function(section) {
    raw <- sheets[[section$sheet]]
    if (is.null(raw)) {
      cli::cli_abort("Sheet {.val {section$sheet}} is not in {.file {basename(path)}}.", call = rlang::caller_env(2))
    }
    sheet_from_raw(raw, section$header_row %||% guess_header_row(raw))
  }

  tables <- unclass(data)
  seen <- NULL
  ids_for <- function(df, columns) {
    author <- mapped_column(df, columns$author)
    year <- mapped_column(df, columns$year)
    ids <- dplyr::coalesce(mapped_column(df, columns$study_id), make_study_id(author, year))
    ids[is_blank(ids)] <- NA_character_
    seen <<- dplyr::bind_rows(seen, data.frame(study_id = ids, author = author, year = year))
    list(ids = ids, author = author, year = year)
  }

  as_is <- purrr::keep(spec, \(s) isTRUE(s$as_is))
  for (sec in names(as_is)) {
    df <- read_sheet(as_is[[sec]])
    if (nrow(df)) tables[[sec]] <- standardise_section(df)
  }
  mapped <- spec[!names(spec) %in% names(as_is)]

  if (!is.null(mapped$studies)) {
    s <- mapped$studies
    df <- read_sheet(s)
    id <- ids_for(df, s$columns)
    studies <- data.frame(study_id = id$ids, author = id$author, year = parse_numeric_if_possible(id$year))
    template <- template_ma_data()$studies
    for (col in setdiff(names(template), c("study_id", "author", "year"))) {
      value <- mapped_column(df, s$columns[[col]])
      studies[[col]] <- if (is.numeric(template[[col]])) parse_numeric_if_possible(value) else value
    }
    studies$trial_design <- dplyr::coalesce(normalise_trial_design(studies$trial_design), studies$trial_design)
    for (col in setdiff(s$extra, names(studies))) {
      if (col %in% names(df)) studies[[col]] <- parse_numeric_if_possible(blank_to_na(df[[col]]))
    }
    tables$studies <- studies[!is.na(studies$study_id) & !duplicated(studies$study_id), , drop = FALSE]
  }

  for (block in mapped$outcomes) {
    df <- read_sheet(block)
    columns <- block$columns
    ids <- ids_for(df, columns)$ids
    pick <- function(col, fixed) dplyr::coalesce(mapped_column(df, columns[[col]]), fixed %||% NA_character_)
    outcome_id <- pick("outcome_id", block$outcome_id)
    if (all(is.na(outcome_id))) cli::cli_abort("Every outcome needs an {.field outcome_id} column or value.")
    rows <- data.frame(study_id = ids, outcome_id = outcome_id,
                       outcome_name = mapped_column(df, columns$outcome_name),
                       timepoint = pick("timepoint", block$timepoint),
                       measure = outcome_type_measure(block$type))
    for (col in c("int_arm", "ctrl_arm", outcome_type_columns(block$type))) {
      value <- mapped_column(df, columns[[col]])
      rows[[col]] <- if (col %in% outcome_numeric_columns()) parse_numeric_if_possible(value) else value
    }
    if (block$type == "continuous") {
      for (arm in c("int", "ctrl")) {
        rows <- fill_from_combined(rows, mapped_column(df, columns[[paste0("iqr_", arm)]]),
                                   paste0(c("q1_", "q3_"), arm), split_iqr)
        rows <- fill_from_combined(rows, mapped_column(df, columns[[paste0("range_", arm)]]),
                                   paste0(c("min_", "max_"), arm), split_range)
      }
    }
    rows <- rows[!is.na(rows$study_id) & !is.na(rows$outcome_id), , drop = FALSE]
    data_cols <- outcome_data_columns(block$type)
    mapped_data <- intersect(data_cols, c(names(columns), if (any(grepl("^(iqr|range)_", names(columns)))) data_cols))
    if (length(mapped_data)) {
      has_data <- purrr::map(rows[mapped_data], \(x) !is.na(x)) |> purrr::reduce(`|`)
      rows <- rows[has_data, , drop = FALSE]
    }
    existing <- tables$outcomes
    replaced <- paste(existing$outcome_id, existing$timepoint) %in% paste(rows$outcome_id, rows$timepoint)
    tables$outcomes <- bind_section(existing[!replaced, , drop = FALSE], rows)
  }

  if (!is.null(mapped$risk_of_bias)) {
    s <- mapped$risk_of_bias
    tool <- rlang::arg_match0(s$tool, rob_tools(), arg_nm = "spec$risk_of_bias$tool")
    df <- read_sheet(s)
    rob <- data.frame(study_id = ids_for(df, s$columns)$ids,
                      outcome_id = mapped_column(df, s$columns$outcome_id),
                      rob_tool = tool,
                      rob_level = mapped_column(df, s$columns$rob_level))
    rob$rob_level <- dplyr::coalesce(tolower(rob$rob_level),
                                     dplyr::if_else(is.na(rob$outcome_id), "study", "outcome"))
    for (col in rob_domains(tool)) {
      raw <- mapped_column(df, s$columns[[col]])
      rob[[col]] <- dplyr::coalesce(standardise_judgement(raw, tool), raw)
    }
    rob <- rob[!is.na(rob$study_id) & !duplicated(rob[c("study_id", "outcome_id", "rob_level")]), , drop = FALSE]
    tables$risk_of_bias <- bind_section(
      tables$risk_of_bias[!tables$risk_of_bias$rob_tool %in% tool, , drop = FALSE], rob
    )
  }

  if (!is.null(mapped$inspect_sr)) {
    s <- mapped$inspect_sr
    df <- read_sheet(s)
    id <- ids_for(df, s$columns)
    inspect <- data.frame(study_id = id$ids, author = id$author, year = parse_numeric_if_possible(id$year))
    for (col in setdiff(inspect_sr_columns(), c("study_id", "author", "year"))) {
      value <- mapped_column(df, s$columns[[col]])
      inspect[[col]] <- if (col %in% c(inspect_sr_items(), "d4_3_pvalues")) value else parse_numeric_if_possible(value)
    }
    tables$inspect_sr <- inspect[!is.na(inspect$study_id) & !duplicated(inspect$study_id), , drop = FALSE]
  }

  tables$studies <- add_seen_studies(tables$studies, seen)
  for (sec in names(spec)) {
    tables[[sec]] <- drop_empty_columns(tables[[sec]], keep = core_columns(sec))
  }
  new_ma_data(tables$studies, tables$outcomes, tables$risk_of_bias, tables$inspect_sr) |>
    add_missing_studies() |>
    finish_step()
}

#' Add a Study Details column from a file
#'
#' Matches rows of a sheet to studies by `study_id`, or by author and year,
#' and adds one of its columns to Study Details.
#'
#' @param data An `ma_data` object.
#' @param path Path to an XLSX, CSV, RDA or RDS file.
#' @param source Name of the column to add.
#' @inheritParams rlang::args_dots_empty
#' @param name Name of the new column in Study Details.
#' @param sheet Sheet name or position.
#' @param header_row Row holding the column names.
#' @param keys Named character vector giving the sheet's `study_id`, `author`
#'   and/or `year` columns, e.g. `c(author = "Author", year = "Year")`.
#' @return `data` with the new Study Details column.
#' @export
import_study_column <- function(data, path, source, ..., name = source, sheet = 1, header_row = 1,
                                keys = c(study_id = "study_id")) {
  rlang::check_dots_empty()
  check_ma_data(data)
  sheets <- read_upload_sheets(path, basename(path))
  raw <- sheets[[sheet]]
  if (is.null(raw)) cli::cli_abort("Sheet {.val {sheet}} is not in {.file {basename(path)}}.")
  src <- sheet_from_raw(raw, header_row)
  if (!source %in% names(src)) cli::cli_abort("Column {.val {source}} is not in sheet {.val {sheet}}.")
  if (name %in% names(data$studies)) cli::cli_abort("Study Details already has a column {.val {name}}.")
  data$studies[[name]] <- study_column_values(data$studies, src, source, keys)$values
  finish_step(data)
}

study_column_values <- function(studies, src, source, keys) {
  key <- \(nm) if (!nm %in% names(keys) || !keys[[nm]] %in% names(src)) {
    rep(NA_character_, nrow(src))
  } else {
    as.character(src[[keys[[nm]]]])
  }
  ids <- dplyr::coalesce(key("study_id"), make_study_id(key("author"), key("year")))
  found <- match(studies$study_id, ids)
  list(values = parse_numeric_if_possible(as.character(src[[source]])[found]), matched = sum(!is.na(found)))
}

#' Edit cells of an `ma_data` section
#'
#' Sets each `value` at its `row` and `column`, in order. Values are stored as
#' numbers when the column is numeric and the value is a number, and as text
#' otherwise. In Study Details, editing `author` or `year` rebuilds a
#' `study_id` that was empty or built from them.
#'
#' @param data An `ma_data` object.
#' @param section Section to edit.
#' @param row,column,value Row positions, column names and new values,
#'   recycled to a common length. An empty value clears the cell.
#' @return `data` with the edited cells.
#' @examples
#' data <- template_ma_data() |> add_ma_rows("studies")
#' edit_ma_cells(data, "studies", row = 1, column = c("author", "year"),
#'               value = c("Smith", "2020"))$studies
#' @export
edit_ma_cells <- function(data, section, row, column, value) {
  check_ma_data(data)
  section <- arg_match_section(section)
  edits <- data.frame(row = row, column = column, value = as.character(value))
  df <- data[[section]]
  bad_row <- !edits$row %in% seq_len(nrow(df))
  if (any(bad_row)) cli::cli_abort("{.arg row} must be between 1 and {nrow(df)}, not {.val {edits$row[bad_row]}}.")
  bad_col <- setdiff(edits$column, names(df))
  if (length(bad_col)) cli::cli_abort("{.val {bad_col}} {?is/are} not {?a column/columns} of {.field {section}}.")
  for (i in seq_len(nrow(edits))) {
    r <- edits$row[i]
    col <- edits$column[i]
    auto_id <- if (section == "studies") make_study_id(df$author[r], df$year[r])
    df[[col]] <- coerce_cell(df[[col]], r, edits$value[i])
    id_is_auto <- section == "studies" && isTRUE(is_blank(df$study_id[r]) | df$study_id[r] %in% auto_id)
    if (id_is_auto && col %in% c("author", "year")) df$study_id[r] <- make_study_id(df$author[r], df$year[r])
  }
  data[[section]] <- df
  finish_step(data)
}

#' Add rows to an `ma_data` section
#'
#' @param data An `ma_data` object.
#' @param section Section to add rows to.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs filling
#'   columns of the new rows, e.g. `study_id = c("S1", "S2")`. Other columns
#'   are left empty.
#' @param .n Number of rows to add. Defaults to the longest value in `...`, or
#'   1.
#' @return `data` with the new rows at the end of `section`.
#' @examples
#' template_ma_data() |>
#'   add_ma_rows("outcomes", study_id = c("S1", "S2"), outcome_id = "pain", measure = "MD")
#' @export
add_ma_rows <- function(data, section, ..., .n = NULL) {
  check_ma_data(data)
  section <- arg_match_section(section)
  values <- rlang::list2(...)
  if (length(values) && !rlang::is_named(values)) cli::cli_abort("Every value in {.arg ...} must be named.")
  n <- .n %||% max(1L, lengths(values))
  data[[section]] <- bind_section(data[[section]], blank_rows(data[[section]], n, values))
  finish_step(data)
}

#' Remove rows from an `ma_data` section
#'
#' @param data An `ma_data` object.
#' @param section Section to remove rows from.
#' @param ... <[`data-masked`][dplyr::dplyr_data_masking]> Conditions
#'   identifying the rows to remove, as in [dplyr::filter_out()]. Rows where a
#'   condition is `NA` are kept. Use `TRUE` to remove every row.
#' @return `data` without the matching rows.
#' @examples
#' data <- template_ma_data() |>
#'   add_ma_rows("outcomes", study_id = c("S1", "S2"), outcome_id = c("pain", "mort"))
#' remove_ma_rows(data, "outcomes", outcome_id == "mort")$outcomes$outcome_id
#' remove_ma_rows(data, "outcomes", dplyr::row_number() %in% 1)$outcomes$outcome_id
#' @export
remove_ma_rows <- function(data, section, ...) {
  check_ma_data(data)
  section <- arg_match_section(section)
  if (!...length()) cli::cli_abort("Give at least one condition in {.arg ...}; use {.code TRUE} to remove every row.")
  df <- dplyr::filter_out(data[[section]], ...)
  rownames(df) <- NULL
  data[[section]] <- df
  finish_step(data)
}

#' Add, convert, drop or rename columns of an `ma_data` section
#'
#' Changes are applied in the order of the arguments: new columns are added,
#' types converted, columns dropped, then renamed. `study_id`, `outcome_id`,
#' `rob_tool` and `rob_level` cannot be dropped or renamed.
#'
#' @param data An `ma_data` object.
#' @param section Section to change.
#' @inheritParams rlang::args_dots_empty
#' @param add Names of new, empty columns.
#' @param types Named character vector of new column types, `"text"`,
#'   `"integer"`, `"numeric"` or `"date"`, e.g. `c(year = "integer")`.
#' @param drop Names of columns to drop.
#' @param rename Named character vector, `c(new_name = "old_name")`.
#' @return `data` with the changed section.
#' @examples
#' template_ma_data() |>
#'   modify_ma_columns("studies", add = "country", drop = "rho_wp",
#'                     rename = c(icc = "rho_w"))
#' @export
modify_ma_columns <- function(data, section, ..., add = NULL, types = NULL, drop = NULL, rename = NULL) {
  rlang::check_dots_empty()
  check_ma_data(data)
  section <- arg_match_section(section)
  df <- data[[section]]
  locked <- intersect(c(drop, rename), protected_columns())
  if (length(locked)) cli::cli_abort("{.val {locked}} cannot be dropped or renamed.")
  missing_cols <- setdiff(c(names(types), drop, rename), c(names(df), add))
  if (length(missing_cols)) cli::cli_abort("{.val {missing_cols}} {?is/are} not {?a column/columns} of {.field {section}}.")
  bad_types <- setdiff(types, c("text", "integer", "numeric", "date"))
  if (length(bad_types)) cli::cli_abort("{.arg types} must be {.val text}, {.val integer}, {.val numeric} or {.val date}, not {.val {bad_types}}.")

  for (nm in setdiff(add, names(df))) df[[nm]] <- rep(NA, nrow(df))
  for (nm in names(types)) {
    v <- as.character(df[[nm]])
    v[is_blank(v)] <- NA
    df[[nm]] <- switch(types[[nm]],
                       integer = suppressWarnings(as.integer(v)),
                       numeric = suppressWarnings(as.numeric(v)),
                       date = suppressWarnings(as.Date(v)),
                       v)
  }
  df <- df[setdiff(names(df), drop)]
  names(df)[match(rename, names(df))] <- names(rename)
  if (anyDuplicated(names(df))) cli::cli_abort("Column names in {.field {section}} must be unique.")
  data[[section]] <- df
  finish_step(data)
}

#' Add a row for every study missing from a section
#'
#' * `outcomes`: adds each study to every outcome, copying the outcome's name,
#'   time point and measure.
#' * `risk_of_bias`: adds a study-level row for `rob_tool`.
#' * `inspect_sr`: adds a row with the study's author and year.
#'
#' @param data An `ma_data` object.
#' @param section Section to complete.
#' @inheritParams rlang::args_dots_empty
#' @param rob_tool Risk-of-bias tool for new `risk_of_bias` rows. See
#'   [rob_tools()].
#' @return `data` with the completed section.
#' @export
sync_ma_section <- function(data, section = c("outcomes", "risk_of_bias", "inspect_sr"), ...,
                            rob_tool = "rob2") {
  rlang::check_dots_empty()
  check_ma_data(data)
  section <- rlang::arg_match(section)
  ids <- data$studies$study_id
  ids <- unique(ids[!is_blank(ids)])
  data[[section]] <- switch(section,
    outcomes = sync_outcome_studies(data$outcomes, ids),
    risk_of_bias = sync_rob_studies(data$risk_of_bias, ids, rlang::arg_match0(rob_tool, rob_tools(), arg_nm = "rob_tool")),
    inspect_sr = sync_inspect_studies(data$inspect_sr, data$studies)
  )
  finish_step(data)
}

arg_match_section <- function(section, call = rlang::caller_env()) {
  rlang::arg_match(section, c("studies", "outcomes", "risk_of_bias", "inspect_sr"), error_call = call)
}

finish_step <- function(data) {
  data <- new_ma_data(data$studies, data$outcomes, data$risk_of_bias, data$inspect_sr)
  if (!nrow(data$outcomes)) return(data)
  data$outcomes$design_effect <- tryCatch(
    fill_design_effect(new_ma_data(data$studies, data$outcomes))$outcomes$design_effect,
    error = function(e) rep(NA_real_, nrow(data$outcomes))
  )
  data
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[is_blank(x)] <- NA_character_
  x
}

mapped_column <- function(df, sources) {
  sources <- sources[sources %in% names(df)]
  purrr::map(sources, \(src) blank_to_na(df[[src]])) |>
    purrr::reduce(dplyr::coalesce, .init = rep(NA_character_, nrow(df)))
}

fill_from_combined <- function(rows, combined, into, split) {
  if (all(is.na(combined))) return(rows)
  parts <- split(combined)
  for (i in seq_along(into)) {
    current <- suppressWarnings(as.numeric(rows[[into[i]]]))
    if (is.character(rows[[into[i]]]) && any(!is.na(rows[[into[i]]]) & is.na(current))) next
    rows[[into[i]]] <- dplyr::coalesce(current, parts[[i]])
  }
  rows
}

add_seen_studies <- function(studies, seen) {
  if (is.null(seen)) return(studies)
  seen <- seen[!is.na(seen$study_id) & !duplicated(seen$study_id), , drop = FALSE]
  seen$year <- parse_numeric_if_possible(seen$year)
  known <- match(studies$study_id, seen$study_id)
  for (col in c("author", "year")) {
    if (!col %in% names(studies)) studies[[col]] <- rep(seen[[col]][NA_integer_], nrow(studies))
    fill <- is_blank(studies[[col]]) & !is.na(known)
    if (any(fill)) {
      if (is.numeric(seen[[col]])) studies[[col]] <- as.numeric(studies[[col]])
      studies[[col]][fill] <- seen[[col]][known[fill]]
    }
  }
  new <- seen[!seen$study_id %in% studies$study_id, , drop = FALSE]
  if (!nrow(new)) return(studies)
  bind_section(studies, new)
}
