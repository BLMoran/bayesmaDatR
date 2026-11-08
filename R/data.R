#' Create a Bayesian meta-analysis data object
#'
#' @param studies Study-level metadata. Must include `study_id` when non-empty.
#' @param outcomes Outcome-level data, one row per study, outcome, time point
#'   and comparison.
#' @param risk_of_bias Optional risk-of-bias data. See [add_rob()].
#' @param inspect_sr Optional INSPECT-SR data, generally one row per study.
#' @return An object of class `ma_data`: a list of four data frames.
#' @export
new_ma_data <- function(studies = data.frame(), outcomes = data.frame(),
                        risk_of_bias = data.frame(), inspect_sr = data.frame()) {
  sections <- list(
    studies = studies,
    outcomes = outcomes,
    risk_of_bias = risk_of_bias,
    inspect_sr = inspect_sr
  )
  structure(purrr::map(sections, standardise_section), class = "ma_data")
}

#' @export
print.ma_data <- function(x, ...) {
  cli::cli_text("{.cls ma_data} with {length(x)} sections:")
  rows <- purrr::map_int(x, nrow)
  cli::cli_ul(paste0(names(x), ": ", rows, " row", ifelse(rows == 1, "", "s")))
  invisible(x)
}

id_columns <- function() {
  c("study_id", "outcome_id", "outcome_name", "timepoint", "int_arm", "ctrl_arm",
    "trial_design", "measure", "rob_tool", "rob_level")
}

standardise_section <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  ids <- intersect(id_columns(), names(df))
  df[ids] <- purrr::map(df[ids], \(x) {
    x <- as.character(x)
    x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
    x
  })
  if ("trial_design" %in% names(df)) {
    df$trial_design <- dplyr::coalesce(normalise_trial_design(df$trial_design), df$trial_design)
  }
  if (all(c("rob_tool", "rob_level") %in% names(df))) df <- standardise_rob(df)
  df
}

standardise_rob <- function(rob) {
  for (tool in intersect(unique(rob$rob_tool), rob_tools())) {
    rows <- which(rob$rob_tool == tool)
    for (col in intersect(rob_domains(tool), names(rob))) {
      raw <- rob[[col]][rows]
      rob[[col]] <- as.character(rob[[col]])
      rob[[col]][rows] <- dplyr::coalesce(standardise_judgement(raw, tool), as.character(raw))
    }
  }
  rob
}

outcome_numeric_columns <- function() {
  arms <- c("int", "ctrl")
  stats <- c("n", "event", "mean", "sd", "median", "q1", "q3", "min", "max", "time")
  c(as.vector(outer(stats, arms, paste, sep = "_")), "n_clusters", "n_periods", "rho_w", "rho_b", "rho_wp")
}

study_template_columns <- function() {
  list(study_id = character(), author = character(), year = integer(), trial_design = character(),
       control = character(), intervention = character(), n_int = numeric(), n_ctrl = numeric(),
       n_clusters = numeric(), n_periods = numeric(), rho_w = numeric(), rho_b = numeric(),
       rho_wp = numeric())
}

order_study_columns <- function(studies) {
  known <- names(study_template_columns())
  studies <- studies[c(intersect(known, names(studies)), setdiff(names(studies), known))]
  optional <- c("n_clusters", "n_periods", "rho_w", "rho_b", "rho_wp")
  empty <- purrr::map_lgl(studies, \(x) all(is.na(x) | !nzchar(trimws(as.character(x)))))
  studies[!(names(studies) %in% optional & empty)]
}

study_default_columns <- function() {
  c("trial_design", "n_int", "n_ctrl", "n_clusters", "n_periods", "cluster_size", "rho_w", "rho_b", "rho_wp")
}

#' Fill outcome rows with their study's default values
#'
#' Study Details can hold `trial_design`, `n_int`, `n_ctrl`, `n_clusters`,
#' `rho_w`, `rho_b` and `rho_wp` for the whole study. An outcome row uses its
#' own value where it has one (e.g. an outcome-specific sample size) and the
#' study's value where its cell is empty.
#'
#' @param outcomes Outcome data frame.
#' @param studies Study Details data frame.
#' @return `outcomes` with the study defaults filled in.
#' @export
fill_study_defaults <- function(outcomes, studies) {
  outcomes <- as.data.frame(outcomes)
  if (!nrow(studies) || !"study_id" %in% names(studies) || !"study_id" %in% names(outcomes)) {
    return(outcomes)
  }
  studies <- studies[!duplicated(studies$study_id) & !is.na(studies$study_id), , drop = FALSE]
  rows <- match(outcomes$study_id, studies$study_id)
  for (col in intersect(study_default_columns(), names(studies))) {
    from_study <- studies[[col]][rows]
    if (!col %in% names(outcomes)) {
      outcomes[[col]] <- from_study
      next
    }
    current <- outcomes[[col]]
    if (is.numeric(from_study) || is.numeric(current)) {
      current <- suppressWarnings(as.numeric(current))
      from_study <- suppressWarnings(as.numeric(from_study))
    } else {
      current <- as.character(current)
      from_study <- as.character(from_study)
    }
    outcomes[[col]] <- dplyr::coalesce(current, from_study)
  }
  outcomes
}

#' Create a blank workbook template
#'
#' @param rob_tool Risk-of-bias tool(s) whose domain columns are added to the
#'   `risk_of_bias` sheet. See [rob_tools()].
#' @return An empty `ma_data` object with the recommended columns.
#' @export
template_ma_data <- function(rob_tool = "rob2") {
  rob_tool <- rlang::arg_match(rob_tool, rob_tools(), multiple = TRUE)
  numeric_cols <- outcome_numeric_columns()
  outcomes <- c(
    list(
      study_id = character(), outcome_id = character(), outcome_name = character(),
      timepoint = character(), int_arm = character(), ctrl_arm = character(),
      measure = character()
    ),
    stats::setNames(rep(list(numeric()), length(numeric_cols)), numeric_cols)
  )
  rob_cols <- unique(unlist(purrr::map(rob_tool, rob_domains)))

  new_ma_data(
    studies = as.data.frame(study_template_columns()),
    outcomes = as.data.frame(outcomes),
    risk_of_bias = as.data.frame(c(
      list(study_id = character(), outcome_id = character(),
           rob_tool = character(), rob_level = character()),
      stats::setNames(rep(list(character()), length(rob_cols)), rob_cols)
    ), check.names = FALSE),
    inspect_sr = template_inspect_sr()
  )
}

template_inspect_sr <- function() {
  cols <- inspect_sr_columns()
  numeric_cols <- c("year", "d4_6_n_control", "d4_6_n_intervention", "d4_6_n_total",
                    "d4_8_mean", "d4_8_n", "d4_8_decimals", "d4_9_statistic",
                    "d4_9_df", "d4_9_pvalue")
  types <- purrr::map(cols, \(x) if (x %in% numeric_cols) numeric() else character())
  as.data.frame(stats::setNames(types, cols))
}

normalise_sheet_names <- function(sheets) {
  normalised <- tolower(gsub("[ .-]", "_", sheets))
  dplyr::recode_values(
    normalised,
    "study_details" ~ "studies",
    "outcome_data" ~ "outcomes",
    "rob_data" ~ "risk_of_bias",
    default = normalised
  )
}

read_workbook_sheets <- function(path) {
  sheets <- readxl::excel_sheets(path)
  purrr::map(sheets, \(s) as.data.frame(readxl::read_excel(path, sheet = s), check.names = FALSE)) |>
    stats::setNames(normalise_sheet_names(sheets))
}

#' Read a workbook into an `ma_data` object
#'
#' @param path Path to an `.xlsx` workbook. Recognised sheets: `studies`,
#'   `outcomes`, `risk_of_bias`, and `inspect_sr` (case-insensitive; the legacy
#'   names `study_details`, `outcome_data` and `rob_data` are also accepted).
#'   Unknown sheets are ignored. Missing or empty sheets are replaced by the
#'   blank template.
#' @return An `ma_data` object.
#' @export
read_ma_workbook <- function(path) {
  if (!file.exists(path)) cli::cli_abort("Workbook does not exist: {.file {path}}.")
  tables <- read_workbook_sheets(path)
  defaults <- template_ma_data()
  get_sheet <- function(target) {
    df <- tables[[target]]
    if (is.null(df) || !ncol(df)) defaults[[target]] else df
  }
  new_ma_data(get_sheet("studies"), get_sheet("outcomes"),
              get_sheet("risk_of_bias"), get_sheet("inspect_sr"))
}

#' Write an `ma_data` object to an Excel workbook or R data file
#'
#' The file type follows the extension of `path`: `.xlsx` for Excel, `.rda`
#' (or `.RData`) for R. In an `.rda` file, each section is saved as its own
#' data frame (`studies`, `outcomes`, `risk_of_bias`, `inspect_sr`, or
#' `<object_name>_studies` and so on), or, for `layout = "single"`, one data
#' frame called `object_name`; [load()] restores them.
#'
#' @param data An `ma_data` object or a named list of data frames.
#' @param path Output path ending in `.xlsx`, `.rda` or `.RData`.
#' @param ... Unused; must be empty.
#' @param layout `"sheets"` (default) writes one sheet (or data frame) per
#'   section, which [read_ma_workbook()] and the app can read back. `"single"`
#'   writes one combined table from [merge_ma_data()].
#' @param object_name For `.rda` files: the name of the saved data frame
#'   (`layout = "single"`, default `"ma_data"`), or a prefix for the section
#'   data frames (`layout = "sheets"`, default none). Must be a valid R name.
#' @return `data`, invisibly.
#' @export
write_ma_workbook <- function(data, path, ..., layout = c("sheets", "single"), object_name = NULL) {
  rlang::check_dots_empty()
  layout <- rlang::arg_match(layout)
  if (!is.null(object_name) && (!rlang::is_string(object_name) || make.names(object_name) != object_name)) {
    cli::cli_abort(c("{.arg object_name} must be a valid R name, e.g. {.val {make.names(object_name[1])}}."))
  }
  is_df_list <- is.list(data) && length(data) > 0 &&
    all(purrr::map_lgl(data, is.data.frame)) && !is.null(names(data))
  if (!is_df_list) {
    cli::cli_abort("{.arg data} must be an {.cls ma_data} object or a named list of data frames.")
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "rda", "rdata")) {
    cli::cli_abort("{.arg path} must end in {.val .xlsx}, {.val .rda} or {.val .RData}, not {.val {ext}}.")
  }
  sheets <- if (layout == "single") {
    stats::setNames(list(merge_ma_data(as_ma_data(data))), object_name %||% "ma_data")
  } else {
    stats::setNames(unclass(data), tolower(names(data)))
  }
  if (!is.null(sheets$studies) && ncol(sheets$studies)) sheets$studies <- order_study_columns(sheets$studies)
  if (ext == "xlsx") {
    if (layout == "single") names(sheets) <- "data"
    writexl::write_xlsx(sheets, path)
  } else {
    if (layout == "sheets" && !is.null(object_name)) names(sheets) <- paste0(object_name, "_", names(sheets))
    env <- list2env(sheets, envir = new.env(parent = emptyenv()))
    save(list = names(sheets), envir = env, file = path)
  }
  invisible(data)
}

as_ma_data <- function(x) {
  if (inherits(x, "ma_data")) return(x)
  new_ma_data(x$studies %||% data.frame(), x$outcomes %||% data.frame(),
              x$risk_of_bias %||% data.frame(), x$inspect_sr %||% data.frame())
}

#' Combine all sections into one data frame
#'
#' One row per study: the study's details first, then each outcome's columns
#' side by side, suffixed with the outcome (and time point, or intervention
#' arm, where an outcome has several), e.g. `outcome_id_hosp_los`,
#' `n_int_hosp_los`, `mean_ctrl_icu_los`. Outcome values include those inherited from Study
#' Details. Risk-of-bias ratings (prefixed `rob_`, suffixed by outcome when
#' they are outcome-level) and INSPECT-SR fields (prefixed `inspect_`) come
#' last. Columns that are empty for every study are left out.
#'
#' @param data An `ma_data` object.
#' @return A data frame with one row per study.
#' @export
merge_ma_data <- function(data) {
  check_ma_data(data)
  studies <- data$studies
  if (!nrow(studies) || !"study_id" %in% names(studies)) {
    studies <- data.frame(study_id = unique(stats::na.omit(data$outcomes$study_id %||% character())))
  }
  studies <- studies[!is.na(studies$study_id) & !duplicated(studies$study_id), , drop = FALSE]
  out <- order_study_columns(studies)

  rob_by_outcome <- any(data$risk_of_bias$rob_level %in% "outcome")
  if (nrow(data$outcomes)) {
    d <- select_outcome(data)
    d$design_effect <- tryCatch(row_design_effect(d), error = \(e) NA_real_)
    inherited <- setdiff(study_default_columns(), c("trial_design", "cluster_size"))
    own_cols <- setdiff(union(names(data$outcomes), c(inherited, "design_effect")), c("study_id", names(studies)))
    own_cols <- union(intersect(c(inherited, "trial_design"), names(data$outcomes)), own_cols)
    preferred <- c("outcome_id", "outcome_name", "timepoint", "measure", "int_arm", "ctrl_arm",
                   outcome_stat_order(), "n_clusters", "n_periods", "cluster_size", "rho_w", "rho_b", "rho_wp",
                   "design_effect")
    own_cols <- c(intersect(preferred, own_cols), setdiff(own_cols, preferred))
    for (g in split_outcome_groups(d, by_arm = TRUE)) {
      cols <- intersect(c(own_cols, if (rob_by_outcome) grep("^rob_", names(g), value = TRUE)), names(g))
      cols <- cols[purrr::map_lgl(g[cols], \(x) any(!is.na(x) & nzchar(trimws(as.character(x)))))]
      block <- g[c("study_id", cols)]
      names(block)[-1] <- paste(cols, attr(g, "label"), sep = "_")
      out <- dplyr::left_join(out, block, by = dplyr::join_by("study_id"), relationship = "one-to-one",
                              na_matches = "never")
    }
  }

  rob <- data$risk_of_bias
  if (!rob_by_outcome && nrow(rob) && "study_id" %in% names(rob)) {
    rob <- rob[!duplicated(rob$study_id) & !is.na(rob$study_id), setdiff(names(rob), c("outcome_id", "rob_level")),
               drop = FALSE]
    rob <- rob[purrr::map_lgl(rob, \(x) any(!is.na(x)))]
    names(rob)[-1] <- paste0("rob_", sub("^rob_", "", names(rob)[-1]))
    out <- dplyr::left_join(out, rob, by = dplyr::join_by("study_id"), na_matches = "never")
  }

  inspect <- data$inspect_sr
  if (nrow(inspect) && "study_id" %in% names(inspect)) {
    inspect <- inspect[!duplicated(inspect$study_id) & !is.na(inspect$study_id),
                       setdiff(names(inspect), c("author", "year")), drop = FALSE]
    inspect <- inspect[purrr::map_lgl(inspect, \(x) any(!is.na(x)))]
    names(inspect)[-1] <- paste0("inspect_", names(inspect)[-1])
    out <- dplyr::left_join(out, inspect, by = dplyr::join_by("study_id"), na_matches = "never")
  }
  rownames(out) <- NULL
  out
}

#' Rename a section's columns to the package schema
#'
#' @param data An `ma_data` object.
#' @param section Section to rename.
#' @param mapping Named character vector, `c(new_name = "old_name")`. Old names
#'   that are not present are ignored.
#' @return `data` with the renamed section.
#' @export
map_ma_columns <- function(data, mapping,
                           section = c("outcomes", "studies", "risk_of_bias", "inspect_sr")) {
  check_ma_data(data)
  section <- rlang::arg_match(section)
  if (!is.character(mapping) || is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
    cli::cli_abort("{.arg mapping} must be a named character vector, e.g. {.code c(n_int = \"N_treat\")}.")
  }
  data[[section]] <- data[[section]] |>
    dplyr::rename(dplyr::any_of(mapping)) |>
    standardise_section()
  data
}

#' Align study records across sections
#'
#' Adds a row to `studies` for every `study_id` used in another section but not
#' yet listed there, and adds any template columns missing from a section.
#'
#' @param data An `ma_data` object.
#' @return The synchronised `ma_data` object.
#' @export
sync_ma_data <- function(data) {
  check_ma_data(data)
  template <- template_ma_data(rob_tool = rob_tools_used(data))
  for (sec in names(template)) {
    missing_cols <- setdiff(names(template[[sec]]), names(data[[sec]]))
    for (nm in missing_cols) {
      data[[sec]][[nm]] <- rep(template[[sec]][[nm]][NA_integer_], nrow(data[[sec]]))
    }
  }
  add_missing_studies(data)
}

add_missing_studies <- function(data) {
  used_ids <- purrr::map(data[c("outcomes", "risk_of_bias", "inspect_sr")], "study_id") |>
    unlist(use.names = FALSE)
  new_ids <- setdiff(stats::na.omit(unique(used_ids)), data$studies$study_id)
  if (length(new_ids)) {
    data$studies <- dplyr::bind_rows(data$studies, data.frame(study_id = new_ids))
  }
  data
}

core_columns <- function(section) {
  switch(section,
    studies = c("study_id", "author", "year", "trial_design"),
    outcomes = c("study_id", "outcome_id", "measure", "n_int", "n_ctrl"),
    risk_of_bias = c("study_id", "outcome_id", "rob_tool", "rob_level"),
    inspect_sr = c("study_id", "author", "year")
  )
}

drop_empty_columns <- function(df, keep = character()) {
  empty <- purrr::map_lgl(df, \(x) all(is.na(x) | !nzchar(trimws(as.character(x)))))
  df[, !empty | names(df) %in% keep, drop = FALSE]
}

rob_tools_used <- function(data) {
  used <- intersect(unique(data$risk_of_bias$rob_tool), rob_tools())
  if (length(used)) used else "rob2"
}

check_ma_data <- function(data, arg = rlang::caller_arg(data), call = rlang::caller_env()) {
  if (!inherits(data, "ma_data")) {
    cli::cli_abort("{.arg {arg}} must be an {.cls ma_data} object; see {.fn new_ma_data} or {.fn read_ma_workbook}.",
                   call = call)
  }
  invisible(data)
}
