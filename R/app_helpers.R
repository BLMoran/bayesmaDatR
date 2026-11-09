outcome_types <- function() {
  c(binary = "Binary", continuous = "Continuous", count = "Count (person-time)")
}

outcome_key_columns <- function() {
  c("study_id", "outcome_id", "outcome_name", "timepoint", "int_arm", "ctrl_arm", "measure")
}

outcome_type_columns <- function(type) {
  stats <- switch(type,
    binary = c("n", "event"),
    continuous = c("n", "mean", "sd", "median", "q1", "q3", "min", "max"),
    count = c("n", "event", "time")
  )
  c(as.vector(t(outer(stats, c("int", "ctrl"), paste, sep = "_"))), "n_clusters", "n_periods", "rho_w", "rho_b", "rho_wp")
}

combined_cell_columns <- function() {
  c(iqr_int = "Q1, Q3 in one cell", iqr_ctrl = "Q1, Q3 in one cell",
    range_int = "min, max in one cell", range_ctrl = "min, max in one cell")
}

outcome_type_measure <- function(type) {
  c(binary = "OR", continuous = "MD", count = "IRR")[[type]]
}

legacy_column_aliases <- function() {
  c(study_id = "study", author = "author", year = "year",
    n_int = "n_intervention", n_ctrl = "n_control",
    event_int = "event_intervention", event_ctrl = "event_control",
    mean_int = "mean_intervention", mean_ctrl = "mean_control",
    sd_int = "sd_intervention", sd_ctrl = "sd_control",
    median_int = "median_intervention", median_ctrl = "median_control",
    q1_int = "q1_intervention", q1_ctrl = "q1_control",
    q3_int = "q3_intervention", q3_ctrl = "q3_control",
    min_int = "min_intervention", min_ctrl = "min_control",
    max_int = "max_intervention", max_ctrl = "max_control",
    event_int = "count_intervention", event_ctrl = "count_control",
    time_int = "time_intervention", time_ctrl = "time_control",
    iqr_int = "iqr_intervention", iqr_ctrl = "iqr_control",
    range_int = "range_intervention", range_ctrl = "range_control",
    n_clusters = "clusters", n_clusters = "number_of_clusters", n_clusters = "num_clusters",
    n_clusters = "k_clusters", n_periods = "periods", n_periods = "number_of_periods",
    n_periods = "num_periods", rho_w = "icc", rho_w = "icc_w",
    int_arm = "study_group", control = "control", control = "comparator", intervention = "intervention")
}

normalise_column_name <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove_all("^_|_$")
}

best_column_matches <- function(expected, file_cols, prefix = "") {
  aliases <- legacy_column_aliases()
  candidates <- normalise_column_name(c(expected, aliases[names(aliases) == expected]))
  normalised <- normalise_column_name(file_cols)
  rank <- match(normalised, candidates)
  generic <- file_cols[!is.na(rank)][order(rank[!is.na(rank)])]
  prefix_tokens <- setdiff(stringr::str_split_1(normalise_column_name(prefix), "_"), "")
  specific <- character()
  loose <- character()
  if (length(prefix_tokens)) {
    tokens <- stringr::str_split(normalised, "_")
    has_prefix <- purrr::map_lgl(tokens, \(t) all(prefix_tokens %in% t))
    rest <- purrr::map(tokens, \(t) t[!t %in% prefix_tokens])
    stripped <- purrr::map_chr(rest, \(t) paste(t, collapse = "_"))
    hit <- has_prefix & stripped %in% candidates
    specific <- file_cols[hit][order(match(stripped[hit], candidates))]
    candidate_tokens <- stringr::str_split(candidates, "_")
    loose_hit <- has_prefix & !hit & purrr::map_lgl(rest, \(r) {
      any(purrr::map_lgl(candidate_tokens, \(cand) tokens_in_order(cand, r)))
    })
    loose <- file_cols[loose_hit]
  }
  unique(c(specific, loose, generic))
}

tokens_in_order <- function(candidate, tokens) {
  if (!length(tokens) || tokens[1] != candidate[1] || tokens[length(tokens)] != candidate[length(candidate)]) {
    return(FALSE)
  }
  positions <- match(candidate, tokens)
  !anyNA(positions) && !is.unsorted(positions)
}

best_column_match <- function(expected, file_cols, prefix = "") {
  matches <- best_column_matches(expected, file_cols, prefix)
  if (length(matches)) matches[1] else "(Skip)"
}

make_study_id <- function(author, year) {
  author <- trimws(as.character(author))
  year <- trimws(as.character(year))
  dplyr::case_when(
    is_blank(author) ~ NA_character_,
    is_blank(year) ~ author,
    .default = paste(author, year)
  )
}

read_upload_sheets <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  with_header <- function(df) {
    df <- as.data.frame(df, check.names = FALSE)
    rbind(names(df), as.matrix(format_cells(df)))
  }
  sheets <- switch(ext,
    csv = {
      width <- max(utils::count.fields(path, sep = ",", quote = "\"", blank.lines.skip = FALSE), na.rm = TRUE)
      raw <- utils::read.csv(path, header = FALSE, colClasses = "character", na.strings = "",
                             col.names = paste0("V", seq_len(width)), fill = TRUE,
                             blank.lines.skip = FALSE)
      stats::setNames(list(unname(as.matrix(raw))), tools::file_path_sans_ext(name))
    },
    xls = , xlsx = {
      sheet_names <- readxl::excel_sheets(path)
      purrr::map(sheet_names, \(s) {
        as.matrix(readxl::read_excel(path, sheet = s, col_names = FALSE, col_types = "text",
                                     .name_repair = "minimal"))
      }) |>
        stats::setNames(sheet_names)
    },
    rds = {
      obj <- readRDS(path)
      if (is.data.frame(obj)) {
        stats::setNames(list(with_header(obj)), tools::file_path_sans_ext(name))
      } else if (is.list(obj) && length(obj) && all(purrr::map_lgl(obj, is.data.frame))) {
        purrr::map(obj, with_header)
      } else {
        cli::cli_abort("The .rds file must contain a data frame or a named list of data frames.")
      }
    },
    rda = , rdata = {
      env <- new.env(parent = emptyenv())
      load(path, envir = env)
      purrr::map(Filter(is.data.frame, as.list(env)), with_header)
    },
    cli::cli_abort("Unsupported file type {.val {ext}}; use XLSX, CSV, RDA or RDS.")
  )
  sheets <- Filter(\(m) length(m) > 0 && ncol(m) > 0, sheets)
  if (!length(sheets)) cli::cli_abort("No data found in {.file {name}}.")
  if (is.null(names(sheets)) || any(!nzchar(names(sheets)))) names(sheets) <- paste0("data_", seq_along(sheets))
  sheets
}

format_cells <- function(df) {
  df[] <- purrr::map(df, \(x) {
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    out
  })
  df
}

guess_header_row <- function(raw, search = 10) {
  if (!nrow(raw)) return(1L)
  top <- raw[seq_len(min(search, nrow(raw))), , drop = FALSE]
  filled <- rowSums(!is.na(top) & nzchar(trimws(top)))
  which(filled == max(filled))[1]
}

sheet_from_raw <- function(raw, header_row = 1) {
  header_row <- max(1L, min(as.integer(header_row %||% 1L), nrow(raw)))
  header <- trimws(as.character(raw[header_row, ]))
  header[is.na(header) | !nzchar(header)] <- paste0("column_", which(is.na(header) | !nzchar(header)))
  body <- raw[-seq_len(header_row), , drop = FALSE]
  blank <- is.na(body) | !nzchar(trimws(body))
  body <- body[rowSums(!blank) > 0, , drop = FALSE]
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  names(df) <- make.unique(header)
  df[] <- purrr::map(df, \(x) utils::type.convert(x, as.is = TRUE, na.strings = c("NA", "")))
  rownames(df) <- NULL
  df
}

section_sheet_aliases <- function() {
  list(
    study = c("studies", "study_details", "study", "study_data"),
    outcome = c("outcomes", "outcome_data", "outcome", "outcome_details"),
    rob = c("risk_of_bias", "rob_data", "rob", rob_tools()),
    inspect = c("inspect_sr", "inspect", "inspectsr", "inspect_sr_data")
  )
}

match_section_sheet <- function(section, sheets) {
  normalised <- tolower(gsub("[ .-]", "_", sheets))
  aliases <- section_sheet_aliases()[[section]]
  hit <- match(aliases, normalised)
  hit <- hit[!is.na(hit)]
  if (!length(hit)) {
    suffix <- purrr::map_int(normalised, \(n) {
      any(endsWith(n, paste0("_", aliases)))
    })
    hit <- which(suffix == 1L)
  }
  if (length(hit)) sheets[hit[1]] else NA_character_
}

recognised_sheets <- function(sheets) {
  matched <- purrr::map_chr(names(section_sheet_aliases()), \(sec) match_section_sheet(sec, sheets))
  stats::setNames(matched, names(section_sheet_aliases()))[!is.na(matched)]
}

blank_rows <- function(df, n = 1, values = list()) {
  rows <- df[rep(NA_integer_, n), , drop = FALSE]
  rownames(rows) <- NULL
  for (nm in names(values)) rows[[nm]] <- values[[nm]]
  rows
}

bind_section <- function(df, rows) {
  out <- dplyr::bind_rows(df, rows)
  rownames(out) <- NULL
  out
}

sync_outcome_studies <- function(outcomes, study_ids) {
  outcome_ids <- unique(stats::na.omit(outcomes$outcome_id))
  if (!length(outcome_ids) || !length(study_ids)) return(outcomes)
  template <- outcomes[!is.na(outcomes$outcome_id) & !duplicated(outcomes$outcome_id), , drop = FALSE]
  carry <- intersect(c("outcome_name", "timepoint", "measure"), names(outcomes))
  needed <- tidyr::expand_grid(study_id = study_ids, outcome_id = outcome_ids) |>
    dplyr::anti_join(outcomes, by = dplyr::join_by("study_id", "outcome_id"))
  if (!nrow(needed)) return(outcomes)
  rows <- blank_rows(outcomes, nrow(needed), list(study_id = needed$study_id, outcome_id = needed$outcome_id))
  shared <- template[match(needed$outcome_id, template$outcome_id), carry, drop = FALSE]
  rows[carry] <- shared
  bind_section(outcomes, rows) |>
    dplyr::arrange(match(.data$outcome_id, outcome_ids), match(.data$study_id, study_ids))
}

sync_rob_studies <- function(rob, study_ids, rob_tool) {
  have <- rob$study_id[rob$rob_tool %in% rob_tool & rob$rob_level %in% "study"]
  need <- setdiff(study_ids, have)
  if (!length(need)) return(rob)
  for (nm in rob_domains(rob_tool)) if (!nm %in% names(rob)) rob[[nm]] <- rep(NA_character_, nrow(rob))
  bind_section(rob, blank_rows(rob, length(need),
                               list(study_id = need, rob_tool = rob_tool, rob_level = "study")))
}

sync_inspect_studies <- function(inspect, studies) {
  need <- setdiff(studies$study_id, inspect$study_id)
  need <- need[!is_blank(need)]
  if (!length(need)) return(inspect)
  meta <- studies[match(need, studies$study_id), intersect(c("author", "year"), names(studies)), drop = FALSE]
  bind_section(inspect, blank_rows(inspect, length(need), c(list(study_id = need), as.list(meta))))
}

rob_button_class <- function(level) {
  dplyr::recode_values(
    level,
    c("Low", "Definitely low") ~ "btn-rob-low",
    c("Low (except confounding)", "Probably low") ~ "btn-rob-lowish",
    c("Some concerns", "Moderate") ~ "btn-rob-some",
    c("Serious", "Probably high") ~ "btn-rob-serious",
    c("High", "Definitely high") ~ "btn-rob-high",
    c("Critical", "Very high") ~ "btn-rob-critical",
    "Unclear" ~ "btn-rob-unclear",
    default = "btn-outline-secondary"
  )
}

dt_editable <- function(df, colnames = names(df), locked = character()) {
  checks <- sprintf('<input type="checkbox" class="row-check" data-row="%d">', seq_len(nrow(df)))
  shown <- cbind(data.frame(.select = checks, stringsAsFactors = FALSE), df)
  DT::datatable(
    shown, colnames = c('<input type="checkbox" class="all-check" title="Tick all rows on this page">', colnames),
    escape = setdiff(seq_len(ncol(shown)), 1), editable = list(target = "cell", disable = list(columns = c(0, match(locked, names(df))))),
    selection = "none", rownames = FALSE, class = "cell-border stripe",
    options = list(
      pageLength = 50, scrollX = TRUE, dom = "tip", autoWidth = FALSE, scrollCollapse = TRUE,
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0, width = "28px", className = "dt-center row-check-cell"),
        list(targets = as.list(match(locked, names(df))), className = "locked-cell")
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  var api = this.api();",
        "  var id = $(api.table().node()).closest('.datatables').attr('id');",
        "  var checked = new Set();",
        "  var send = function() { Shiny.setInputValue(id + '_checked', Array.from(checked), {priority: 'event'}); };",
        "  var restore = function() {",
        "    $(api.table().body()).find('input.row-check').each(function() {",
        "      this.checked = checked.has(parseInt($(this).data('row')));",
        "    });",
        "  };",
        "  send();",
        "  api.on('draw', restore);",
        "  $(api.table().container()).on('change', 'input.all-check', function() {",
        "    var on = this.checked;",
        "    $(api.table().body()).find('input.row-check').each(function() {",
        "      this.checked = on; var r = parseInt($(this).data('row'));",
        "      if (on) checked.add(r); else checked.delete(r);",
        "    });",
        "    send();",
        "  });",
        "  $(api.table().body()).on('change', 'input.row-check', function() {",
        "    var r = parseInt($(this).data('row'));",
        "    if (this.checked) checked.add(r); else checked.delete(r);",
        "    send();",
        "  });",
        "  $(api.table().body()).on('click', 'td', function(e) {",
        "    var td = this;",
        "    if ($(td).hasClass('row-check-cell') || $(td).hasClass('locked-cell') || $(e.target).is('input.row-check')) return;",
        "    if ($(td).find('input, select, textarea').length) return;",
        "    setTimeout(function() {",
        "      if (!$(td).find('input, select, textarea').length) $(td).trigger('dblclick');",
        "    }, 50);",
        "  });",
        "}"
      )
    )
  )
}

write_section_file <- function(df, file, format) {
  switch(format,
    rds = saveRDS(df, file),
    csv = utils::write.csv(df, file, row.names = FALSE, na = ""),
    xlsx = writexl::write_xlsx(df, file)
  )
}

coerce_cell <- function(column, row, value) {
  value <- if (is.null(value) || !nzchar(trimws(value))) NA_character_ else as.character(value)
  number <- suppressWarnings(as.numeric(value))
  numeric_ok <- is.na(value) || !is.na(number)
  if (is.logical(column) && all(is.na(column))) {
    column <- if (numeric_ok) as.numeric(column) else as.character(column)
  }
  if (is.numeric(column) && numeric_ok) {
    column <- as.numeric(column)
    column[row] <- number
  } else {
    column <- as.character(column)
    column[row] <- value
  }
  column
}

folder_picker_available <- function(os = Sys.info()[["sysname"]]) {
  switch(os,
    Darwin = nzchar(Sys.which("osascript")),
    Windows = TRUE,
    nzchar(Sys.which("zenity")) || isTRUE(capabilities("tcltk"))
  )
}

pick_folder <- function(prompt, os = Sys.info()[["sysname"]]) {
  folder <- tryCatch(
    suppressWarnings(switch(os,
      Darwin = system2("osascript", c("-e", shQuote("activate"),
                                      "-e", shQuote(paste0('POSIX path of (choose folder with prompt "', prompt, '")'))),
                       stdout = TRUE, stderr = FALSE),
      Windows = utils::choose.dir(caption = prompt),
      if (nzchar(Sys.which("zenity"))) {
        system2("zenity", c("--file-selection", "--directory", paste0("--title=", shQuote(prompt))),
                stdout = TRUE, stderr = FALSE)
      } else {
        utils::getFromNamespace("tk_choose.dir", "tcltk")(caption = prompt)
      }
    )),
    error = function(e) NULL
  )
  clean_folder(folder)
}

clean_folder <- function(folder) {
  if (!is.null(attr(folder, "status"))) return(NULL)
  folder <- folder[!is.na(folder) & nzchar(folder)]
  if (!length(folder)) return(NULL)
  sub("(.)/$", "\\1", gsub("\\\\", "/", folder[1]))
}
