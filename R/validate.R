#' Validate managed meta-analysis data
#'
#' Checks identifiers, outcome summaries, trial-design fields, risk-of-bias
#' assessments and INSPECT-SR fields.
#'
#' @param data An `ma_data` object.
#' @return A tibble with one row per issue (`section`, `row`, `column`,
#'   `issue`); zero rows means no issues were found.
#' @export
validate_ma_data <- function(data) {
  check_ma_data(data)
  list(
    validate_study_ids(data),
    validate_trial_design(data$studies, "studies"),
    validate_numeric_columns(data$studies, "studies", study_default_columns()),
    validate_outcomes(data$outcomes, data$studies),
    validate_rob(data$risk_of_bias, data$outcomes),
    validate_inspect_sr(data$inspect_sr)
  ) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(empty_issues())
}

empty_issues <- function() {
  tibble::tibble(section = character(), row = integer(), column = character(), issue = character())
}

issue <- function(section, row, column, text) {
  if (!length(row)) return(empty_issues())
  tibble::tibble(section = section, row = as.integer(row), column = column, issue = text)
}

is_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

validate_study_ids <- function(data) {
  sections <- c("studies", "outcomes", "risk_of_bias", "inspect_sr")
  purrr::map(sections, \(sec) {
    df <- data[[sec]]
    if (!nrow(df)) return(empty_issues())
    if (!"study_id" %in% names(df)) {
      if (sec == "inspect_sr" && all(c("author", "year") %in% tolower(names(df)))) return(empty_issues())
      return(issue(sec, NA, "study_id", "Required identifier column is missing."))
    }
    out <- issue(sec, which(is_blank(df$study_id)), "study_id", "Study identifier is missing.")
    if (sec == "studies") {
      out <- dplyr::bind_rows(out, issue(sec, which(duplicated(df$study_id) & !is_blank(df$study_id)),
                                         "study_id", "Duplicate study_id in studies."))
    } else if (nrow(data$studies) && "study_id" %in% names(data$studies)) {
      unknown <- which(!is_blank(df$study_id) & !df$study_id %in% data$studies$study_id)
      out <- dplyr::bind_rows(out, issue(sec, unknown, "study_id",
                                         "study_id is not listed in studies; see sync_ma_data()."))
    }
    out
  }) |>
    purrr::list_rbind()
}

validate_outcomes <- function(out, studies) {
  if (!nrow(out)) return(empty_issues())
  resolved <- fill_study_defaults(out, studies)
  issues <- list(
    issue("outcomes", NA, setdiff(c("outcome_id", "n_int", "n_ctrl"), names(resolved)),
          "Required column is missing (in Outcome Details or Study Details).")
  )

  if ("outcome_id" %in% names(out)) {
    issues <- c(issues, list(issue("outcomes", which(is_blank(out$outcome_id)), "outcome_id",
                                   "Outcome identifier is missing.")))
  }

  issues <- c(issues, list(validate_trial_design(out, "outcomes"), validate_design_inputs(resolved)))

  if ("measure" %in% names(out)) {
    bad <- which(!is.na(out$measure) & !out$measure %in% effect_measures())
    issues <- c(issues, list(issue("outcomes", bad, "measure",
                                   paste("Expected one of:", paste(effect_measures(), collapse = ", ")))))
  }

  key <- intersect(c("study_id", "outcome_id", "timepoint", "int_arm", "ctrl_arm"), names(out))
  if (all(c("study_id", "outcome_id") %in% key)) {
    issues <- c(issues, list(issue("outcomes", which(duplicated(out[key])), paste(key, collapse = "+"),
                                   "Duplicate study/outcome/timepoint/comparison row.")))
  }

  issues <- c(issues, list(validate_numeric_columns(out, "outcomes", outcome_numeric_columns())))
  for (arm in c("int", "ctrl")) {
    e <- suppressWarnings(as.numeric(resolved[[paste0("event_", arm)]]))
    n <- suppressWarnings(as.numeric(resolved[[paste0("n_", arm)]]))
    if (length(e) && length(n)) {
      issues <- c(issues, list(issue("outcomes", which(e > n), paste0("event_", arm),
                                     "Events exceed the arm's sample size.")))
    }
  }

  purrr::list_rbind(issues)
}

validate_rob <- function(rob, outcomes) {
  if (!nrow(rob)) return(empty_issues())
  issues <- list(issue("risk_of_bias", NA, setdiff(c("rob_tool", "rob_level"), names(rob)),
                       "Required column is missing."))
  if (!all(c("rob_tool", "rob_level") %in% names(rob))) return(purrr::list_rbind(issues))
  if (!"outcome_id" %in% names(rob)) rob$outcome_id <- NA_character_

  issues <- c(issues, list(
    issue("risk_of_bias", which(!rob$rob_level %in% c("study", "outcome")), "rob_level",
          "Expected 'study' or 'outcome'."),
    issue("risk_of_bias", which(!rob$rob_tool %in% rob_tools()), "rob_tool",
          paste("Unrecognised tool; use one of:", paste(rob_tools(), collapse = ", "))),
    issue("risk_of_bias", which(rob$rob_level %in% "outcome" & is_blank(rob$outcome_id)), "outcome_id",
          "Outcome-level assessments require an outcome_id."),
    issue("risk_of_bias", which(rob$rob_level %in% "study" & !is_blank(rob$outcome_id)), "outcome_id",
          "Study-level assessments should not carry an outcome_id.")
  ))

  if (nrow(outcomes) && all(c("study_id", "outcome_id") %in% names(outcomes))) {
    known <- paste(outcomes$study_id, outcomes$outcome_id)
    rows <- which(rob$rob_level %in% "outcome" & !is_blank(rob$outcome_id) &
                    !paste(rob$study_id, rob$outcome_id) %in% known)
    issues <- c(issues, list(issue("risk_of_bias", rows, "outcome_id",
                                   "No matching study_id + outcome_id in outcomes.")))
  }

  key <- dplyr::if_else(rob$rob_level %in% "outcome",
                        paste(rob$study_id, rob$outcome_id, rob$rob_level),
                        paste(rob$study_id, rob$rob_level))
  rating_cols <- setdiff(names(rob), c("study_id", "author", "year", "outcome_id", "rob_level"))
  row_values <- do.call(paste, c(unname(rob[rating_cols]), sep = "\r"))
  dup_key <- duplicated(key) | duplicated(key, fromLast = TRUE)
  conflicting <- dup_key & stats::ave(row_values, key, FUN = \(v) length(unique(v))) > 1
  issues <- c(issues, list(
    issue("risk_of_bias", which(conflicting), "study_id",
          "Conflicting assessments for the same study (or study + outcome)."),
    issue("risk_of_bias", which(duplicated(key) & !conflicting), "study_id",
          "Duplicate assessment row.")
  ))

  for (tool in intersect(unique(rob$rob_tool), rob_tools())) {
    rows <- which(rob$rob_tool == tool)
    required <- rob_domains(tool, include_overall = FALSE, include_optional = FALSE)
    issues <- c(issues, list(issue("risk_of_bias", NA, setdiff(required, names(rob)),
                                   paste("Domain column required for", tool, "is missing."))))
    for (nm in intersect(rob_domains(tool), names(rob))) {
      raw <- rob[[nm]][rows]
      bad <- rows[!is_blank(raw) & is.na(standardise_judgement(raw, tool))]
      issues <- c(issues, list(issue("risk_of_bias", bad, nm,
                                     paste0("Unrecognised ", tool, " judgement; use one of: ",
                                            paste(rob_judgements(tool), collapse = ", ")))))
    }
  }

  purrr::list_rbind(issues)
}

validate_inspect_sr <- function(isp) {
  if (!nrow(isp)) return(empty_issues())
  names(isp) <- tolower(names(isp))
  issues <- list()
  if (!"study_id" %in% names(isp) && all(c("author", "year") %in% names(isp))) {
    issues <- c(issues, list(issue("inspect_sr", which(is_blank(isp$author) | is_blank(isp$year)),
                                   "author/year",
                                   "INSPECT-SR rows require both author and year when study_id is absent.")))
  }
  items <- inspect_sr_items()
  issues <- c(issues, list(issue("inspect_sr", NA, setdiff(items, names(isp)),
                                 "INSPECT-SR assessment field is missing.")))
  allowed <- c("no concerns", "some concerns", "serious concerns")
  issues <- c(issues, purrr::map(intersect(items, names(isp)), \(nm) {
    values <- tolower(trimws(as.character(isp[[nm]])))
    issue("inspect_sr", which(!is_blank(values) & !values %in% allowed), nm,
          "Use No concerns, Some concerns, or Serious concerns.")
  }))
  numeric_cols <- intersect(setdiff(inspect_sr_columns(), c("study_id", "author", "year", items, "d4_3_pvalues")),
                            names(isp))
  issues <- c(issues, purrr::map(numeric_cols, \(nm) {
    values <- as.character(isp[[nm]])
    issue("inspect_sr", which(!is_blank(values) & is.na(suppressWarnings(as.numeric(values)))), nm,
          "Expected a numeric value.")
  }))
  purrr::list_rbind(issues)
}

trial_designs <- function() c("rct", "xo_rct", "c_rct", "cxo_rct")

validate_trial_design <- function(df, section) {
  if (!nrow(df) || !"trial_design" %in% names(df)) return(empty_issues())
  bad <- which(!is_blank(df$trial_design) & !df$trial_design %in% trial_designs())
  issue(section, bad, "trial_design",
        paste("Expected one of:", paste(trial_designs(), collapse = ", ")))
}

validate_numeric_columns <- function(df, section, columns) {
  cols <- setdiff(intersect(columns, names(df)), "trial_design")
  purrr::map(cols, \(nm) {
    raw <- df[[nm]]
    num <- suppressWarnings(as.numeric(raw))
    dplyr::bind_rows(
      issue(section, which(!is_blank(raw) & is.na(num)), nm, "Expected a numeric value."),
      if (grepl("^(n|event|sd|time)_", nm)) issue(section, which(num < 0), nm, "Must not be negative."),
      if (grepl("^rho_", nm)) issue(section, which(abs(num) > 1), nm, "Correlations must lie in [-1, 1]."),
      if (nm %in% c("n_clusters", "n_periods", "cluster_size")) issue(section, which(num <= 0), nm, "Must be positive.")
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(empty_issues())
}

validate_design_inputs <- function(resolved) {
  if (!"study_id" %in% names(resolved)) return(empty_issues())
  col <- \(nm) if (nm %in% names(resolved)) resolved[[nm]] else rep(NA, nrow(resolved))
  design <- normalise_trial_design(col("trial_design"))
  clusters <- !is_blank(col("n_clusters")) | !is_blank(col("cluster_size"))
  checks <- list(
    list(design = c("c_rct", "cxo_rct"), ok = clusters, column = "n_clusters"),
    list(design = c("c_rct", "cxo_rct"), ok = !is_blank(col("rho_w")), column = "rho_w"),
    list(design = "xo_rct", ok = !is_blank(col("rho_wp")), column = "rho_wp")
  )
  purrr::map(checks, \(chk) {
    issue("outcomes", which(design %in% chk$design & !chk$ok), chk$column,
          paste0(chk$column, " is missing here and in Study Details: a design effect of 1 is used."))
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(empty_issues())
}

effect_measures <- function() c("OR", "RR", "IRR", "MD", "SMD", "MedD")
