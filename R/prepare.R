#' Select outcome rows
#'
#' @param data An `ma_data` object or outcome data frame.
#' @param outcome Outcome identifier(s), matched against `outcome_id` and then
#'   `outcome_name`. `NULL` keeps every outcome.
#' @param timepoint Time point(s) to keep. `NULL` keeps every time point.
#' @return The matching outcome rows, with study-level fields from `studies`
#'   and risk-of-bias ratings (prefixed `rob_`) joined on when `data` is an
#'   `ma_data` object.
#' @export
select_outcome <- function(data, outcome = NULL, timepoint = NULL) {
  d <- if (inherits(data, "ma_data")) data$outcomes else standardise_section(data)
  if (!is.null(outcome)) {
    ids <- if ("outcome_id" %in% names(d)) d$outcome_id else NA_character_
    labels <- if ("outcome_name" %in% names(d)) d$outcome_name else NA_character_
    keep <- ids %in% outcome | labels %in% outcome
    if (!any(keep)) cli::cli_abort("No rows found for outcome {.val {outcome}}.")
    d <- d[keep, , drop = FALSE]
  }
  if (!is.null(timepoint)) {
    check_columns(d, "timepoint")
    d <- d[d$timepoint %in% timepoint, , drop = FALSE]
    if (!nrow(d)) cli::cli_abort("No rows found for time point {.val {timepoint}}.")
  }
  if (inherits(data, "ma_data")) {
    d <- join_studies(d, data$studies)
    d <- join_rob(d, data$risk_of_bias)
  }
  d
}

join_studies <- function(outcomes, studies) {
  outcomes <- fill_study_defaults(outcomes, studies)
  extra <- setdiff(names(studies), names(outcomes))
  if (!nrow(studies) || !"study_id" %in% names(studies) || !length(extra)) return(outcomes)
  studies <- studies[!duplicated(studies$study_id), c("study_id", extra), drop = FALSE]
  joined <- dplyr::left_join(outcomes, studies, by = dplyr::join_by("study_id"), na_matches = "never")
  joined[c("study_id", extra, setdiff(names(outcomes), "study_id"))]
}

arm_stat_pattern <- function() "^(n|event|mean|sd|median|q1|q3|min|max|time)_(int|ctrl)$"

#' Prepare arm-level data
#'
#' Arm-level data for one-stage, multivariate, multi-arm and longitudinal
#' models. Pairwise rows are reshaped to one row per study, outcome, time point
#' and arm, keeping each outcome's own sample sizes, the trial design and its
#' correlation fields. For multi-arm studies, enter one row per comparison and
#' name the arms in `int_arm` and `ctrl_arm`; a control arm shared by several
#' comparisons appears once.
#'
#' @inheritParams select_outcome
#' @param ... Unused; must be empty.
#' @param format `"long"` (one row per arm) or `"wide"` (one row per
#'   comparison, as entered).
#' @return A data frame. In long format, the columns are the identifiers
#'   (`study_id`, `outcome_id`, `timepoint`, `arm_id`), `arm` (`"ctrl"` or
#'   `"int"`), the arm summaries (`n`, `event`, `mean`, `sd`, `median`, `q1`,
#'   `q3`, `min`, `max`, `time`) and the remaining outcome-level columns.
#' @export
prepare_arm_data <- function(data, outcome = NULL, ..., timepoint = NULL,
                             format = c("long", "wide")) {
  rlang::check_dots_empty()
  format <- rlang::arg_match(format)
  d <- select_outcome(data, outcome, timepoint)
  check_columns(d, c("study_id", "outcome_id", "n_int", "n_ctrl"))
  for (nm in c("timepoint", "trial_design", "int_arm", "ctrl_arm")) {
    if (!nm %in% names(d)) d[[nm]] <- NA_character_
  }
  for (nm in c("n_clusters", "rho_w", "rho_b", "rho_wp")) if (!nm %in% names(d)) d[[nm]] <- NA_real_
  d$int_arm <- dplyr::coalesce(d$int_arm, "int")
  d$ctrl_arm <- dplyr::coalesce(d$ctrl_arm, "ctrl")
  if (format == "wide") return(d)

  value_cols <- grep(arm_stat_pattern(), names(d), value = TRUE)
  d[value_cols] <- purrr::map(d[value_cols], as.numeric)
  long <- d |>
    tidyr::pivot_longer(dplyr::all_of(value_cols),
                        names_to = c(".value", "arm"), names_pattern = arm_stat_pattern()) |>
    dplyr::mutate(arm_id = dplyr::if_else(.data$arm == "int", .data$int_arm, .data$ctrl_arm)) |>
    dplyr::select(-"int_arm", -"ctrl_arm")

  keys <- c("study_id", "outcome_id", "timepoint", "arm_id")
  stat_cols <- intersect(c("n", "event", "mean", "sd", "median", "q1", "q3", "min", "max", "time"),
                         names(long))
  long <- dplyr::distinct(long, dplyr::across(dplyr::all_of(c(keys, "arm", stat_cols))), .keep_all = TRUE)
  conflicts <- long[duplicated(long[keys]), keys, drop = FALSE]
  if (nrow(conflicts)) {
    cli::cli_abort(c(
      "Arm {.val {unique(conflicts$arm_id)}} is entered with different values in study {.val {unique(conflicts$study_id)}}.",
      "i" = "A control arm shared across comparisons must have identical summaries in each row."
    ))
  }
  long$arm <- factor(long$arm, levels = c("ctrl", "int"))
  long |>
    dplyr::relocate(dplyr::all_of(c(keys, "arm", stat_cols))) |>
    dplyr::arrange(.data$study_id, .data$outcome_id, .data$timepoint, .data$arm) |>
    as.data.frame()
}

#' Prepare a single outcome for analysis
#'
#' Package-neutral preparation: arm-level data for one-stage models, or `yi`
#' and `sei` for two-stage models.
#'
#' @inheritParams select_outcome
#' @param ... Unused; must be empty.
#' @param model_type `"two_stage"` (effect sizes) or `"one_stage"` (arm-level
#'   data).
#' @param measure Effect measure for two-stage preparation. `NULL` uses the
#'   outcome's `measure` column.
#' @return A data frame.
#' @export
prepare_outcome <- function(data, outcome, ..., timepoint = NULL,
                            model_type = c("two_stage", "one_stage"), measure = NULL) {
  rlang::check_dots_empty()
  model_type <- rlang::arg_match(model_type)
  if (model_type == "one_stage") return(prepare_arm_data(data, outcome, timepoint = timepoint))
  d <- select_outcome(data, outcome, timepoint)
  compute_effect_size(d, measure = resolve_measure(d, measure))
}

resolve_measure <- function(d, measure, call = rlang::caller_env()) {
  if (!is.null(measure)) return(rlang::arg_match(measure, effect_measures(), error_call = call))
  recorded <- unique(stats::na.omit(d$measure))
  if (length(recorded) != 1) {
    cli::cli_abort(c("Supply {.arg measure}.",
                     "i" = "The outcome's {.field measure} column has {length(recorded)} distinct value{?s}."),
                   call = call)
  }
  rlang::arg_match(recorded, effect_measures(), error_arg = "measure", error_call = call)
}
