#' Export one outcome for MetaStan
#'
#' Builds MetaStan's arm-based input: one row per study with the number of
#' arms `nd` and numbered arm columns, arm 1 being the control. Multi-arm
#' studies (several rows sharing a control arm, see [prepare_arm_data()]) get
#' one column set per arm.
#'
#' | `measure` | MetaStan `likelihood` | Arm columns |
#' |---|---|---|
#' | `"OR"`, `"RR"` | `"binomial"` | `r1`, `n1`, ... (events, sample size) |
#' | `"MD"` | `"normal"` | `M1`, `SE1`, ... (mean, standard error) |
#' | `"IRR"` | `"poisson"` | `count1`, `exposure1`, ... |
#'
#' Arm sizes, events and person-time are divided by the design effect (see
#' [compute_design_effect()]); counts are then rounded, as the binomial and
#' Poisson likelihoods need whole numbers. The reported sample sizes are kept
#' as `n_reported1`, `n_reported2`, ... for forest plots.
#'
#' @inheritParams as_bayesma
#' @param output `"metastan"` (default) returns the result of
#'   `MetaStan::create_MetaStan_dat()`, ready for `MetaStan::meta_stan()`;
#'   `"wide"` returns the one-row-per-study data frame it is built from.
#' @return For `"metastan"`, MetaStan's list (`data_long`, `data_wide`); for
#'   `"wide"`, a data frame. Both carry the MetaStan likelihood in
#'   `attr(x, "likelihood")`.
#' @examples
#' \dontrun{
#' metastan_dat <- as_MetaStan(dat, outcome = "pain_24h", measure = "MD")
#' MetaStan::meta_stan(metastan_dat, likelihood = attr(metastan_dat, "likelihood"),
#'                     theta_prior = c(0, 1), tau_prior = 0.5)
#' }
#' @export
as_MetaStan <- function(data, outcome = NULL, ..., timepoint = NULL, measure = NULL,
                        estimate_means = c("none", "wan", "luo", "qe", "bc", "shi", "yang"),
                        design_effect = c("apply", "ignore"),
                        output = c("metastan", "wide")) {
  rlang::check_dots_empty()
  estimate_means <- rlang::arg_match(estimate_means)
  design_effect <- rlang::arg_match(design_effect)
  output <- rlang::arg_match(output)
  d <- select_outcome(data, outcome, timepoint) |>
    drop_unreported()
  measure <- resolve_measure(d, measure)
  likelihood <- metastan_likelihood(measure)
  if (estimate_means != "none" && measure == "MD") d <- convert_median_to_mean(d, method = estimate_means)
  check_columns(d, c("study_id", required_columns(measure)))
  check_complete(d, required_columns(measure), measure)
  d <- d[stats::complete.cases(d[required_columns(measure)]), , drop = FALSE]
  if (nrow(d) && anyDuplicated(d[intersect(c("study_id", "int_arm"), names(d))])) {
    cli::cli_abort(c(
      "MetaStan takes one outcome at one time point.",
      "i" = "Select one {.arg outcome} and {.arg timepoint}."
    ))
  }
  d$design_effect <- if (design_effect == "apply") row_design_effect(d) else 1

  arms <- prepare_arm_data(d, format = "long") |>
    dplyr::mutate(arm_number = dplyr::row_number(), nd = dplyr::n(), .by = "study_id")
  wide <- metastan_arm_values(arms, likelihood) |>
    dplyr::select("study_id", "nd", "arm_number", "arm_id", dplyr::starts_with("..")) |>
    tidyr::pivot_wider(names_from = "arm_number", values_from = c("arm_id", dplyr::starts_with("..")),
                       names_sep = "") |>
    dplyr::rename_with(\(x) sub("^\\.\\.", "", x))
  study_cols <- intersect(c("study_id", "author", "year", "design_effect"), names(d))
  wide <- d[!duplicated(d$study_id), study_cols, drop = FALSE] |>
    dplyr::left_join(wide, by = dplyr::join_by("study_id")) |>
    as.data.frame()

  if (output == "wide") return(structure(wide, likelihood = likelihood))
  rlang::check_installed("MetaStan", reason = "to build MetaStan's input.")
  out <- MetaStan::create_MetaStan_dat(wide, armVars = metastan_arm_vars(likelihood), nArmsVar = "nd")
  structure(out, likelihood = likelihood)
}

metastan_likelihood <- function(measure, call = rlang::caller_env()) {
  switch(measure,
    OR = , RR = "binomial",
    MD = "normal",
    IRR = "poisson",
    cli::cli_abort(c(
      "MetaStan's arm-based models cannot use {.val {measure}}.",
      "i" = "Use {.val OR}, {.val RR}, {.val MD} or {.val IRR}."
    ), call = call)
  )
}

metastan_arm_vars <- function(likelihood) {
  switch(likelihood,
    binomial = c(responders = "r", sampleSize = "n"),
    normal = c(mean = "M", std.err = "SE"),
    poisson = c(count = "count", exposure = "exposure")
  )
}

metastan_arm_values <- function(arms, likelihood) {
  n_eff <- arms$n / arms$design_effect
  values <- switch(likelihood,
    binomial = list(..r = round(arms$event / arms$design_effect), ..n = round(n_eff)),
    normal = list(..M = arms$mean, ..SE = arms$sd / sqrt(n_eff)),
    poisson = list(..count = round(arms$event / arms$design_effect),
                   ..exposure = arms$time / arms$design_effect)
  )
  rounded <- likelihood != "normal" && any(arms$design_effect != 1, na.rm = TRUE)
  if (rounded) {
    cli::cli_inform(c("i" = "Effective counts were rounded to whole numbers for MetaStan's {likelihood} likelihood."))
  }
  arms[names(values)] <- values
  arms$..n_reported <- arms$n
  arms
}
