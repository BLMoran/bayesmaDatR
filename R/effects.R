#' Estimate study-level effect sizes
#'
#' Calculates `yi` and `sei` with the same rules as bayesma's two-stage models:
#'
#' * `"OR"`, `"RR"`: `event_*` and `n_*`, with a continuity correction for zero
#'   (or, for RR, full) cells.
#' * `"IRR"`: `event_*` and person-time `time_*`.
#' * `"MD"`, `"SMD"` (Hedges' g): `mean_*`, `sd_*` and `n_*`.
#' * `"MedD"`: difference in medians by quantile estimation (McGrath et al.,
#'   2020) via metamedian, from whichever of minimum/median/maximum,
#'   quartiles, or mean/SD each arm reports.
#'
#' Ratios are on the log scale. Rows lacking the required summaries get `NA`;
#' for `"MedD"`, a warning names the studies that cannot be estimated.
#'
#' Clustered and crossover trials are handled with the design effect (see
#' [compute_design_effect()]): sample sizes, events and person-time are
#' divided by it before `yi` and `sei` are calculated. The reported `n_int` and
#' `n_ctrl` are kept, so forest plots can show them; the effective sizes are
#' added as `n_int_eff` and `n_ctrl_eff`. The design effect is always
#' calculated from the design inputs (missing correlations give 1); a
#' `design_effect` column already in `data` is replaced.
#'
#' @param data Outcome data frame in the canonical schema (see
#'   [template_ma_data()]).
#' @param measure Effect measure.
#' @param ... Unused; must be empty.
#' @param continuity Continuity correction for zero cells in binary and count
#'   outcomes.
#' @param design_effect `"apply"` (default) uses effective sample sizes;
#'   `"ignore"` uses the reported sizes for every study.
#' @return `data` with `design_effect`, `n_int_eff`, `n_ctrl_eff`, `yi` and
#'   `sei` columns added.
#' @export
compute_effect_size <- function(data, measure = c("OR", "RR", "IRR", "MD", "SMD", "MedD"),
                                ..., continuity = 0.5, design_effect = c("apply", "ignore")) {
  rlang::check_dots_empty()
  measure <- rlang::arg_match(measure)
  design_effect <- rlang::arg_match(design_effect)
  d <- as.data.frame(data)
  check_columns(d, required_columns(measure))
  deff <- if (design_effect == "apply") row_design_effect(d) else rep(1, nrow(d))
  eff <- effective_sizes(d, deff)
  col <- \(nm) as.numeric(eff[[nm]])

  es <- switch(measure,
    OR = {
      a <- col("event_int"); b <- col("n_int") - a
      c <- col("event_ctrl"); e <- col("n_ctrl") - c
      cc <- dplyr::if_else(a == 0 | b == 0 | c == 0 | e == 0, continuity, 0)
      list(yi = log(((a + cc) * (e + cc)) / ((b + cc) * (c + cc))),
           sei = sqrt(1 / (a + cc) + 1 / (b + cc) + 1 / (c + cc) + 1 / (e + cc)))
    },
    RR = {
      a <- col("event_int"); c <- col("event_ctrl"); nt <- col("n_int"); nc <- col("n_ctrl")
      cc <- dplyr::if_else(a == 0 | c == 0 | a == nt | c == nc, continuity, 0)
      list(yi = log(((a + cc) / (nt + 2 * cc)) / ((c + cc) / (nc + 2 * cc))),
           sei = sqrt(1 / (a + cc) - 1 / (nt + 2 * cc) + 1 / (c + cc) - 1 / (nc + 2 * cc)))
    },
    IRR = {
      et <- col("event_int"); ec <- col("event_ctrl")
      cc <- dplyr::if_else(et == 0 | ec == 0, continuity, 0)
      list(yi = log(((et + cc) / col("time_int")) / ((ec + cc) / col("time_ctrl"))),
           sei = sqrt(1 / (et + cc) + 1 / (ec + cc)))
    },
    MD = list(yi = col("mean_int") - col("mean_ctrl"),
              sei = sqrt(col("sd_int")^2 / col("n_int") + col("sd_ctrl")^2 / col("n_ctrl"))),
    SMD = {
      nt <- col("n_int"); nc <- col("n_ctrl")
      sp <- sqrt(((nc - 1) * col("sd_ctrl")^2 + (nt - 1) * col("sd_int")^2) / (nc + nt - 2))
      j <- 1 - 3 / (4 * (nc + nt) - 9)
      yi <- j * (col("mean_int") - col("mean_ctrl")) / sp
      list(yi = yi, sei = sqrt((nc + nt) / (nc * nt) + yi^2 / (2 * (nc + nt))))
    },
    MedD = median_difference_es(eff)
  )
  d$design_effect <- deff
  d$n_int_eff <- eff$n_int
  d$n_ctrl_eff <- eff$n_ctrl
  d$yi <- es$yi
  d$sei <- es$sei
  d
}

effective_sizes <- function(d, deff) {
  scaled <- intersect(c("n_int", "n_ctrl", "event_int", "event_ctrl", "time_int", "time_ctrl"), names(d))
  d[scaled] <- purrr::map(d[scaled], \(x) as.numeric(x) / deff)
  d
}

required_columns <- function(measure) {
  switch(measure,
    OR = , RR = c("event_int", "event_ctrl", "n_int", "n_ctrl"),
    IRR = c("event_int", "event_ctrl", "time_int", "time_ctrl"),
    MD = , SMD = c("mean_int", "mean_ctrl", "sd_int", "sd_ctrl", "n_int", "n_ctrl"),
    MedD = c("n_int", "n_ctrl")
  )
}

check_columns <- function(d, cols, call = rlang::caller_env()) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) cli::cli_abort("Missing required column{?s}: {.val {missing}}.", call = call)
  invisible(d)
}

median_summaries <- function(d, arm, group) {
  stats <- c(min = "min", q1 = "q1", med = "median", q3 = "q3", max = "max",
             n = "n", mean = "mean", sd = "sd")
  purrr::map(stats, \(s) {
    nm <- paste0(s, "_", arm)
    if (nm %in% names(d)) as.numeric(d[[nm]]) else rep(NA_real_, nrow(d))
  }) |>
    stats::setNames(paste0(names(stats), ".", group)) |>
    tibble::as_tibble()
}

median_difference_es <- function(d, call = rlang::caller_env()) {
  rlang::check_installed("metamedian", reason = "for `measure = \"MedD\"`.", call = call)
  summaries <- dplyr::bind_cols(median_summaries(d, "int", "g1"), median_summaries(d, "ctrl", "g2"))
  fits <- purrr::map(seq_len(nrow(summaries)), \(i) {
    tryCatch(
      suppressWarnings(metamedian::metamedian(as.data.frame(summaries[i, ]), median_method = "qe",
                                              pool_studies = FALSE)),
      error = \(e) list(yi = NA_real_, sei = NA_real_)
    )
  })
  es <- list(yi = purrr::map_dbl(fits, \(f) as.numeric(f$yi[1])),
             sei = purrr::map_dbl(fits, \(f) as.numeric(f$sei[1])))
  labels <- d$study_id %||% seq_len(nrow(d))
  failed <- labels[!is.finite(es$yi) | !is.finite(es$sei) | es$sei <= 0]
  if (length(failed)) {
    cli::cli_warn(c(
      "Could not estimate the difference in medians for {length(failed)} stud{?y/ies}: {.val {failed}}; their effect sizes are left empty.",
      "i" = "Each arm needs {.field n} and one of: minimum, median, maximum; first quartile, median, third quartile; all five; or mean and SD."
    ), call = call)
    bad <- !is.finite(es$yi) | !is.finite(es$sei) | es$sei <= 0
    es$yi[bad] <- NA_real_
    es$sei[bad] <- NA_real_
  }
  es
}

#' Estimate means and standard deviations from medians and quantiles
#'
#' Fills in `mean_<arm>` and `sd_<arm>` where they are missing, using
#' metamedian, from whichever of median, quartiles and range each study
#' reports. Reported means and SDs are never overwritten. The column
#' `mean_<arm>_method` records whether each value was `"reported"`, estimated
#' (the `method` used), or could not be estimated (`NA`).
#'
#' @param data Outcome data frame in the canonical schema.
#' @param arm Arm(s) to fill: `"int"`, `"ctrl"`, or both (the default).
#' @param ... Unused; must be empty.
#' @param method Estimation method, passed to [metamedian::metamean()] as the
#'   matching `mean_method` and `sd_method`:
#'   * `"wan"`: Hozo et al. (2005), Wan et al. (2014) and Bland (2015)
#'     (`"hozo/wan/bland"`), with Wan et al.'s SD.
#'   * `"luo"`: Luo et al. (2018), with the Wan/Shi SD (`"wan/shi_normal"`).
#'   * `"qe"`: quantile matching estimation (McGrath et al. 2020).
#'   * `"bc"`: Box-Cox (McGrath et al. 2020). It uses random numbers, so it
#'     runs with a fixed seed and leaves your random number stream unchanged.
#'   * `"shi"`: Shi et al. (2020b), log-normal (`"shi_lognormal"`).
#'   * `"yang"`: Yang et al. (2022), assuming normality.
#'
#'   SDs come from the method's own SD estimator.
#' @return `data` with the arms' mean, SD and method columns filled in.
#' @export
convert_median_to_mean <- function(data, arm = c("int", "ctrl"), ...,
                                   method = c("wan", "luo", "qe", "bc", "shi", "yang")) {
  rlang::check_dots_empty()
  rlang::check_installed("metamedian", reason = "to estimate means from medians and quantiles.")
  arm <- rlang::arg_match(arm, multiple = TRUE)
  method <- rlang::arg_match(method)
  purrr::reduce(arm, \(d, a) estimate_arm_mean(d, a, method), .init = as.data.frame(data))
}

estimate_arm_mean <- function(d, arm, method) {
  mean_col <- paste0("mean_", arm)
  sd_col <- paste0("sd_", arm)
  method_col <- paste0("mean_", arm, "_method")
  for (nm in c(mean_col, sd_col)) if (!nm %in% names(d)) d[[nm]] <- NA_real_
  d[[mean_col]] <- as.numeric(d[[mean_col]])
  d[[sd_col]] <- as.numeric(d[[sd_col]])

  reported <- !is.na(d[[mean_col]]) & !is.na(d[[sd_col]])
  summaries <- median_summaries(d, arm, "g1") |>
    dplyr::select(-"mean.g1", -"sd.g1")
  metamedian_method <- median_mean_methods()[[method]]
  rows <- which(!reported & !is.na(summaries$med.g1) & !is.na(summaries$n.g1))

  estimates <- purrr::map(rows, \(i) {
    fit <- tryCatch(
      with_fixed_seed(suppressWarnings(metamedian::metamean(
        as.data.frame(summaries[i, ]), mean_method = metamedian_method[["mean"]], se_method = "naive",
        sd_method = metamedian_method[["sd"]], pool_studies = FALSE
      ))),
      error = \(e) NULL
    )
    if (is.null(fit)) return(c(mean = NA_real_, sd = NA_real_))
    c(mean = as.numeric(fit$yi[1]), sd = as.numeric(fit$sei[1]) * sqrt(summaries$n.g1[i]))
  })
  d[[mean_col]][rows] <- purrr::map_dbl(estimates, "mean")
  d[[sd_col]][rows] <- purrr::map_dbl(estimates, "sd")

  previous <- if (method_col %in% names(d)) d[[method_col]] else rep(NA_character_, nrow(d))
  d[[method_col]] <- dplyr::case_when(
    reported & !is.na(previous) ~ previous,
    reported ~ "reported",
    !is.na(d[[mean_col]]) ~ method,
    .default = NA_character_
  )
  d
}

median_mean_methods <- function() {
  list(
    wan = c(mean = "hozo/wan/bland", sd = "wan"),
    luo = c(mean = "luo", sd = "wan/shi_normal"),
    qe = c(mean = "qe", sd = "qe"),
    bc = c(mean = "bc", sd = "bc"),
    shi = c(mean = "shi_lognormal", sd = "shi_lognormal"),
    yang = c(mean = "yang", sd = "yang")
  )
}

with_fixed_seed <- function(code, seed = 1L) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)
  code
}
