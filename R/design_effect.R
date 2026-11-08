#' Design effect for clustered and crossover trials
#'
#' Computes each study's design effect from its trial design. Effect sizes
#' divide counts and sample sizes by it to give effective sample sizes; the
#' reported sizes are kept unchanged in the data.
#'
#' | `trial_design` | Design effect |
#' |---|---|
#' | `"rct"` | \eqn{1} |
#' | `"c_rct"` | \eqn{1 + (m - 1)\rho_w} |
#' | `"xo_rct"` | \eqn{(1 - \rho_{wp}) / 2} |
#' | `"cxo_rct"` | \eqn{1 + (m - 1)\rho_w - m\rho_b} |
#'
#' where \eqn{m} is the mean cluster size (per cluster-period for cluster
#' crossover trials). It is taken from `cluster_size` when given, and
#' otherwise derived as \eqn{m = n_{total} / (k \times p)}, where \eqn{k} is
#' `n_clusters` and \eqn{p} is `n_periods` (1 for cluster trials and 2 for
#' cluster crossover trials when `n_periods` is missing).
#'
#' Missing inputs give a design effect of 1:
#' a cluster crossover trial without `rho_b` uses \eqn{\rho_b = 0}, and any
#' design whose formula still cannot be evaluated (e.g. no `rho_w` or no
#' `cluster_size`) is treated as 1. bayesma's names (`"parallel"`,
#' `"cluster"`, `"crossover"`, `"cluster_crossover"`) are also accepted, and a
#' missing design is treated as `"rct"`.
#'
#' For crossover trials the formula assumes that `n_ctrl + n_int` is the number
#' of participants, so a trial of 40 participants is entered as 20 and 20.
#'
#' @param trial_design Trial design of each study.
#' @param ... Unused; must be empty.
#' @param n_clusters Number of clusters, \eqn{k} above.
#' @param n_periods Number of periods, \eqn{p} above.
#' @param n_total Total sample size, `n_int + n_ctrl`, used with `n_clusters`.
#' @param cluster_size Mean participants per cluster (per cluster-period for
#'   cluster crossover trials), `m` above. Overrides the value derived from
#'   `n_clusters`.
#' @param rho_w Within-period intracluster correlation.
#' @param rho_b Between-period intracluster correlation.
#' @param rho_wp Within-person correlation across periods.
#' @param labels Study labels used in messages.
#' @return A numeric vector the same length as `trial_design`.
#' @examples
#' compute_design_effect(c("rct", "c_rct", "xo_rct", "cxo_rct"),
#'                       n_clusters = c(NA, 10, NA, 10),
#'                       n_total = c(100, 200, 40, 400),
#'                       rho_w = c(NA, 0.05, NA, 0.05),
#'                       rho_wp = c(NA, NA, 0.6, NA))
#' @export
compute_design_effect <- function(trial_design, ..., n_clusters = NA_real_, n_periods = NA_real_,
                                  n_total = NA_real_,
                                  cluster_size = NA_real_, rho_w = NA_real_,
                                  rho_b = NA_real_, rho_wp = NA_real_, labels = NULL) {
  rlang::check_dots_empty()
  n <- length(trial_design)
  design <- dplyr::coalesce(normalise_trial_design(trial_design), "rct")
  unknown <- unique(trial_design[is.na(normalise_trial_design(trial_design)) & !is_blank(trial_design)])
  if (length(unknown)) {
    cli::cli_abort("Unrecognised {.arg trial_design}: {.val {unknown}}. Use {.or {.val {trial_designs()}}}.")
  }
  k <- rep_len(suppressWarnings(as.numeric(n_clusters)), n)
  n_total <- rep_len(suppressWarnings(as.numeric(n_total)), n)
  periods <- dplyr::coalesce(rep_len(suppressWarnings(as.numeric(n_periods)), n),
                             dplyr::if_else(design == "cxo_rct", 2, 1))
  m <- dplyr::coalesce(rep_len(suppressWarnings(as.numeric(cluster_size)), n), n_total / (k * periods))
  rho_w <- rep_len(suppressWarnings(as.numeric(rho_w)), n)
  rho_b <- rep_len(suppressWarnings(as.numeric(rho_b)), n)
  rho_wp <- rep_len(suppressWarnings(as.numeric(rho_wp)), n)
  labels <- labels %||% seq_len(n)

  deff <- dplyr::case_when(
    design == "c_rct" ~ 1 + (m - 1) * rho_w,
    design == "xo_rct" ~ (1 - rho_wp) / 2,
    design == "cxo_rct" ~ 1 + (m - 1) * rho_w - m * dplyr::coalesce(rho_b, 0),
    .default = 1
  ) |>
    dplyr::coalesce(1)
  if (any(deff <= 0)) {
    cli::cli_abort(c(
      "The design effect must be positive; check {.val {unique(labels[deff <= 0])}}.",
      "i" = "For cluster crossover trials {.field rho_b} is usually below {.field rho_w}; {.field rho_wp} must be below 1."
    ))
  }
  deff
}

#' Add the design effect column
#'
#' Calculates [compute_design_effect()] for each outcome row and stores it in
#' `design_effect`, replacing any earlier value. Each input (`trial_design`,
#' `n_int`, `n_ctrl`, `n_clusters`, `n_periods`, `rho_w`, `rho_b`, `rho_wp`) comes from the
#' outcome row, or from Study Details where the outcome's cell is empty (see
#' [fill_study_defaults()]). The column is for display: effect sizes and
#' exports always recalculate the design effect from these inputs.
#'
#' @param data An `ma_data` object.
#' @return `data` with the `design_effect` column updated.
#' @export
fill_design_effect <- function(data) {
  check_ma_data(data)
  if (!nrow(data$outcomes)) return(data)
  data$outcomes$design_effect <- design_effect_from_columns(fill_study_defaults(data$outcomes, data$studies))
  data
}

normalise_trial_design <- function(x) {
  key <- stringr::str_remove_all(tolower(trimws(as.character(x))), "[^a-z]")
  dplyr::recode_values(
    key,
    c("rct", "parallel", "parallelrct", "individual", "individuallyrandomised",
      "individuallyrandomized") ~ "rct",
    c("crct", "cluster", "clusterrct", "clusterrandomised", "clusterrandomized") ~ "c_rct",
    c("xorct", "xo", "xrct", "crossover", "crossoverrct") ~ "xo_rct",
    c("cxorct", "cxo", "cxrct", "clustercrossover", "clustercrossoverrct") ~ "cxo_rct",
    default = NA_character_
  )
}

row_design_effect <- function(d) design_effect_from_columns(d)

design_effect_from_columns <- function(d) {
  col <- \(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
  n_total <- suppressWarnings(as.numeric(col("n_int")) + as.numeric(col("n_ctrl")))
  compute_design_effect(col("trial_design"), n_clusters = col("n_clusters"), n_periods = col("n_periods"),
                        n_total = n_total,
                        cluster_size = col("cluster_size"), rho_w = col("rho_w"), rho_b = col("rho_b"),
                        rho_wp = col("rho_wp"), labels = d$study_id %||% seq_len(nrow(d)))
}
