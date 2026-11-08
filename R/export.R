#' Export outcomes for bayesma
#'
#' Checks that the workbook holds what the chosen bayesma model needs and
#' returns data in bayesma's input structure.
#'
#' * `format = "wide"`: one row per study for `bayesma::bayesma()`, with the
#'   outcome's own `n_int`/`n_ctrl` and the arm summaries named as bayesma's
#'   defaults (`event_*`, `mean_*`, `sd_*`, `time_*`, `median_*`, ...). For
#'   `model_type = "two_stage"`, `yi` and `sei` are appended. Use
#'   `study_var = study_id` (or `author` with `year_var = year`) and, for
#'   `model_type = "bc_jung"`, `rob_var = rob_Overall`. The reported sample
#'   sizes are kept; a `design_effect` column (see [compute_design_effect()])
#'   is added for `bayesma(design_effect = design_effect)`, and two-stage
#'   `yi`/`sei` already use the effective sample sizes.
#' * `format = "long"`: one row per arm for `bayesma::bayesma_mv()` (several
#'   outcomes), `bayesma::bayesma_long()` (several time points) or multi-arm
#'   models, with columns `arm_id`, `events`, `n`, `mean`, `sd` and
#'   `exposure`. Use `arm_var = arm_id`.
#'
#' Studies with no summary data for an outcome (e.g. rows added by "Sync
#' studies" for studies that did not report it) are left out of that outcome,
#' with a message. Studies with partial data (e.g. a mean without an SD) are
#' kept with a warning naming the missing fields; `yi`/`sei` are left empty
#' where they cannot be calculated.
#'
#' @section Several outcomes:
#' With more than one outcome (or time point) in wide format, outcomes are
#' placed side by side: one row per study, study details once, and each
#' outcome's columns suffixed with its `outcome_id` (and time point, when an
#' outcome has several), e.g. `outcome_id_hosp_los`, `yi_hosp_los`,
#' `sei_icu_los`.
#' Studies that did not report an outcome have empty cells for it. Each
#' outcome uses its own `measure` unless `measure` is given. With a single
#' outcome, columns keep their plain names (`yi`, `sei`, ...).
#'
#' @inheritParams select_outcome
#' @param ... Unused; must be empty.
#' @param model_type `"two_stage"` or `"one_stage"`, as in bayesma's `stage`.
#' @param measure Effect measure for every outcome. `NULL` uses each outcome's
#'   `measure` column.
#' @param format `"wide"` or `"long"`; see Description.
#' @param estimate_means For `"MD"` and `"SMD"`: `"none"` (default) keeps
#'   studies as reported, so studies reporting only medians are flagged as
#'   incomplete; `"wan"` or `"qe"` first estimates missing means and SDs from
#'   medians, quartiles and ranges with [convert_median_to_mean()].
#' @param design_effect `"apply"` (default) or `"ignore"`; see
#'   [compute_effect_size()].
#' @return A data frame.
#' @export
as_bayesma <- function(data, outcome = NULL, ..., timepoint = NULL,
                       model_type = c("two_stage", "one_stage"), measure = NULL,
                       format = c("wide", "long"), estimate_means = c("none", "wan", "qe"),
                       design_effect = c("apply", "ignore")) {
  rlang::check_dots_empty()
  model_type <- rlang::arg_match(model_type)
  design_effect <- rlang::arg_match(design_effect)
  format <- rlang::arg_match(format)
  estimate_means <- rlang::arg_match(estimate_means)
  d <- select_outcome(data, outcome, timepoint) |>
    drop_unreported()
  groups <- split_outcome_groups(d)
  call <- rlang::current_env()

  parts <- purrr::map(groups, \(g) {
    m <- resolve_measure(g, measure)
    if (estimate_means != "none" && m %in% c("MD", "SMD")) g <- convert_median_to_mean(g, method = estimate_means)
    if (m == "MedD" && (model_type == "one_stage" || format == "long")) {
      cli::cli_abort("bayesma supports {.val MedD} only for two-stage models in wide format.")
    }
    if (m == "SMD" && format == "long") {
      cli::cli_abort("bayesma's arm-level models support {.val OR}, {.val RR}, {.val IRR} and {.val MD}.")
    }
    check_columns(g, c("study_id", required_columns(m)))
    check_complete(g, required_columns(m), m, label = attr(g, "label"), call = call)
    g$measure <- m
    g$design_effect <- if (design_effect == "apply") row_design_effect(g) else 1
    if (format == "wide") {
      if (anyDuplicated(g$study_id)) {
        cli::cli_abort(c(
          "Wide format needs one row per study and outcome, but {.val {unique(g$study_id[duplicated(g$study_id)])}} {?has/have} several for {.val {attr(g, 'label')}}.",
          "i" = "Select one {.arg timepoint}, or use {.code format = \"long\"} for longitudinal or multi-arm data."
        ))
      }
      if (model_type == "two_stage") g <- compute_effect_size(g, measure = m, design_effect = design_effect)
    }
    g
  })

  if (format == "long") {
    return(
      purrr::list_rbind(parts) |>
        prepare_arm_data(format = "long") |>
        dplyr::rename(dplyr::any_of(c(events = "event", exposure = "time")))
    )
  }
  arrange_export(parts, data, d)
}

stat_columns <- function(d) grep("^(event|mean|sd|median|q1|q3|min|max|time)_(int|ctrl)$", names(d), value = TRUE)

drop_unreported <- function(d) {
  stats <- stat_columns(d)
  if (!length(stats) || !nrow(d)) return(d)
  reported <- rowSums(!is.na(as.data.frame(purrr::map(d[stats], \(x) suppressWarnings(as.numeric(x)))))) > 0
  if (any(!reported)) {
    skipped <- tibble::tibble(outcome = d$outcome_id[!reported] %||% NA, study = d$study_id[!reported]) |>
      dplyr::summarise(studies = paste(.data$study, collapse = ", "), .by = "outcome")
    cli::cli_inform(c(
      "i" = "Left out {sum(!reported)} study row{?s} with no data reported:",
      stats::setNames(paste0(skipped$outcome, ": ", skipped$studies), rep("*", nrow(skipped)))
    ))
  }
  d[reported, , drop = FALSE]
}

split_outcome_groups <- function(d, by_arm = FALSE) {
  if (!nrow(d)) return(list(structure(d, label = NA_character_)))
  outcome <- if ("outcome_id" %in% names(d)) d$outcome_id else rep(NA_character_, nrow(d))
  timepoint <- if ("timepoint" %in% names(d)) d$timepoint else rep(NA_character_, nrow(d))
  key <- paste(outcome, timepoint, sep = "\r")
  several_times <- tapply(timepoint, dplyr::coalesce(outcome, "NA"), \(t) length(unique(t)) > 1)
  groups <- purrr::map(unique(key), \(k) {
    g <- d[key == k, , drop = FALSE]
    o <- dplyr::coalesce(g$outcome_id[1] %||% NA_character_, "NA")
    label <- normalise_column_name(if (o == "NA") "outcome" else o)
    if (isTRUE(several_times[[o]]) && !is.na(g$timepoint[1])) {
      label <- paste(label, normalise_column_name(g$timepoint[1]), sep = "_")
    }
    structure(g, label = label)
  })
  if (!by_arm) return(groups)
  purrr::list_flatten(purrr::map(groups, \(g) {
    if (!anyDuplicated(g$study_id) || !"int_arm" %in% names(g)) return(list(g))
    arm <- dplyr::coalesce(g$int_arm, "int")
    purrr::map(unique(arm), \(a) {
      structure(g[arm == a, , drop = FALSE],
                label = paste(attr(g, "label"), normalise_column_name(a), sep = "_"))
    })
  }))
}

outcome_stat_order <- function() {
  stats <- c("n", "event", "mean", "sd", "median", "q1", "q3", "min", "max", "time")
  as.vector(t(outer(stats, c("int", "ctrl"), paste, sep = "_")))
}

study_block <- function(data, d, include_n) {
  studies <- if (inherits(data, "ma_data") && nrow(data$studies)) data$studies else d
  studies <- studies[!duplicated(studies$study_id), , drop = FALSE]
  known <- names(study_template_columns())
  extras <- if (inherits(data, "ma_data")) setdiff(names(data$studies), known) else character()
  cols <- intersect(c(known, extras), names(studies))
  if (!include_n) cols <- setdiff(cols, c("n_int", "n_ctrl"))
  block <- order_study_columns(studies[cols])
  if ("trial_design" %in% names(d) && "trial_design" %in% names(block)) {
    from_outcomes <- d$trial_design[match(block$study_id, d$study_id)]
    block$trial_design <- dplyr::coalesce(block$trial_design, from_outcomes)
  }
  block
}

outcome_block <- function(g, measure) {
  always <- c("n_int", "n_ctrl", required_columns(measure))
  stats <- intersect(outcome_stat_order(), names(g))
  filled <- purrr::map_lgl(g[stats], \(x) any(!is.na(x)))
  stats <- stats[filled | stats %in% always]
  cols <- intersect(c("outcome_id", "measure", stats, "design_effect", "yi", "sei"), names(g))
  g[c("study_id", cols)]
}

arrange_export <- function(parts, data, d) {
  rob_cols <- grep("^rob_", names(d), value = TRUE)
  rob_by_outcome <- inherits(data, "ma_data") && any(data$risk_of_bias$rob_level %in% "outcome")
  single <- length(parts) == 1
  base <- study_block(data, purrr::list_rbind(parts), include_n = !single)
  base <- base[base$study_id %in% unique(unlist(purrr::map(parts, "study_id"))), , drop = FALSE]
  base <- base[order(match(base$study_id, unique(unlist(purrr::map(parts, "study_id"))))), , drop = FALSE]

  out <- purrr::reduce(parts, \(acc, g) {
    block <- outcome_block(g, g$measure[1])
    if (rob_by_outcome) block <- cbind(block, g[intersect(rob_cols, names(g))])
    if (!single) names(block)[-1] <- paste(names(block)[-1], attr(g, "label"), sep = "_")
    dplyr::left_join(acc, block, by = dplyr::join_by("study_id"), relationship = "one-to-one")
  }, .init = base)

  if (!rob_by_outcome && length(rob_cols)) {
    rob <- purrr::list_rbind(purrr::map(parts, \(g) g[c("study_id", intersect(rob_cols, names(g)))]))
    rob <- rob[!duplicated(rob$study_id), , drop = FALSE]
    out <- dplyr::left_join(out, rob, by = dplyr::join_by("study_id"))
  }
  rob_last <- grepl("^rob_", names(out))
  out <- out[c(which(!rob_last), which(rob_last))]
  rownames(out) <- NULL
  as.data.frame(out)
}

check_complete <- function(d, cols, measure, label = NULL, call = rlang::caller_env()) {
  if (measure == "MedD") return(invisible(d))
  incomplete <- !stats::complete.cases(d[cols])
  if (any(incomplete)) {
    missing <- purrr::map_chr(which(incomplete), \(i) {
      paste0(d$study_id[i], ": ", paste(cols[is.na(unlist(d[i, cols]))], collapse = ", "))
    })
    shown <- utils::head(missing, 10)
    where <- if (!is.null(label) && !is.na(label)) paste0(" (", label, ")") else ""
    cli::cli_warn(c(
      "{sum(incomplete)} stud{?y/ies} lack{?s/} some of the data for {.val {measure}}{where}; their effect sizes are left empty where they cannot be calculated:",
      stats::setNames(shown, rep("*", length(shown))),
      if (length(missing) > 10) c("*" = "... and {length(missing) - 10} more."),
      "i" = if (measure %in% c("MD", "SMD")) {
        "For studies reporting medians, set {.arg estimate_means} to {.val wan} or {.val qe} (needs n, median and quartiles or range), or use {.code measure = \"MedD\"}."
      }
    ), call = call)
  }
  invisible(d)
}
