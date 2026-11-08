#' Built-in risk-of-bias tools
#'
#' The tools supported by robbR: RoB 2 (`"rob2"`), ROBINS-I V2 (`"robins_i"`),
#' ROBINS-E (`"robins_e"`), QUADAS-2 (`"quadas2"`), CLARITY (`"clarity"`),
#' RoB NMA (`"rob_nma"`), PROBAST (`"probast"`) and PROBAST+AI (`"probast_ai"`).
#' @return Character vector of supported tool identifiers.
#' @export
rob_tools <- function() {
  c("rob2", "robins_i", "robins_e", "quadas2", "clarity", "rob_nma", "probast", "probast_ai")
}

#' Domain columns for a risk-of-bias tool
#'
#' @param rob_tool Tool identifier from [rob_tools()].
#' @param include_overall Include the tool's overall judgement column(s).
#' @param include_optional Include design-specific optional domains (RoB 2
#'   `D1b`/`DS`, CLARITY `D11`/`D12`, RoB NMA `R1`-`R3`, PROBAST `A1`-`A3`).
#' @return Character vector of column names.
#' @export
rob_domains <- function(rob_tool = "rob2", include_overall = TRUE, include_optional = TRUE) {
  spec <- rob_spec(rob_tool)
  domains <- if (include_optional) spec$domains else setdiff(spec$domains, spec$optional)
  c(domains, if (include_overall) spec$overall)
}

#' Accepted judgements for a risk-of-bias tool
#'
#' Judgements are matched case-insensitively against the standard levels and
#' their common synonyms (e.g. `"low risk"` for `"Low"`), as in robbR.
#' @inheritParams rob_domains
#' @return Character vector of standard judgement levels.
#' @export
rob_judgements <- function(rob_tool = "rob2") {
  names(rob_spec(rob_tool)$levels)
}

rob_spec <- function(rob_tool, call = rlang::caller_env()) {
  rob_tool <- rlang::arg_match(rob_tool, rob_tools(), error_call = call)
  syn_low <- c("low", "low risk", "low risk of bias", "low concern", "low concerns")
  syn_some <- c("some concerns", "some concern", "concerns")
  syn_high <- c("high", "high risk", "high risk of bias", "high concern", "high concerns")
  syn_unclear <- c("unclear", "unclear risk", "unclear risk of bias", "no information", "ni")
  syn_low_except <- c("low (except confounding)", "low except confounding",
                      "low (except for concerns about uncontrolled confounding)",
                      "low risk of bias (except for concerns about uncontrolled confounding)")
  rob2 <- list(Low = syn_low, "Some concerns" = syn_some, High = syn_high)
  lu_h <- list(Low = syn_low, Unclear = syn_unclear, High = syn_high)
  probast <- list(domains = c("D1", "D2", "D3", "D4", "A1", "A2", "A3"),
                  optional = c("A1", "A2", "A3"), overall = c("Overall", "Applicability"),
                  levels = lu_h)
  switch(rob_tool,
    rob2 = list(domains = c("D1", "D1b", "D2", "D3", "D4", "D5", "DS"),
                optional = c("D1b", "DS"), overall = "Overall", levels = rob2),
    robins_i = list(
      domains = paste0("D", 1:6), optional = character(), overall = "Overall",
      levels = list(Low = syn_low, "Low (except confounding)" = syn_low_except,
                    Moderate = c("moderate", "moderate risk", "moderate risk of bias"),
                    Serious = c("serious", "serious risk", "serious risk of bias"),
                    Critical = c("critical", "critical risk", "critical risk of bias"))),
    robins_e = list(
      domains = paste0("D", 1:7), optional = character(), overall = "Overall",
      levels = list(Low = syn_low, "Low (except confounding)" = syn_low_except,
                    "Some concerns" = syn_some, High = syn_high,
                    "Very high" = c("very high", "very high risk", "very high risk of bias"))),
    quadas2 = list(domains = paste0("D", 1:4), optional = character(), overall = "Overall",
                   levels = lu_h),
    clarity = list(
      domains = paste0("D", 1:12), optional = c("D11", "D12"), overall = character(),
      levels = list("Definitely low" = c("low", "definitely low", "definitely low risk", "definitely yes"),
                    "Probably low" = c("probably low", "probably low risk", "probably yes"),
                    "Probably high" = c("probably high", "probably high risk", "probably no"),
                    "Definitely high" = c("high", "definitely high", "definitely high risk", "definitely no"))),
    rob_nma = list(
      domains = c("R1", "R2", "R3", "D1", "D2", "D3"), optional = c("R1", "R2", "R3"),
      overall = c("Results", "Conclusions"),
      levels = list(Low = c(syn_low, "no concerns", "no concern"), "Some concerns" = syn_some,
                    Unclear = syn_unclear, High = syn_high)),
    probast = probast,
    probast_ai = probast
  )
}

standardise_judgement <- function(x, rob_tool) {
  levels <- rob_spec(rob_tool)$levels
  lookup <- purrr::imap(levels, \(syn, std) c(tolower(std), syn)) |>
    purrr::imap(\(syn, std) stats::setNames(rep(std, length(syn)), syn)) |>
    unname() |>
    unlist()
  key <- tolower(trimws(as.character(x)))
  unname(lookup[key])
}

#' Add risk-of-bias assessments
#'
#' Appends assessments for one tool to the `risk_of_bias` section, recording the
#' tool and whether each assessment applies to the whole study or to one
#' outcome. Study-level ratings are later joined to every outcome from that
#' study; outcome-level ratings only to their own `outcome_id`.
#'
#' @param data An `ma_data` object.
#' @param rob A data frame with `study_id`, the tool's domain columns (see
#'   [rob_domains()]) and, for `rob_level = "outcome"`, `outcome_id`.
#' @param rob_tool Tool identifier from [rob_tools()].
#' @param ... Unused; must be empty.
#' @param rob_level `"study"` or `"outcome"`.
#' @return `data` with the assessments appended. Judgements are standardised
#'   to the tool's levels (see [rob_judgements()]).
#' @export
add_rob <- function(data, rob, rob_tool, ..., rob_level = c("study", "outcome")) {
  rlang::check_dots_empty()
  check_ma_data(data)
  rob_tool <- rlang::arg_match(rob_tool, rob_tools())
  rob_level <- rlang::arg_match(rob_level)
  rob <- standardise_section(rob)

  required <- c("study_id", if (rob_level == "outcome") "outcome_id",
                rob_domains(rob_tool, include_overall = FALSE, include_optional = FALSE))
  missing <- setdiff(required, names(rob))
  if (length(missing)) {
    cli::cli_abort("{.arg rob} for {.val {rob_tool}} is missing required column{?s} {.val {missing}}.")
  }
  if (rob_level == "study" && "outcome_id" %in% names(rob) && any(!is.na(rob$outcome_id))) {
    cli::cli_abort(c("Study-level assessments cannot carry an {.field outcome_id}.",
                     "i" = "Use {.code rob_level = \"outcome\"} for outcome-specific ratings."))
  }

  rated <- intersect(rob_domains(rob_tool), names(rob))
  standardised <- purrr::map(rob[rated], \(x) standardise_judgement(x, rob_tool))
  unknown <- purrr::map2(rob[rated], standardised, \(raw, std) raw[!is.na(raw) & is.na(std)]) |>
    unlist(use.names = FALSE) |>
    unique()
  if (length(unknown)) {
    cli::cli_abort(c("Unrecognised {.val {rob_tool}} judgement{?s}: {.val {unknown}}.",
                     "i" = "Use one of {.val {rob_judgements(rob_tool)}}."))
  }
  rob[rated] <- standardised
  rob$rob_tool <- rob_tool
  rob$rob_level <- rob_level
  if (!"outcome_id" %in% names(rob)) rob$outcome_id <- NA_character_

  keys <- c("study_id", "outcome_id", "rob_tool", "rob_level")
  rob <- rob[c(keys, setdiff(names(rob), keys))]
  data$risk_of_bias <- dplyr::bind_rows(data$risk_of_bias, rob)
  data
}

join_rob <- function(outcomes, rob, call = rlang::caller_env()) {
  if (!nrow(rob) || !"rob_level" %in% names(rob)) return(outcomes)
  rating_cols <- setdiff(names(rob), c("study_id", "outcome_id", "rob_level"))
  prefixed <- rob |>
    dplyr::rename_with(\(x) paste0("rob_", x), dplyr::all_of(setdiff(rating_cols, "rob_tool")))
  rob_cols <- setdiff(names(prefixed), c("study_id", "outcome_id", "rob_level"))

  study_rob <- prefixed |>
    dplyr::filter(.data$rob_level == "study") |>
    dplyr::select(dplyr::all_of(c("study_id", rob_cols)))
  outcome_rob <- prefixed |>
    dplyr::filter(.data$rob_level == "outcome") |>
    dplyr::select(dplyr::all_of(c("study_id", "outcome_id", rob_cols)))

  joined <- tryCatch(
    outcomes |>
      dplyr::left_join(outcome_rob, by = dplyr::join_by("study_id", "outcome_id"),
                       relationship = "many-to-one", na_matches = "never") |>
      dplyr::left_join(study_rob, by = dplyr::join_by("study_id"),
                       relationship = "many-to-one", na_matches = "never",
                       suffix = c("", ".study")),
    error = function(e) {
      cli::cli_abort(c("A study or outcome has more than one risk-of-bias assessment.",
                       "i" = "Run {.fn validate_ma_data} to find the duplicates."),
                     parent = e, call = call)
    }
  )
  for (nm in rob_cols) {
    study_nm <- paste0(nm, ".study")
    if (study_nm %in% names(joined)) {
      joined[[nm]] <- dplyr::coalesce(joined[[nm]], joined[[study_nm]])
      joined[[study_nm]] <- NULL
    }
  }
  joined
}

rob_tool_labels <- function() {
  c(rob2 = "RoB 2", robins_i = "ROBINS-I V2", robins_e = "ROBINS-E", quadas2 = "QUADAS-2",
    clarity = "CLARITY", rob_nma = "RoB NMA", probast = "PROBAST", probast_ai = "PROBAST+AI")
}

rob_domain_labels <- function(rob_tool) {
  probast <- function(participants, overall) {
    c(D1 = participants, D2 = "Predictors", D3 = "Outcome", D4 = "Analysis",
      A1 = paste0("Applicability: ", tolower(participants)),
      A2 = "Applicability: predictors", A3 = "Applicability: outcome",
      Overall = overall, Applicability = "Overall applicability")
  }
  overall <- c(Overall = "Overall risk of bias")
  switch(rob_tool,
    rob2 = c(
      D1 = "Bias arising from the randomization process",
      D1b = "Bias arising from the timing of identification or recruitment of participants (cluster-randomized trials)",
      D2 = "Bias due to deviations from intended interventions",
      D3 = "Bias due to missing outcome data",
      D4 = "Bias in measurement of the outcome",
      D5 = "Bias in selection of the reported result",
      DS = "Bias arising from period and carryover effects (crossover trials)", overall),
    robins_i = c(
      D1 = "Bias due to confounding",
      D2 = "Bias in classification of interventions",
      D3 = "Bias in selection of participants into the study (or into the analysis)",
      D4 = "Bias due to missing data",
      D5 = "Bias in measurement of the outcome",
      D6 = "Bias in selection of the reported result", overall),
    robins_e = c(
      D1 = "Bias due to confounding",
      D2 = "Bias arising from measurement of the exposure",
      D3 = "Bias in selection of participants into the study (or into the analysis)",
      D4 = "Bias due to post-exposure interventions",
      D5 = "Bias due to missing data",
      D6 = "Bias arising from measurement of the outcome",
      D7 = "Bias in selection of the reported result", overall),
    quadas2 = c(D1 = "Patient selection", D2 = "Index test(s)", D3 = "Reference standard",
                D4 = "Flow and timing", overall),
    clarity = c(
      D1 = "Allocation sequence adequately generated", D2 = "Allocation adequately concealed",
      D3 = "Patients blinded", D4 = "Healthcare providers blinded", D5 = "Data collectors blinded",
      D6 = "Outcome assessors blinded", D7 = "Data analysts blinded",
      D8 = "Missing outcome data infrequent", D9 = "No selective outcome reporting",
      D10 = "Free of other bias",
      D11 = "No selective enrolment within clusters (cluster trials)",
      D12 = "No changes between intervention and control periods (cluster cross-over trials)"),
    rob_nma = c(
      R1 = "ROBIS: Study eligibility criteria",
      R2 = "ROBIS: Identification and selection of studies",
      R3 = "ROBIS: Data collection and study appraisal",
      D1 = "Interventions and network geometry", D2 = "Effect modifiers",
      D3 = "Statistical synthesis",
      Results = "Overall risk of bias in the NMA results",
      Conclusions = "Concerns about the authors' conclusions"),
    probast = probast("Participants", "Overall risk of bias"),
    probast_ai = probast("Participants and data sources",
                         "Overall quality (development) / risk of bias (evaluation)")
  )
}
