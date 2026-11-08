rob_help_content <- function(rob_tool) {
  labels <- rob_domain_labels(rob_tool)
  spec <- rob_spec(rob_tool)
  ratings <- paste(rob_judgements(rob_tool), collapse = " / ")
  optional <- spec$optional
  overall <- spec$overall
  rows <- purrr::imap(labels, \(label, domain) {
    note <- dplyr::case_when(
      domain %in% optional ~ " (optional; include when relevant to the design)",
      domain %in% overall ~ " (overall judgement)",
      .default = ""
    )
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$strong(domain)),
      shiny::tags$td(paste0(label, note)),
      shiny::tags$td(shiny::tags$code(ratings))
    )
  })
  shiny::tagList(
    shiny::p(class = "text-muted small",
             rob_tool_labels()[[rob_tool]], " columns and judgements, as defined in robbR. ",
             "Judgements are matched case-insensitively, so common synonyms such as ",
             shiny::code("low risk"), " are accepted."),
    shiny::tags$table(
      class = "table table-sm table-bordered",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Column"), shiny::tags$th("Domain"), shiny::tags$th("Ratings")
      )),
      shiny::tags$tbody(unname(rows))
    ),
    if (!length(overall)) {
      shiny::p(class = "text-muted small mb-0", "This tool has no overall judgement column.")
    }
  )
}

inspect_help_content <- function() {
  irow <- function(item, desc, score0, score1, score2) {
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$strong(item)),
      shiny::tags$td(style = "font-size:0.85rem;", desc),
      shiny::tags$td(score0), shiny::tags$td(score1), shiny::tags$td(score2)
    )
  }
  isection <- function(label) {
    shiny::tags$tr(shiny::tags$td(shiny::tags$strong(label), colspan = 5,
                                  style = "background:#f0f4f8; font-size:0.85rem;"))
  }
  shiny::tagList(
    shiny::p(class = "text-muted small",
             "INSPECT-SR assesses study integrity across 4 domains. Rate each item ",
             shiny::code("No concerns"), ", ", shiny::code("Some concerns"), " or ",
             shiny::code("Serious concerns"), "."),
    shiny::div(
      style = "overflow-x:auto;",
      shiny::tags$table(
        class = "table table-sm table-bordered",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Item"), shiny::tags$th("Description"), shiny::tags$th("No concerns"),
          shiny::tags$th("Some concerns"), shiny::tags$th("Serious concerns")
        )),
        shiny::tags$tbody(
          isection("Domain 1: Registration"),
          irow("D1.1 Prospective registration", "Was the trial registered before participant enrolment began?",
               "Registered before enrolment", "Retrospectively registered", "Not registered / unclear"),
          irow("D1.2 Registration consistency", "Is the published report consistent with the trial registration?",
               "No discrepancies", "Minor discrepancies", "Major discrepancies / unclear"),
          irow("D1.3 SAP consistency", "Is the statistical analysis plan consistent with the registration?",
               "Consistent SAP", "Minor inconsistencies", "Major inconsistencies / no SAP"),
          isection("Domain 2: Protocol Conduct"),
          irow("D2.1 Ethical approval", "Was documented ethical approval obtained?",
               "Yes, with evidence", "Unclear", "No evidence of approval"),
          irow("D2.2 Primary registry", "Was the trial registered in a primary registry (e.g., ClinicalTrials.gov, ANZCTR)?",
               "Yes", "Secondary registry only", "Not registered"),
          irow("D2.3 Registered before enrolment", "Was registration completed before participant enrolment began?",
               "Yes", "After enrolment started", "After completion / unclear"),
          irow("D2.4 Protocol vs. conduct", "Is the conduct of the trial consistent with the registered protocol?",
               "Consistent", "Minor inconsistencies", "Major inconsistencies"),
          irow("D2.5 Other conduct concerns", "Are there other concerns about how the trial was conducted?",
               "No", "Some concerns", "Serious concerns"),
          isection("Domain 3: Reporting"),
          irow("D3.1 All outcomes reported", "Are all pre-specified outcomes reported in the publication?",
               "All reported", "Minor omissions", "Major omissions"),
          irow("D3.2 Selective reporting", "Is there evidence that outcome reporting was selective based on results?",
               "No evidence", "Some evidence", "Clear evidence"),
          isection("Domain 4: Data Integrity"),
          irow("D4.1 Flow consistency", "Is the participant flow (CONSORT diagram) consistent throughout the paper?",
               "Consistent", "Minor inconsistencies", "Major inconsistencies"),
          irow("D4.2 Analysis populations", "Are analysis populations (ITT, per-protocol) appropriately defined and used?",
               "Appropriately defined", "Some issues", "Not defined / inappropriate"),
          irow("D4.3 Carlisle test", "Are baseline p-values consistent with the expected uniform distribution? (Enter p-values for the automated test.)",
               "Consistent", "Borderline", "Inconsistent / suspicious"),
          irow("D4.4 Data errors", "Are there apparent errors in the reported data (e.g., impossible values, arithmetic errors)?",
               "No", "Possible errors", "Clear errors"),
          irow("D4.5 Data duplication", "Is there evidence that data from this study appeared in another publication?",
               "No evidence", "Possible", "Clear duplication"),
          irow("D4.6 N-consistency", "Are sample sizes consistent across tables, figures, and text? (Enter N values for the automated check.)",
               "Consistent", "Minor inconsistencies", "Major inconsistencies"),
          irow("D4.7 Per guidelines", "Were analyses conducted per relevant reporting guidelines (e.g., CONSORT)?",
               "Yes", "Mostly", "No / unclear"),
          irow("D4.8 GRIM test", "Are reported means consistent with the sample size and number of decimal places? (Enter mean, n, and decimals for the automated test.)",
               "GRIM-consistent", "Borderline", "GRIM-inconsistent"),
          irow("D4.9 p-value verification", "Are reported p-values consistent with the test statistics and degrees of freedom? (Enter statistic, df, and p-value for the automated check.)",
               "Consistent", "Borderline", "Inconsistent"),
          irow("D4.10 Retraction / correction", "Has the paper been retracted, or have corrections been issued?",
               "No", "Correction issued", "Retracted"),
          irow("D4.11 Other data concerns", "Are there other concerns about the integrity of the reported data?",
               "No", "Some concerns", "Serious concerns")
        )
      )
    ),
    shiny::p(class = "text-muted small mb-0", shiny::HTML(paste0(
      "Reference: Riddle et al. (2025). INSPECT-SR: an instrument for assessing the credibility of ",
      "randomised trial reports. <em>medRxiv</em>. ",
      "<a href=\"https://doi.org/10.1101/2025.09.03.25334905\" target=\"_blank\">doi:10.1101/2025.09.03.25334905</a>"
    )))
  )
}
