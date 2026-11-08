#' INSPECT-SR workbook columns
#'
#' @param include_calculations Include the auxiliary fields used for the
#'   Carlisle, N-consistency, GRIM, and p-value checks.
#' @return Character vector of recommended column names.
#' @export
inspect_sr_columns <- function(include_calculations = TRUE) {
  calc <- c("d4_3_pvalues", "d4_6_n_control", "d4_6_n_intervention", "d4_6_n_total",
            "d4_8_mean", "d4_8_n", "d4_8_decimals", "d4_9_statistic", "d4_9_df", "d4_9_pvalue")
  c("study_id", "author", "year", inspect_sr_items(), if (include_calculations) calc)
}

inspect_sr_items <- function() {
  c(paste0("d1_", 1:3), paste0("d2_", 1:5), paste0("d3_", 1:2), paste0("d4_", 1:11))
}

#' INSPECT-SR field labels
#' @return Named character vector mapping field names to descriptive labels.
#' @export
inspect_sr_labels <- function() {
  c(d1_1 = "D1.1 Registration prospective", d1_2 = "D1.2 Registration consistent",
    d1_3 = "D1.3 SAP consistent", d2_1 = "D2.1 Ethical approval",
    d2_2 = "D2.2 Primary registry", d2_3 = "D2.3 Registered before enrolment",
    d2_4 = "D2.4 Protocol vs conduct", d2_5 = "D2.5 Other conduct concerns",
    d3_1 = "D3.1 All outcomes reported", d3_2 = "D3.2 Selective reporting",
    d4_1 = "D4.1 Flow consistent", d4_2 = "D4.2 Analysis populations",
    d4_3 = "D4.3 Carlisle test", d4_4 = "D4.4 Data errors",
    d4_5 = "D4.5 Data duplication", d4_6 = "D4.6 N-consistency",
    d4_7 = "D4.7 Per guidelines", d4_8 = "D4.8 GRIM test",
    d4_9 = "D4.9 p-value verify", d4_10 = "D4.10 Retraction",
    d4_11 = "D4.11 Other concerns", d4_3_pvalues = "D4.3: Baseline p-values (;-separated)",
    d4_6_n_control = "D4.6: N (Control)", d4_6_n_intervention = "D4.6: N (Intervention)",
    d4_6_n_total = "D4.6: N (Total)", d4_8_mean = "D4.8: Reported mean",
    d4_8_n = "D4.8: Sample size (n)", d4_8_decimals = "D4.8: Decimal places",
    d4_9_statistic = "D4.9: Test statistic", d4_9_df = "D4.9: Degrees of freedom",
    d4_9_pvalue = "D4.9: Reported p-value")
}
