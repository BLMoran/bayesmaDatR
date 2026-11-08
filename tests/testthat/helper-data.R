example_outcomes <- function() {
  data.frame(
    study_id = c("S1", "S2", "S3"), outcome_id = "pain", outcome_name = "Pain",
    timepoint = "24h", trial_design = "rct", measure = "MD",
    n_int = c(50, 40, 30), n_ctrl = c(50, 42, 31),
    event_int = c(10, 0, 5), event_ctrl = c(15, 3, 8),
    mean_int = c(3, 4, NA), sd_int = c(1, 1.2, NA),
    mean_ctrl = c(4, 4.5, NA), sd_ctrl = c(1.1, 1.3, NA),
    median_int = c(NA, NA, 3), q1_int = c(NA, NA, 2), q3_int = c(NA, NA, 5),
    median_ctrl = c(NA, NA, 4), q1_ctrl = c(NA, NA, 3), q3_ctrl = c(NA, NA, 6),
    time_int = c(100, 120, 90), time_ctrl = c(100, 110, 95)
  )
}

example_data <- function() {
  new_ma_data(
    studies = data.frame(study_id = c("S1", "S2", "S3"), author = c("A", "B", "C"),
                         year = c(2020, 2021, 2022)),
    outcomes = example_outcomes()
  )
}
