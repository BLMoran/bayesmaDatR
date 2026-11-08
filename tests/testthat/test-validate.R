test_that("clean data has no issues", {
  expect_equal(nrow(validate_ma_data(example_data())), 0)
})

test_that("outcome problems are reported", {
  dat <- example_data()
  dat$outcomes$trial_design[1] <- "rtc"
  dat$outcomes$event_int[2] <- 99
  dat$outcomes$rho_w <- c(NA, 1.5, NA)
  dat$outcomes$n_ctrl <- c("50", "forty", "31")
  found <- validate_ma_data(dat)
  expect_setequal(found$column, c("trial_design", "event_int", "rho_w", "n_ctrl"))
})

test_that("RoB levels, duplicates and outcome links are checked", {
  rob <- data.frame(study_id = c("S1", "S1", "S2", "S3"),
                    outcome_id = c(NA, NA, "pain", "nausea"),
                    rob_tool = "rob2", rob_level = c("study", "study", "outcome", "outcome"),
                    D1 = c("Low", "High", "Low", "Low"), D2 = "Low", D3 = "Low", D4 = "Low",
                    D5 = c("Low", "Low", "Bad", "Low"), Overall = "Low")
  dat <- example_data()
  dat$risk_of_bias <- rob
  found <- validate_ma_data(dat)
  expect_true(any(grepl("Conflicting", found$issue)))
  expect_true(any(found$column == "D5" & found$row == 3))
  expect_true(any(grepl("No matching", found$issue) & found$row == 4))
})
