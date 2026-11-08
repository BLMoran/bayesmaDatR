test_that("prepare_arm_data returns one row per arm", {
  long <- prepare_arm_data(example_data(), "pain")
  expect_equal(nrow(long), 6)
  expect_true(all(c("study_id", "outcome_id", "timepoint", "arm_id", "arm", "n",
                    "trial_design", "rho_w") %in% names(long)))
  expect_equal(long$n[long$study_id == "S2"], c(42, 40))
})

test_that("shared control arms in multi-arm studies appear once", {
  d <- example_outcomes()[c(1, 1), ]
  d$int_arm <- c("low_dose", "high_dose")
  d$ctrl_arm <- "placebo"
  d$mean_int <- c(3, 2.5)
  long <- prepare_arm_data(d)
  expect_equal(sort(long$arm_id), c("high_dose", "low_dose", "placebo"))
  d$n_ctrl[2] <- 99
  expect_error(prepare_arm_data(d), "different values")
})

test_that("as_bayesma checks completeness and adds yi/sei", {
  dat <- example_data()
  expect_warning(as_bayesma(dat, "pain"), "S3")
  out <- as_bayesma(dat, "pain", measure = "OR")
  expect_true(all(c("study_id", "author", "year", "yi", "sei") %in% names(out)))
  one <- as_bayesma(dat, "pain", measure = "OR", model_type = "one_stage")
  expect_false("yi" %in% names(one))
  expect_error(as_bayesma(dat, "pain", model_type = "one_stage", measure = "MedD"), "two-stage")
})

test_that("as_bayesma long format uses bayesma arm names", {
  long <- as_bayesma(example_data(), "pain", measure = "IRR", format = "long")
  expect_true(all(c("arm_id", "events", "exposure", "n") %in% names(long)))
})

test_that("several outcomes or time points go side by side", {
  dat <- example_data()
  dat$outcomes <- rbind(dat$outcomes, transform(dat$outcomes, timepoint = "48h"))
  both <- as_bayesma(dat, "pain", measure = "OR")
  expect_equal(nrow(both), 3)
  expect_true(all(c("yi_pain_24h", "yi_pain_48h", "sei_pain_48h") %in% names(both)))
  expect_equal(nrow(as_bayesma(dat, "pain", timepoint = "48h", measure = "OR")), 3)

  dat <- example_data()
  icu <- transform(example_outcomes(), outcome_id = "icu", measure = "OR")
  icu[1, stat_columns(icu)] <- NA
  dat$outcomes <- rbind(dat$outcomes, icu)
  expect_message(out <- as_bayesma(dat, c("pain", "icu"), estimate_means = "wan"), "no data reported")
  expect_equal(out$study_id, c("S1", "S2", "S3"))
  expect_equal(out$author, c("A", "B", "C"))
  expect_true(is.na(out$yi_icu[1]))
  expect_equal(out$measure_pain, rep("MD", 3))
  expect_equal(out$measure_icu, c(NA, "OR", "OR"))
})

test_that("rows with no reported data are left out, partial rows are errors", {
  dat <- example_data()
  dat$outcomes[3, stat_columns(dat$outcomes)] <- NA
  expect_message(out <- as_bayesma(dat, "pain", measure = "MD"), "no data reported")
  expect_equal(out$study_id, c("S1", "S2"))
  dat$outcomes$sd_int[2] <- NA
  expect_warning(out <- suppressMessages(as_bayesma(dat, "pain", measure = "MD")), "S2: sd_int")
  expect_equal(out$yi[2], -0.5)
  expect_true(is.na(out$sei[2]))
})

test_that("exports follow the study, outcome, risk-of-bias column layout", {
  dat <- example_data()
  dat$studies$trial_design <- "rct"
  dat$studies$n_int <- c(50, 40, 30)
  dat$studies$n_ctrl <- c(50, 42, 31)
  dat$studies$subgroup1 <- c("a", "b", "a")
  dat$outcomes$trial_design <- NULL
  dat <- add_rob(dat, data.frame(study_id = c("S1", "S2", "S3"), D1 = "Low", D2 = "Low", D3 = "Low",
                                 D4 = "Low", D5 = "Low", Overall = "Low"), "rob2")
  one <- as_bayesma(dat, "pain", measure = "OR")
  expect_equal(names(one)[1:7], c("study_id", "author", "year", "trial_design", "subgroup1", "outcome_id", "measure"))
  rob_at <- match("rob_tool", names(one))
  expect_equal(names(one)[rob_at - 3:1], c("design_effect", "yi", "sei"))
  expect_true(all(grepl("^rob_", utils::tail(names(one), 7))))

  dat$outcomes <- rbind(dat$outcomes, transform(dat$outcomes, outcome_id = "icu"))
  two <- as_bayesma(dat, measure = "OR")
  expect_equal(names(two)[1:7], c("study_id", "author", "year", "trial_design", "n_int", "n_ctrl", "subgroup1"))
  expect_true(match("yi_icu", names(two)) > match("sei_pain", names(two)))
  expect_true(all(grepl("^rob_", utils::tail(names(two), 7))))
})

test_that("study columns are written in the standard order", {
  dat <- example_data()
  dat$studies$subgroup2 <- "x"
  dat$studies$rho_wp <- NA
  dat$studies$n_ctrl <- c(50, 42, 31)
  dat$studies$trial_design <- "rct"
  ordered <- order_study_columns(dat$studies)
  expect_equal(names(ordered), c("study_id", "author", "year", "trial_design", "n_ctrl", "subgroup2"))
})
