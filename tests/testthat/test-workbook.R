test_that("template has the schema sections and columns", {
  tm <- template_ma_data()
  expect_s3_class(tm, "ma_data")
  expect_named(tm, c("studies", "outcomes", "risk_of_bias", "inspect_sr"))
  expect_true(all(c("n_int", "n_ctrl", "median_int", "q1_ctrl", "n_clusters",
                    "rho_w", "rho_b", "rho_wp", "int_arm", "ctrl_arm") %in% names(tm$outcomes)))
  expect_true(all(c("trial_design", "n_int", "n_ctrl", "n_clusters", "rho_w", "rho_b", "rho_wp") %in%
                    names(tm$studies)))
  expect_false("trial_design" %in% names(tm$outcomes))
  expect_true(all(rob_domains("rob2") %in% names(tm$risk_of_bias)))
  expect_true(all(rob_domains("robins_i") %in%
                    names(template_ma_data(rob_tool = c("rob2", "robins_i"))$risk_of_bias)))
})

test_that("workbooks round-trip", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  dat <- example_data()
  write_ma_workbook(dat, path)
  back <- read_ma_workbook(path)
  expect_equal(back$outcomes$n_int, dat$outcomes$n_int)
  expect_equal(names(back$inspect_sr), inspect_sr_columns())
  expect_type(back$studies$study_id, "character")
})

test_that("legacy sheet names are recognised", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Study_Details = data.frame(study_id = "S1"),
                           Outcome_Data = data.frame(study_id = "S1", outcome_id = "o")), path)
  back <- read_ma_workbook(path)
  expect_equal(back$studies$study_id, "S1")
  expect_equal(back$outcomes$outcome_id, "o")
})

test_that("map_ma_columns and sync_ma_data", {
  dat <- new_ma_data(outcomes = data.frame(study_id = c("S1", "S2"), outcome_id = "o",
                                           N_treat = 10, n_ctrl = 10))
  dat <- map_ma_columns(dat, c(n_int = "N_treat"))
  expect_true("n_int" %in% names(dat$outcomes))
  synced <- sync_ma_data(dat)
  expect_equal(synced$studies$study_id, c("S1", "S2"))
  expect_true("rho_w" %in% names(synced$outcomes))
})


test_that("workbooks can be written as one merged sheet", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  dat <- example_data()
  dat$studies <- rbind(dat$studies, data.frame(study_id = "S4", author = "D", year = 2023))
  write_ma_workbook(dat, path, layout = "single")
  expect_equal(readxl::excel_sheets(path), "data")
  merged <- merge_ma_data(dat)
  expect_equal(nrow(merged), 4)
  expect_equal(merged$author, c("A", "B", "C", "D"))
  expect_true(is.na(merged$outcome_id_pain[4]))
})

test_that("workbooks can be saved as .rda", {
  dat <- example_data()
  path <- withr::local_tempfile(fileext = ".rda")
  write_ma_workbook(dat, path)
  env <- new.env()
  expect_setequal(load(path, envir = env), c("studies", "outcomes", "risk_of_bias", "inspect_sr"))
  expect_equal(env$outcomes$n_int, dat$outcomes$n_int)
  sheets <- read_upload_sheets(path, "workbook.rda")
  expect_setequal(names(recognised_sheets(names(sheets))), c("study", "outcome"))

  single <- withr::local_tempfile(fileext = ".rda")
  write_ma_workbook(dat, single, layout = "single")
  env <- new.env()
  expect_equal(load(single, envir = env), "ma_data")
  expect_equal(nrow(env$ma_data), 3)
  expect_error(write_ma_workbook(dat, withr::local_tempfile(fileext = ".csv")), "must end in")
})

test_that("the combined sheet has one row per study with outcomes side by side", {
  dat <- example_data()
  dat$outcomes <- rbind(dat$outcomes, transform(dat$outcomes, outcome_id = "icu", n_int = c(20, 18, 16)))
  merged <- merge_ma_data(dat)
  expect_equal(nrow(merged), 3)
  expect_equal(merged$n_int_icu, c(20, 18, 16))
  expect_equal(merged$n_int_pain, c(50, 40, 30))
  expect_true(match("outcome_id_icu", names(merged)) > match("design_effect_pain", names(merged)))

  d <- example_outcomes()[c(1, 1), ]
  d$int_arm <- c("low", "high")
  d$ctrl_arm <- "placebo"
  arms <- merge_ma_data(new_ma_data(outcomes = d))
  expect_equal(nrow(arms), 1)
  expect_true(all(c("n_int_pain_low", "n_int_pain_high") %in% names(arms)))
})

test_that(".rda files can use a chosen object name", {
  dat <- example_data()
  single <- withr::local_tempfile(fileext = ".rda")
  write_ma_workbook(dat, single, layout = "single", object_name = "los_review")
  env <- new.env()
  expect_equal(load(single, envir = env), "los_review")
  sheets <- withr::local_tempfile(fileext = ".rda")
  write_ma_workbook(dat, sheets, object_name = "los")
  env <- new.env()
  expect_setequal(load(sheets, envir = env), c("los_studies", "los_outcomes", "los_risk_of_bias", "los_inspect_sr"))
  expect_setequal(names(recognised_sheets(names(read_upload_sheets(sheets, "x.rda")))), c("study", "outcome"))
  expect_error(write_ma_workbook(dat, single, object_name = "my review"), "valid R name")
})
