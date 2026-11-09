tidy_fixture <- function(dir) {
  sheet <- data.frame(
    c("DKA extraction", "Author", "Smith", "Jones", "Lee", "Wu"),
    c(NA, "year", "2020", "2021", "2022", "2019"),
    c(NA, "trial_design", "RCT", "CRCT", "RCT", "parallel"),
    c(NA, "n_analysed_int", "50", "40", "30", "20"),
    c(NA, "n_analysed_ctrl", "51", "41", "31", "21"),
    c(NA, "n_clusters", NA, "8", NA, NA),
    c(NA, "rho_w", NA, "0.05", NA, NA),
    c(NA, "age_group", "adult", "child", "adult", "adult"),
    c(NA, "n_hosp_los_int", "48", NA, "29", NA),
    c(NA, "n_hosp_los_ctrl", "50", NA, NA, NA),
    c(NA, "hosp_los_mean_int", "5", "6", NA, NA),
    c(NA, "hosp_los_sd_int", "1", "1.2", NA, NA),
    c(NA, "hosp_los_mean_ctrl", "6", "7", NA, NA),
    c(NA, "hosp_los_sd_ctrl", "1.1", "1.3", NA, NA),
    c(NA, "hosp_los_median_int", NA, NA, "4", "3"),
    c(NA, "hosp_los_iqr_int", NA, NA, "3-6", "[2; 5]"),
    c(NA, "hosp_los_median_ctrl", NA, NA, "5", "4"),
    c(NA, "hosp_los_iqr_ctrl", NA, NA, "4 - 7", "3, 6"),
    c(NA, "deaths_int", "5", "4", "3", "2"),
    c(NA, "deaths_ctrl", "6", "5", "4", "3")
  )
  rob <- data.frame(Author = c("Smith", "Jones", "Lee", "Wu"), Year = c(2020, 2021, 2022, 2019),
                    D1 = c("low", "Low", "some concerns", "High"), Overall = c("Low", "low", "High", "high"))
  writexl::write_xlsx(list(Sheet1 = sheet), file.path(dir, "DKA.xlsx"), col_names = FALSE)
  writexl::write_xlsx(list(RoB = rob), file.path(dir, "rob.xlsx"))
  list(DKA.xlsx = file.path(dir, "DKA.xlsx"), rob.xlsx = file.path(dir, "rob.xlsx"))
}

tidy_spec <- function() {
  ids <- list(author = "Author", year = "year")
  list(
    studies = list(sheet = "Sheet1", header_row = 2, columns = c(ids, list(
      trial_design = "trial_design", n_int = "n_analysed_int", n_ctrl = "n_analysed_ctrl",
      n_clusters = "n_clusters", rho_w = "rho_w")), extra = "age_group"),
    outcomes = list(
      list(type = "continuous", sheet = "Sheet1", header_row = 2, outcome_id = "hosp_los",
           columns = c(ids, list(n_int = "n_hosp_los_int", n_ctrl = "n_hosp_los_ctrl",
                                 mean_int = "hosp_los_mean_int", mean_ctrl = "hosp_los_mean_ctrl",
                                 sd_int = "hosp_los_sd_int", sd_ctrl = "hosp_los_sd_ctrl",
                                 median_int = "hosp_los_median_int", median_ctrl = "hosp_los_median_ctrl",
                                 n_clusters = "n_clusters", rho_w = "rho_w",
                                 iqr_int = "hosp_los_iqr_int", iqr_ctrl = "hosp_los_iqr_ctrl"))),
      list(type = "binary", sheet = "Sheet1", header_row = 2, outcome_id = "mortality",
           columns = c(ids, list(event_int = "deaths_int", event_ctrl = "deaths_ctrl")))
    )
  )
}

check_tidy_steps <- function(steps, export, files) {
  app_data <- purrr::reduce(steps, \(d, s) run_ma_step(d, s, files), .init = template_ma_data())
  expected <- suppressWarnings(run_ma_step(app_data, export))
  script <- format_tidy_script(steps, export, files)
  list(script = script, check = check_tidy_script(script, files, expected))
}

test_that("the tidyverse script reproduces imports, edits and the export", {
  skip_on_cran()
  files <- tidy_fixture(withr::local_tempdir())
  rob_spec <- list(risk_of_bias = list(tool = "rob2", sheet = "RoB", header_row = 1,
                                       columns = list(author = "Author", year = "Year", D1 = "D1", Overall = "Overall")))
  steps <- list(
    new_step("import_ma_data", path = "DKA.xlsx", spec = tidy_spec()),
    new_step("import_ma_data", path = "rob.xlsx", spec = rob_spec),
    new_step("edit_ma_cells", "outcomes", row = 2, column = "mean_ctrl", value = "7.5"),
    new_step("add_ma_rows", "studies"),
    new_step("edit_ma_cells", "studies", row = 5, column = c("author", "year"), value = c("Zed", "2018")),
    new_step("modify_ma_columns", "studies", add = "country", rename = c(icc = "rho_w")),
    new_step("edit_ma_cells", "studies", row = 1, column = "country", value = "AU"),
    new_step("remove_ma_rows", "studies", rlang::expr(dplyr::row_number() %in% 5)),
    new_step("edit_ma_cells", "risk_of_bias", row = 3, column = "D1", value = "Low")
  )
  for (outcome in c("hosp_los", "mortality")) {
    export <- new_step("as_bayesma", outcome = outcome, estimate_means = "wan", design_effect = "apply",
                       model_type = "two_stage", format = "wide")
    result <- check_tidy_steps(steps, export, files)
    expect_true(result$check, label = outcome)
  }
  script <- result$script
  expect_true(any(grepl("n_int = dplyr::coalesce(n_hosp_los_int, n_analysed_int)", script, fixed = TRUE)))
  expect_true(any(grepl('"parallel" ~ "rct"', script, fixed = TRUE)))
  expect_true(any(grepl('"some concerns" ~ "Some concerns"', script, fixed = TRUE)))
  expect_true(any(grepl("dplyr::mutate(dplyr::across(D1:Overall, \\(x) dplyr::replace_values(", script, fixed = TRUE)))
  expect_equal(sum(grepl("replace_values(", script, fixed = TRUE)), 2)
  rob_block <- script[seq(match("# Risk of bias (rob2)", script), length(script))]
  expect_equal(unname(rob_block[4:6]), c("    study_id,", "    author,", "    year,"))
  expect_true(any(grepl("outcomes <- dplyr::bind_rows(", script, fixed = TRUE)))
  expect_true(any(grepl('replace(mean_ctrl, study_id == "Jones 2021" & outcome_id == "hosp_los", 7.5)', script,
                        fixed = TRUE)))
  expect_false(any(grepl("import_ma_data|edit_ma_cells|pair_pattern", script)))
  expect_true(any(grepl('bayesmaDatR::split_iqr(hosp_los_iqr_int, names = c("q1_int", "q3_int"))', script,
                        fixed = TRUE)))
  expect_true(any(grepl("outcomes = outcomes", script, fixed = TRUE)))
  expect_true(any(grepl("dplyr::rename(author = Author)", script, fixed = TRUE)))
  expect_true(any(grepl("dplyr::relocate(study_id, author, year, trial_design,", script, fixed = TRUE)))
  steps_at <- purrr::map_int(c("# Import data", "# Study details", "# Outcomes", "# Risk of bias (rob2)",
                               "ma_data <- bayesmaDatR::new_ma_data(", "  risk_of_bias = dat_rob", "dat_outcomes <- ma_data |>",
                               "dat_hosp_los <- ma_data |>"),
                             \(line) match(line, script))
  expect_false(anyNA(steps_at))
  expect_false(is.unsorted(steps_at))
})

test_that("outcomes fall back to the study N even when Study Details is imported later", {
  skip_on_cran()
  files <- tidy_fixture(withr::local_tempdir())
  spec <- tidy_spec()
  steps <- list(
    new_step("import_ma_data", path = "DKA.xlsx", spec = spec["outcomes"]),
    new_step("import_ma_data", path = "DKA.xlsx", spec = spec["studies"])
  )
  export <- new_step("as_bayesma", outcome = "hosp_los", estimate_means = "wan", model_type = "two_stage",
                     format = "wide")
  result <- check_tidy_steps(steps, export, files)
  expect_true(result$check)
  expect_true(any(grepl("n_ctrl = dplyr::coalesce(n_hosp_los_ctrl, n_analysed_ctrl)", result$script, fixed = TRUE)))
  expect_true(any(grepl("n_int = n_analysed_int,", result$script, fixed = TRUE)))
  expect_true(any(grepl("dplyr::filter_out(dplyr::if_all(c(event_int, event_ctrl), is.na))", result$script,
                        fixed = TRUE)))
})

test_that("the tidyverse script handles outcome-only imports and synced sections", {
  skip_on_cran()
  files <- tidy_fixture(withr::local_tempdir())
  spec <- tidy_spec()["outcomes"]
  steps <- list(
    new_step("import_ma_data", path = "DKA.xlsx", spec = spec),
    new_step("sync_ma_section", "inspect_sr"),
    new_step("sync_ma_section", "risk_of_bias", rob_tool = "rob2"),
    new_step("edit_ma_cells", "risk_of_bias", row = 2, column = "D2", value = "High"),
    new_step("remove_ma_rows", "outcomes", rlang::expr(outcome_id == "mortality"))
  )
  export <- new_step("as_bayesma", outcome = "hosp_los", model_type = "one_stage", format = "long")
  result <- check_tidy_steps(steps, export, files)
  expect_true(result$check)
  expect_true(any(grepl("# Studies named only in other sections", result$script, fixed = TRUE)))
})

test_that("a failed check is noted in the script header", {
  lines <- c("# header", "", "dat <- 1")
  expect_equal(note_tidy_check(lines, TRUE), lines)
  noted <- note_tidy_check(lines, "boom")
  expect_true(any(grepl("boom", noted)))
  expect_equal(match("", noted), 5)
})

test_that("the tidyverse script makes a data frame for every outcome and time point", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writexl::write_xlsx(list(Long = data.frame(
    Author = c("Smith", "Smith", "Jones", "Jones"), Year = c(2020, 2020, 2021, 2021),
    Outcome = "pain", Time = c("24h", "48h", "24h", "48h"),
    NT = 30, NC = 30, MT = c(3, 2, 4, 3), ST = 1, MC = c(4, 3, 5, 4), SC = 1,
    DT = c(2, NA, 3, NA), DC = c(4, NA, 5, NA)
  )), file.path(dir, "long.xlsx"))
  files <- list(long.xlsx = file.path(dir, "long.xlsx"))
  ids <- list(author = "Author", year = "Year")
  spec <- list(outcomes = list(
    list(type = "continuous", sheet = "Long", header_row = 1,
         columns = c(ids, list(outcome_id = "Outcome", timepoint = "Time", n_int = "NT", n_ctrl = "NC",
                               mean_int = "MT", sd_int = "ST", mean_ctrl = "MC", sd_ctrl = "SC"))),
    list(type = "binary", sheet = "Long", header_row = 1, outcome_id = "nausea",
         columns = c(ids, list(n_int = "NT", n_ctrl = "NC", event_int = "DT", event_ctrl = "DC")))
  ))
  steps <- list(new_step("import_ma_data", path = "long.xlsx", spec = spec))
  export <- new_step("as_bayesma", outcome = "pain", timepoint = "24h", model_type = "two_stage", format = "wide")
  result <- check_tidy_steps(steps, export, files)
  expect_true(result$check)
  expect_equal(attr(result$script, "result"), "dat_outcomes")
  for (line in c("dat_outcomes <- ma_data |>", "dat_pain_24h <- ma_data |>", "dat_pain_48h <- ma_data |>", "dat_nausea <- ma_data |>")) {
    expect_true(line %in% result$script, label = line)
  }
  expect_false(any(grepl("^# Could not", result$script)))
})

test_that("both script styles read the files from data_dir", {
  skip_on_cran()
  files <- tidy_fixture(withr::local_tempdir())
  steps <- list(new_step("import_ma_data", path = "DKA.xlsx", spec = tidy_spec()))
  export <- new_step("as_bayesma", outcome = "mortality", model_type = "two_stage", format = "wide")
  app_data <- run_ma_step(template_ma_data(), steps[[1]], files)
  expected <- run_ma_step(app_data, export)

  tidy <- format_tidy_script(steps, export, files, data_dir = "C:\\Data\\DKA")
  expect_true('data_dir <- "C:/Data/DKA"' %in% tidy)
  expect_true(any(grepl('readxl::read_excel(file.path(data_dir, "DKA.xlsx")', tidy, fixed = TRUE)))
  expect_true(check_tidy_script(tidy, files, expected))

  replay <- format_ma_script(steps, export)
  expect_true('data_dir <- "."' %in% replay)
  expect_true(any(grepl('import_ma_data(file.path(data_dir, "DKA.xlsx"), spec_1)', replay, fixed = TRUE)))
  withr::with_dir(dirname(files$DKA.xlsx), {
    env <- new.env()
    eval(parse(text = replay), envir = env)
    expect_equal(env$analysis_data, expected)
  })
})

test_that("an empty session still gives runnable code", {
  script <- format_tidy_script(list())
  expect_true("ma_data <- bayesmaDatR::new_ma_data()" %in% script)
  expect_s3_class(eval(parse(text = script)), "ma_data")
  expect_true("data <- template_ma_data()" %in% format_ma_script(list()))
})
