test_that("edit_ma_cells keeps types and rebuilds automatic study IDs", {
  data <- template_ma_data() |>
    add_ma_rows("studies") |>
    edit_ma_cells("studies", row = 1, column = c("author", "year"), value = c("Smith", "2020"))
  expect_equal(data$studies$study_id, "Smith 2020")
  expect_type(data$studies$year, "double")

  data <- edit_ma_cells(data, "studies", row = 1, column = "study_id", value = "S1") |>
    edit_ma_cells("studies", row = 1, column = "year", value = "2021")
  expect_equal(data$studies$study_id, "S1")

  expect_error(edit_ma_cells(data, "studies", row = 5, column = "author", value = "x"), "between 1 and 1")
  expect_error(edit_ma_cells(data, "studies", row = 1, column = "nope", value = "x"), "nope")
})

test_that("add_ma_rows and remove_ma_rows change rows and design effects", {
  data <- template_ma_data() |>
    add_ma_rows("outcomes", study_id = c("S1", "S2"), outcome_id = "pain", measure = "MD") |>
    add_ma_rows("outcomes", study_id = "S1", outcome_id = "mort")
  expect_equal(data$outcomes$outcome_id, c("pain", "pain", "mort"))
  expect_equal(data$outcomes$design_effect, rep(1, 3))
  expect_equal(nrow(add_ma_rows(data, "studies", .n = 2)$studies), 2)

  expect_equal(remove_ma_rows(data, "outcomes", outcome_id == "mort")$outcomes$study_id, c("S1", "S2"))
  expect_equal(remove_ma_rows(data, "outcomes", dplyr::row_number() %in% 1)$outcomes$study_id, c("S2", "S1"))
  expect_equal(nrow(remove_ma_rows(data, "outcomes", TRUE)$outcomes), 0)
  expect_error(remove_ma_rows(data, "outcomes"), "at least one condition")
  expect_error(add_ma_rows(data, "outcome"), class = "rlang_error")
})

test_that("modify_ma_columns adds, converts, drops and renames", {
  data <- template_ma_data() |>
    add_ma_rows("studies", study_id = "S1", year = 2020L) |>
    modify_ma_columns("studies", add = "country", types = c(year = "numeric"),
                      drop = "rho_wp", rename = c(icc = "rho_w"))
  expect_true(all(c("country", "icc") %in% names(data$studies)))
  expect_false(any(c("rho_wp", "rho_w") %in% names(data$studies)))
  expect_type(data$studies$year, "double")
  expect_error(modify_ma_columns(data, "studies", drop = "study_id"), "cannot be dropped")
  expect_error(modify_ma_columns(data, "studies", types = c(year = "float")), "float")
})

test_that("sync_ma_section fills each section from Study Details", {
  data <- template_ma_data() |>
    add_ma_rows("studies", study_id = c("S1", "S2"), author = c("A", "B"), year = c(2020, 2021)) |>
    add_ma_rows("outcomes", study_id = "S1", outcome_id = "pain", measure = "MD")
  expect_equal(sync_ma_section(data, "outcomes")$outcomes$study_id, c("S1", "S2"))
  rob <- sync_ma_section(data, "risk_of_bias", rob_tool = "robins_i")$risk_of_bias
  expect_equal(rob$rob_tool, c("robins_i", "robins_i"))
  expect_equal(sync_ma_section(data, "inspect_sr")$inspect_sr$author, c("A", "B"))
})

test_that("import_ma_data maps columns and import_study_column adds one", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Author = c("Smith", "Jones"), Year = c(2020, 2021), Country = c("AU", "NZ"),
                              N1 = c(50, 40), N2 = c(51, 41), E1 = c(5, 4), E2 = c(6, 3)),
                   path, row.names = FALSE)
  sheet <- tools::file_path_sans_ext(basename(path))
  spec <- list(
    studies = list(sheet = sheet, header_row = 1, columns = list(author = "Author", year = "Year")),
    outcomes = list(list(type = "binary", sheet = sheet, header_row = 1, outcome_id = "mort",
                         columns = list(author = "Author", year = "Year", n_int = "N1", n_ctrl = "N2",
                                        event_int = "E1", event_ctrl = "E2")))
  )
  data <- import_ma_data(template_ma_data(), path, spec)
  expect_equal(data$studies$study_id, c("Smith 2020", "Jones 2021"))
  expect_equal(data$outcomes$event_ctrl, c(6, 3))
  expect_equal(data$outcomes$measure, c("OR", "OR"))

  sparse <- spec
  sparse$outcomes[[1]]$columns$event_int <- "E3"
  sparse$outcomes[[1]]$columns$event_ctrl <- "E4"
  utils::write.csv(data.frame(Author = c("Smith", "Jones"), Year = c(2020, 2021), Country = c("AU", "NZ"),
                              N1 = c(50, 40), N2 = c(51, 41), E1 = c(5, 4), E2 = c(6, 3), E3 = c(NA, 2), E4 = c(NA, 1)),
                   path, row.names = FALSE)
  expect_equal(import_ma_data(template_ma_data(), path, sparse)$outcomes$study_id, "Jones 2021")

  data <- import_study_column(data, path, "Country", sheet = sheet, keys = c(author = "Author", year = "Year"))
  expect_equal(data$studies$Country, c("AU", "NZ"))
  expect_error(import_ma_data(data, path, list(studies = list(sheet = "nope"))), "nope")
  expect_error(import_ma_data(data, path, list(foo = list())), "spec")
})

test_that("format_ma_script pipes steps and merges consecutive edits", {
  steps <- list(
    new_step("add_ma_rows", "studies"),
    new_step("edit_ma_cells", "studies", row = 1, column = "author", value = "Smith"),
    new_step("edit_ma_cells", "studies", row = 1, column = "year", value = "2020"),
    new_step("remove_ma_rows", "outcomes", rlang::expr(outcome_id == "x"))
  )
  script <- format_ma_script(steps, new_step("as_bayesma", outcome = "x"), date = as.Date("2026-01-01"))
  expect_true("library(bayesmaDatR)" %in% script)
  expect_true(any(grepl('edit_ma_cells("studies", row = c(1, 1), column = c("author", "year")', script, fixed = TRUE)))
  expect_true(any(grepl('remove_ma_rows("outcomes", outcome_id == "x")', script, fixed = TRUE)))
  expect_true(any(grepl('as_bayesma(outcome = "x")', script, fixed = TRUE)))
  expect_false(endsWith(script[length(script) - 1], "|>"))
})
