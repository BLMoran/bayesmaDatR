test_that("app cell edits keep column types", {
  expect_equal(coerce_cell(c(1, 2), 1, "5"), c(5, 2))
  expect_type(coerce_cell(c(NA, NA), 1, "5"), "double")
  expect_type(coerce_cell(c(NA, NA), 1, "Low"), "character")
  expect_equal(coerce_cell(c(1, 2), 2, ""), c(1, NA))
})

test_that("legacy column names are matched on import", {
  cols <- c("Author", "Year", "N_Control", "Primary_Mean_Intervention")
  expect_equal(best_column_match("n_ctrl", cols), "N_Control")
  expect_equal(best_column_match("mean_int", cols, prefix = "Primary"), "Primary_Mean_Intervention")
  expect_equal(best_column_match("sd_int", cols), "(Skip)")
  tagged <- c("N_int", "N_ctrl", "N_hosplos_int", "N_hosplos_ctrl")
  expect_equal(best_column_matches("n_int", tagged, prefix = "hosplos"), c("N_hosplos_int", "N_int"))
  expect_equal(best_column_matches("n_int", tagged), "N_int")
  expect_equal(make_study_id(c("Smith", NA, "Lee"), c(2020, 2021, NA)), c("Smith 2020", NA, "Lee"))
})

test_that("app server manages studies, outcomes, RoB and export", {
  skip_on_cran()
  shiny::testServer(app_server, {
    session$setInputs(rob_tool = "rob2", dl_format = "xlsx")
    session$setInputs(add_study = 1)
    session$setInputs(study_tbl_cell_edit = list(row = 1, col = 2, value = "Smith"))
    session$setInputs(study_tbl_cell_edit = list(row = 1, col = 3, value = 2020))
    expect_equal(rv$tables$studies$study_id, "Smith 2020")
    expect_type(rv$tables$studies$year, "double")

    session$setInputs(add_outcome = 1, new_outcome_id = "mort", new_outcome_name = "",
                      new_outcome_timepoint = "", new_outcome_design = "rct",
                      new_outcome_measure = "OR")
    session$setInputs(confirm_add_outcome = 1)
    expect_equal(rv$tables$outcomes$study_id, "Smith 2020")

    expect_equal(outcome_type_filter(), "binary")
    session$setInputs(otype_all = 1)
    values <- c(n_int = "50", n_ctrl = "50", event_int = "10", event_ctrl = "15")
    for (nm in names(values)) {
      col <- match(nm, names(rv$tables$outcomes))
      session$setInputs(outcome_tbl_cell_edit = list(row = 1, col = col, value = values[[nm]]))
    }
    expect_equal(rv$tables$outcomes$event_ctrl, 15)

    session$setInputs(sync_rob = 1)
    session$setInputs(rob_tbl_cell_clicked = list(row = 1, col = 5, value = NA))
    session$setInputs(btn_rob_rob2_3 = 1)
    expect_equal(rv$tables$risk_of_bias$D1, "High")

    session$setInputs(exp_outcome = "mort", exp_timepoint = "(All)", exp_measure = "OR",
                      exp_model_type = "two_stage", exp_format = "wide", preview_btn = 1)
    expect_equal(rv$export$df$rob_D1, "High")
    expect_equal(rv$export$df$yi, log((10 * 35) / (40 * 15)))

    session$setInputs(add_study = 2)
    session$setInputs(add_study = 3)
    expect_equal(nrow(rv$tables$studies), 3)
    session$setInputs(study_tbl_checked = list(2, 3), del_study = 1)
    expect_equal(rv$tables$studies$study_id, "Smith 2020")
  })
})

test_that("upload sheets keep title rows and find the header row", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Study details,,", "Author,Year,Country", "Smith,2020,AU", "Jones,2021,NZ"), path)
  raw <- read_upload_sheets(path, "studies.csv")[[1]]
  expect_equal(guess_header_row(raw), 2)
  df <- sheet_from_raw(raw, 2)
  expect_named(df, c("Author", "Year", "Country"))
  expect_type(df$Year, "integer")
  expect_equal(nrow(sheet_from_raw(raw, 1)), 3)
})

test_that("each section imports from its own file, sheet and header row", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  outcomes <- data.frame(
    c("Outcome data", "Study", "Smith 2020", "Jones 2021"), c(NA, "Outcome", "mort", "mort"),
    c(NA, "N_Intervention", "50", "40"), c(NA, "N_Control", "50", "42"),
    c(NA, "Event_Intervention", "10", "0"), c(NA, "Event_Control", "15", "3")
  )
  writexl::write_xlsx(list(Notes = data.frame(x = "read me"), Extraction = outcomes),
                      file.path(dir, "outcomes.xlsx"), col_names = FALSE)
  rob <- data.frame(c("RoB 2", "Author", "Smith", "Jones"), c(NA, "Year", "2020", "2021"),
                    c(NA, "D1", "low", "Low"), c(NA, "D2", "Low", "Low"), c(NA, "D3", "Low", "Low"),
                    c(NA, "D4", "Low", "Low"), c(NA, "D5", "Low", "high"), c(NA, "Overall", "Low", "High"))
  writexl::write_xlsx(list(RoB = rob), file.path(dir, "rob.xlsx"), col_names = FALSE)

  shiny::testServer(app_server, {
    set1 <- function(name, value) do.call(session$setInputs, stats::setNames(list(value), name))
    session$setInputs(rob_tool = "rob2")

    session$setInputs(upload_file = list(name = "outcomes.xlsx", datapath = file.path(dir, "outcomes.xlsx")),
                      upload_map_btn = 1)
    set1(mid("sections"), "outcome")
    set1(mid("sheet_outcome"), "Extraction")
    expect_equal(section_header_row("outcome"), 2)
    cols <- names(block_df(1))
    set1(mid("outcome_1_id_study_id"), "Study")
    set1(mid("outcome_1_outcome_id"), "Outcome")
    for (cc in outcome_type_columns("binary")) set1(mid(paste0("outcome_1_", cc)), best_column_match(cc, cols))
    session$setInputs(confirm_import = 1)
    expect_equal(rv$tables$outcomes$event_ctrl, c(15, 3))

    session$setInputs(upload_file = list(name = "rob.xlsx", datapath = file.path(dir, "rob.xlsx")),
                      upload_map_btn = 2)
    set1(mid("sections"), "rob")
    expect_equal(section_header_row("rob"), 2)
    cols <- names(section_df("rob"))
    set1(mid("rob_id_author"), "Author")
    set1(mid("rob_id_year"), "Year")
    for (cc in rob_domains("rob2")) set1(mid(paste0("rob_", cc)), best_column_match(cc, cols))
    session$setInputs(confirm_import = 2)

    expect_equal(rv$tables$risk_of_bias$D5, c("Low", "High"))
    expect_equal(nrow(rv$tables$outcomes), 2)
    expect_equal(rv$tables$studies$author, c("Smith", "Jones"))
    expect_equal(nrow(validate_ma_data(current_data())), 0)
  })
})

test_that("import falls back across columns and splits IQR cells", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  sheet <- data.frame(
    Author = c("Smith", "Jones", "Lee"), Year = c(2020, 2021, 2022),
    N_int = c(50, 40, 30), N_ctrl = c(50, 42, 31),
    N_hosplos_int = c(45, NA, 28), N_hosplos_ctrl = c(47, NA, NA),
    Mean_int = c(3, NA, NA), SD_int = c(1, NA, NA),
    Median_int = c(NA, 4, 5), IQR_int = c(NA, "3, 6", "4; 7")
  )
  writexl::write_xlsx(list(LOS = sheet), file.path(dir, "los.xlsx"))
  shiny::testServer(app_server, {
    set1 <- function(name, value) do.call(session$setInputs, stats::setNames(list(value), name))
    session$setInputs(rob_tool = "rob2")
    session$setInputs(upload_file = list(name = "los.xlsx", datapath = file.path(dir, "los.xlsx")),
                      upload_map_btn = 1)
    set1(mid("sections"), "outcome")
    session$setInputs(imp_otype_1_continuous = 1)
    cols <- names(section_df("outcome"))
    set1(mid("outcome_1_id_author"), "Author")
    set1(mid("outcome_1_id_year"), "Year")
    set1(mid("imp_id_1"), "hosplos")
    for (cc in c(outcome_type_columns("continuous"), names(combined_cell_columns()))) {
      set1(mid(paste0("outcome_1_", cc)), best_column_matches(cc, cols, prefix = "hosplos"))
    }
    expect_equal(input[[mid("outcome_1_n_int")]], c("N_hosplos_int", "N_int"))
    session$setInputs(confirm_import = 1)
    out <- rv$tables$outcomes
    expect_equal(out$n_int, c(45, 40, 28))
    expect_equal(out$n_ctrl, c(47, 42, 31))
    expect_equal(out$q1_int, c(NA, 3, 4))
    expect_equal(out$q3_int, c(NA, 6, 7))
    expect_equal(out$mean_int, c(3, NA, NA))
  })
})

test_that("imports drop skipped columns and normalise design codes", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writexl::write_xlsx(list(Studies = data.frame(Author = c("Ramanan", "Tsui"), Year = c(2021, 2019),
                                                Design = c("CXRCT", "XRCT"))),
                      file.path(dir, "studies.xlsx"))
  shiny::testServer(app_server, {
    set1 <- function(name, value) do.call(session$setInputs, stats::setNames(list(value), name))
    session$setInputs(rob_tool = "rob2")
    session$setInputs(upload_file = list(name = "studies.xlsx", datapath = file.path(dir, "studies.xlsx")),
                      upload_map_btn = 1)
    set1(mid("sections"), "study")
    set1(mid("study_id_author"), "Author")
    set1(mid("study_id_year"), "Year")
    set1(mid("study_trial_design"), "Design")
    session$setInputs(confirm_import = 1)
    expect_equal(rv$tables$studies$trial_design, c("cxo_rct", "xo_rct"))
    expect_named(rv$tables$studies, c("study_id", "author", "year", "trial_design"))
  })
})

test_that("each outcome block can use its own sheet", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  hosp <- data.frame(Author = c("Smith", "Jones"), Year = c(2020, 2021),
                     N_int = c(50, 40), N_ctrl = c(50, 42), Mean_int = c(5, 6), SD_int = c(1, 1),
                     Mean_ctrl = c(6, 7), SD_ctrl = c(1, 1))
  icu <- data.frame(Study = c("Smith 2020", "Jones 2021"),
                    N_icu_int = c(20, 18), N_icu_ctrl = c(21, 19), Mean_icu_int = c(2, 3),
                    SD_icu_int = c(1, 1), Mean_icu_ctrl = c(3, 4), SD_icu_ctrl = c(1, 1))
  writexl::write_xlsx(list(Hospital = hosp, ICU = icu), file.path(dir, "los.xlsx"))
  shiny::testServer(app_server, {
    set1 <- function(name, value) do.call(session$setInputs, stats::setNames(list(value), name))
    session$setInputs(rob_tool = "rob2")
    session$setInputs(upload_file = list(name = "los.xlsx", datapath = file.path(dir, "los.xlsx")),
                      upload_map_btn = 1)
    set1(mid("sections"), "outcome")
    session$setInputs(imp_otype_1_continuous = 1, imp_add_outcome = 1)
    session$setInputs(imp_otype_2_continuous = 1)
    set1(mid("imp_id_1"), "hosp_los")
    set1(mid("imp_id_2"), "icu_los")
    set1(mid("osheet_2"), "ICU")
    expect_equal(names(block_df(2))[1], "Study")
    for (ii in 1:2) {
      cols <- names(block_df(ii))
      tag <- if (ii == 2) "icu" else ""
      for (cc in c("study_id", "author", "year")) {
        set1(mid(paste0("outcome_", ii, "_id_", cc)), best_column_matches(cc, cols))
      }
      for (cc in outcome_type_columns("continuous")) {
        set1(mid(paste0("outcome_", ii, "_", cc)), best_column_matches(cc, cols, prefix = tag))
      }
    }
    session$setInputs(confirm_import = 1)
    out <- rv$tables$outcomes
    expect_equal(out$n_int[out$outcome_id == "icu_los"], c(20, 18))
    expect_equal(out$mean_ctrl[out$outcome_id == "hosp_los"], c(6, 7))
    expect_equal(sort(unique(out$study_id)), c("Jones 2021", "Smith 2020"))
    expect_equal(out$design_effect, rep(1, 4))
  })
})

test_that("extra study columns keep their names and every workbook download works", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writexl::write_xlsx(list(Studies = data.frame(Author = c("Smith", "Jones"), Year = c(2020, 2021),
                                                subgroup1 = c("adult", "child"), subgroup2 = c("ED", "ICU"))),
                      file.path(dir, "studies.xlsx"))
  shiny::testServer(app_server, {
    set1 <- function(name, value) do.call(session$setInputs, stats::setNames(list(value), name))
    session$setInputs(rob_tool = "rob2")
    session$setInputs(upload_file = list(name = "studies.xlsx", datapath = file.path(dir, "studies.xlsx")),
                      upload_map_btn = 1)
    set1(mid("sections"), "study")
    set1(mid("study_id_author"), "Author")
    set1(mid("study_id_year"), "Year")
    set1(mid("study_extra"), c("subgroup1", "subgroup2"))
    session$setInputs(confirm_import = 1)
    expect_equal(rv$tables$studies$subgroup2, c("ED", "ICU"))
    for (fmt in c("xlsx", "rda")) for (layout in c("sheets", "single")) {
      session$setInputs(wb_format = fmt, wb_layout = layout)
      expect_true(file.exists(output$dl_workbook))
    }
    session$setInputs(dl_format = "csv")
    studies <- utils::read.csv(output$dl_studies)
    expect_equal(names(studies), c("study_id", "author", "year", "trial_design", "subgroup1", "subgroup2"))
  })
})
