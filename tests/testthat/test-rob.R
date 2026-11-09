test_that("rob_domains follows robbR", {
  expect_equal(rob_domains("robins_i"), c(paste0("D", 1:6), "Overall"))
  expect_equal(rob_domains("clarity"), paste0("D", 1:12))
  expect_equal(rob_domains("rob_nma", include_overall = FALSE, include_optional = FALSE),
               c("D1", "D2", "D3"))
  expect_equal(rob_domains("probast"), c("D1", "D2", "D3", "D4", "A1", "A2", "A3",
                                         "Overall", "Applicability"))
  expect_error(rob_domains("robins_x"))
})

test_that("add_rob standardises and records the level", {
  rob <- data.frame(study_id = c("S1", "S2"), D1 = c("low risk", "some concerns"),
                    D2 = "Low", D3 = "Low", D4 = "Low", D5 = "high", Overall = "High")
  dat <- add_rob(example_data(), rob, rob_tool = "rob2")
  expect_equal(dat$risk_of_bias$D1, c("Low", "Some concerns"))
  expect_equal(dat$risk_of_bias$rob_level, c("study", "study"))
  expect_false(any(validate_ma_data(dat)$column %in% c("author", "year")))
})

test_that("add_rob rejects bad input", {
  rob <- data.frame(study_id = "S1", D1 = "Low", D2 = "Low", D3 = "Low", D4 = "Low", D5 = "Low")
  expect_error(add_rob(example_data(), rob, "rob2", rob_level = "outcome"), "outcome_id")
  expect_error(add_rob(example_data(), transform(rob, D1 = "Meh"), "rob2"), "Unrecognised")
  expect_error(add_rob(example_data(), rob[1:3], "rob2"), "missing required")
})

test_that("outcome-level ratings override study-level ones for that outcome only", {
  dat <- example_data()
  dat$outcomes <- rbind(dat$outcomes, transform(dat$outcomes[1, ], outcome_id = "nausea"))
  base <- data.frame(study_id = "S1", D1 = "Low", D2 = "Low", D3 = "Low", D4 = "Low",
                     D5 = "Low", Overall = "Low")
  dat <- add_rob(dat, base, "rob2")
  dat <- add_rob(dat, transform(base, outcome_id = "pain", D3 = "High", Overall = "High"),
                 "rob2", rob_level = "outcome")
  pain <- select_outcome(dat, "pain")
  nausea <- select_outcome(dat, "nausea")
  expect_equal(pain$rob_Overall, c("High", NA, NA))
  expect_equal(nausea$rob_Overall, "Low")
})

test_that("CLARITY yes/no answers map to judgements", {
  rob <- data.frame(study_id = "S1", rob_tool = "clarity", rob_level = "study",
                    D1 = "Definitely yes", D2 = "Probably yes", D3 = "Probably no", D4 = "Definitely no")
  dat <- new_ma_data(studies = data.frame(study_id = "S1"), risk_of_bias = rob)
  expect_equal(unlist(dat$risk_of_bias[c("D1", "D2", "D3", "D4")], use.names = FALSE),
               c("Definitely low", "Probably low", "Probably high", "Definitely high"))
  expect_false(any(validate_ma_data(dat)$column %in% c("D1", "D2", "D3", "D4")))
})

test_that("author and year in risk_of_bias are not treated as ratings", {
  dat <- example_data()
  dat$risk_of_bias <- data.frame(study_id = c("S1", "S2"), author = c("A", "B"), year = c(2020, 2021),
                                 rob_tool = "rob2", rob_level = "study", D1 = c("Low", "High"))
  out <- suppressWarnings(as_bayesma(dat, "pain", measure = "MD"))
  expect_false(any(c("rob_author", "rob_year", "rob_D1") %in% names(out)))
  expect_equal(out$D1[1:2], c("Low", "High"))
  expect_equal(sum(names(out) == "author"), 1)
  expect_false(any(c("rob_author", "rob_year") %in% names(merge_ma_data(dat))))
  expect_false(any(validate_ma_data(dat)$column %in% c("author", "year")))
})
