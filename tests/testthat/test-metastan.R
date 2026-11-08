test_that("as_MetaStan builds MetaStan's arm-based columns", {
  wide <- as_MetaStan(example_data(), "pain", measure = "OR", output = "wide")
  expect_equal(attr(wide, "likelihood"), "binomial")
  expect_equal(wide$nd, c(2, 2, 2))
  expect_equal(wide$r1, c(15, 3, 8))
  expect_equal(wide$n2, c(50, 40, 30))
  expect_equal(wide$arm_id1, rep("ctrl", 3))

  expect_warning(md <- as_MetaStan(example_data(), "pain", measure = "MD", output = "wide"), "S3")
  expect_equal(md$study_id, c("S1", "S2"))
  expect_error(as_MetaStan(example_data(), "pain", measure = "SMD"), "cannot use")
})

test_that("as_MetaStan handles multi-arm studies and design effects", {
  d <- example_outcomes()[c(1, 1, 2), ]
  d$int_arm <- c("low", "high", "int")
  d$ctrl_arm <- c("pbo", "pbo", "ctrl")
  d$trial_design <- c("c_rct", "c_rct", "rct")
  d$cluster_size <- c(11, 11, NA)
  d$rho_w <- c(0.1, 0.1, NA)
  wide <- as_MetaStan(new_ma_data(outcomes = d), measure = "MD", output = "wide")
  expect_equal(wide$nd, c(3, 2))
  expect_equal(wide$arm_id1, c("pbo", "ctrl"))
  expect_equal(wide$SE1[1], 1.1 / sqrt(50 / 2))
  expect_equal(wide$n_reported1[1], 50)
})

test_that("as_MetaStan output feeds MetaStan", {
  skip_if_not_installed("MetaStan")
  out <- as_MetaStan(example_data(), "pain", measure = "OR")
  expect_named(out, c("data_long", "data_wide"))
  expect_equal(nrow(out$data_long), 6)
  expect_equal(attr(out, "likelihood"), "binomial")
})
