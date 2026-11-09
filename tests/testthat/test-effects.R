test_that("effect sizes match metafor", {
  skip_if_not_installed("metafor")
  d <- example_outcomes()[1:2, ]
  or <- metafor::escalc("OR", ai = event_int, n1i = n_int, ci = event_ctrl, n2i = n_ctrl, data = d)
  expect_equal(compute_effect_size(d, "OR")$yi, as.numeric(or$yi), tolerance = 1e-8)
  expect_equal(compute_effect_size(d, "OR")$sei, sqrt(as.numeric(or$vi)), tolerance = 1e-8)
  md <- metafor::escalc("MD", m1i = mean_int, sd1i = sd_int, n1i = n_int,
                        m2i = mean_ctrl, sd2i = sd_ctrl, n2i = n_ctrl, data = d)
  expect_equal(compute_effect_size(d, "MD")$sei, sqrt(as.numeric(md$vi)))
  smd <- metafor::escalc("SMD", m1i = mean_int, sd1i = sd_int, n1i = n_int,
                         m2i = mean_ctrl, sd2i = sd_ctrl, n2i = n_ctrl, data = d)
  expect_equal(compute_effect_size(d, "SMD")$yi, as.numeric(smd$yi), tolerance = 1e-4)
})

test_that("IRR uses events and person-time", {
  d <- example_outcomes()[1, ]
  expect_equal(compute_effect_size(d, "IRR")$yi, log((10 / 100) / (15 / 100)))
})

test_that("MedD works without min/max columns and names failures", {
  skip_if_not_installed("metamedian")
  d <- example_outcomes()
  es <- compute_effect_size(d, "MedD")
  expect_equal(es$yi[3], -1)
  expect_true(all(is.finite(es$sei)))
  d$mean_int[1] <- NA
  expect_warning(es <- compute_effect_size(d, "MedD"), "S1")
  expect_true(is.na(es$yi[1]))
})

test_that("convert_median_to_mean fills only missing values", {
  skip_if_not_installed("metamedian")
  d <- convert_median_to_mean(example_outcomes(), method = "wan")
  expect_equal(d$mean_ctrl_method, c("reported", "reported", "wan"))
  expect_equal(d$mean_int[1:2], c(3, 4))
  expect_equal(d$mean_int_method, c("reported", "reported", "wan"))
  expect_true(d$mean_int[3] > 2 && d$mean_int[3] < 5)
})

test_that("every metamedian method estimates means and SDs reproducibly", {
  skip_if_not_installed("metamedian")
  for (method in c("wan", "luo", "qe", "bc", "shi", "yang")) {
    d <- convert_median_to_mean(example_outcomes(), method = method)
    expect_equal(d$mean_int_method[3], method)
    expect_true(d$mean_int[3] > 2 && d$mean_int[3] < 5, label = method)
    expect_true(d$sd_int[3] > 0, label = method)
  }
  set.seed(42)
  first <- convert_median_to_mean(example_outcomes(), method = "bc")
  after_first <- stats::runif(1)
  set.seed(42)
  second <- convert_median_to_mean(example_outcomes(), method = "bc")
  expect_equal(first, second)
  expect_equal(stats::runif(1), after_first)
  expect_error(convert_median_to_mean(example_outcomes(), method = "mln"), "method")
})

test_that("as_bayesma can estimate means for studies reporting medians", {
  skip_if_not_installed("metamedian")
  dat <- example_data()
  expect_warning(as_bayesma(dat, "pain", measure = "MD"), "estimate_means")
  out <- as_bayesma(dat, "pain", measure = "MD", estimate_means = "wan")
  expect_true(all(is.finite(out$yi)))
  expect_equal(out$mean_int[1:2], c(3, 4))
})
