test_that("design effects follow bayesma's formulas", {
  deff <- compute_design_effect(c("rct", "c_rct", "xo_rct", "cxo_rct", NA),
                                cluster_size = c(NA, 20, NA, 20, NA),
                                rho_w = c(NA, 0.05, NA, 0.05, NA),
                                rho_b = c(NA, NA, NA, 0.02, NA),
                                rho_wp = c(NA, NA, 0.6, NA, NA))
  expect_equal(deff, c(1, 1.95, 0.2, 1.95 - 0.4, 1))
  expect_equal(compute_design_effect("cluster", cluster_size = 11, rho_w = 0.1), 2)
  expect_equal(compute_design_effect(c("c_rct", "xo_rct"), labels = c("A", "B")), c(1, 1))
  expect_equal(compute_design_effect("cxo_rct", cluster_size = 11, rho_w = 0.1), 2)
  expect_error(compute_design_effect("stepped"), "Unrecognised")
})

test_that("effect sizes use effective sample sizes but keep the reported ones", {
  d <- example_outcomes()[1:2, ]
  d$trial_design <- c("rct", "c_rct")
  d$cluster_size <- c(NA, 11)
  d$rho_w <- c(NA, 0.1)
  es <- compute_effect_size(d, "MD")
  expect_equal(es$n_int, c(50, 40))
  expect_equal(es$n_int_eff, c(50, 20))
  expect_equal(es$design_effect, c(1, 2))
  plain <- compute_effect_size(d, "MD", design_effect = "ignore")
  expect_equal(es$sei[2], plain$sei[2] * sqrt(2))
  expect_equal(es$yi, plain$yi)
})

test_that("trial design can come from Study Details", {
  dat <- example_data()
  dat$outcomes$trial_design <- NULL
  dat$outcomes$cluster_size <- c(NA, 11, NA)
  dat$outcomes$rho_w <- c(NA, 0.1, NA)
  dat$studies$trial_design <- c("rct", "c_rct", "rct")
  out <- as_bayesma(dat, "pain", measure = "OR")
  expect_equal(out$design_effect, c(1, 2, 1))
  expect_equal(out$n_int, c(50, 40, 30))
  dat$outcomes$rho_w <- NA
  expect_true(any(validate_ma_data(dat)$column == "rho_w"))
})

test_that("fill_design_effect always recalculates", {
  dat <- example_data()
  dat$studies$trial_design <- c("rct", "c_rct", "cxo_rct")
  dat$outcomes$trial_design <- NULL
  dat$outcomes$cluster_size <- c(NA, 11, 11)
  dat$outcomes$rho_w <- c(NA, 0.1, NA)
  dat$outcomes$design_effect <- c(NA, NA, 1.5)
  expect_equal(fill_design_effect(dat)$outcomes$design_effect, c(1, 2, 1))
  expect_equal(as_bayesma(dat, "pain", measure = "OR")$design_effect, c(1, 2, 1))
})

test_that("cluster size is derived from the number of clusters", {
  expect_equal(compute_design_effect("c_rct", n_clusters = 10, n_total = 110, rho_w = 0.1), 2)
  expect_equal(compute_design_effect("cxo_rct", n_clusters = 5, n_total = 110, rho_w = 0.1), 2)
  expect_equal(compute_design_effect("c_rct", n_clusters = 10, n_total = NA, rho_w = 0.1), 1)
  expect_equal(compute_design_effect("c_rct", n_clusters = 10, n_total = 110, cluster_size = 21,
                                     rho_w = 0.1), 3)
})

test_that("outcomes inherit Study Details values unless they have their own", {
  dat <- example_data()
  dat$studies$trial_design <- c("rct", "c_rct", "rct")
  dat$studies$n_int <- c(100, 60, 30)
  dat$studies$n_ctrl <- c(100, 50, 31)
  dat$studies$n_clusters <- c(NA, 10, NA)
  dat$studies$rho_w <- c(NA, 0.1, NA)
  dat$outcomes$trial_design <- NULL
  dat$outcomes$n_int <- c(50, NA, NA)
  dat$outcomes$n_ctrl <- c(50, NA, NA)

  filled <- fill_study_defaults(dat$outcomes, dat$studies)
  expect_equal(filled$n_int, c(50, 60, 30))
  expect_equal(filled$n_ctrl, c(50, 50, 31))
  expect_equal(filled$rho_w, c(NA, 0.1, NA))

  out <- as_bayesma(dat, "pain", measure = "OR")
  expect_equal(out$n_int, c(50, 60, 30))
  expect_equal(out$design_effect, c(1, 2, 1))
  expect_equal(fill_design_effect(dat)$outcomes$design_effect, c(1, 2, 1))
  expect_equal(nrow(validate_ma_data(dat)), 0)

  dat$studies$rho_w <- NA
  expect_true(any(validate_ma_data(dat)$column == "rho_w"))
  dat$studies$n_int[2] <- "sixty"
  expect_true(any(validate_ma_data(dat)$section == "studies" & validate_ma_data(dat)$column == "n_int"))
})

test_that("cluster size uses the number of periods", {
  deff <- compute_design_effect(
    c("cxo_rct", "cxo_rct", "cxo_rct", "c_rct"),
    n_clusters = c(7, 22, 1, 10), n_periods = c(2, 2, 15, NA), n_total = c(90, 498, 172, 110),
    rho_w = c(0.083, 0.0108, 0.059, 0.1), rho_b = c(NA, 0, NA, NA)
  )
  expect_equal(round(deff, 6), c(1.450571, 1.111436, 1.617533, 2))
  expect_equal(compute_design_effect("cxo_rct", n_clusters = 5, n_total = 110, rho_w = 0.1), 2)
})
