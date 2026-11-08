test_that("split_iqr and split_range separate paired cells", {
  out <- split_iqr(c("2, 5", "[1.5; 3]", "4 to 9", "(2.1, 3.4)", "7", NA))
  expect_named(out, c("q1", "q3"))
  expect_equal(out$q1, c(2, 1.5, 4, 2.1, NA, NA))
  expect_equal(out$q3, c(5, 3, 9, 3.4, NA, NA))
  expect_equal(split_range("-3, 4")$min, -3)
  expect_equal(split_iqr("2 - 5", sep = "\\s+-\\s+")$q3, 5)
  expect_equal(nrow(split_range(character())), 0)
})
