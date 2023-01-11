test_that("hamming_weight handles `NA`", {
  v <- sample(c(0, 1, NA), 1000, replace = TRUE)
  weight <- hamming_weight(v, norm = TRUE)
  expect_lt(abs(weight - 0.3), 0.1)
})

test_that("hamming_dist fails on different lengths", {
  expect_error(hamming_dist(c(0, 0), c(0, 1, 0)))
})

test_that("ratio_bits handles `NA`", {
  v <- sample(c(0, 1, NA), 1000, replace = TRUE)
  ratio <- ratio_bits(v)
  expect_lt(ratio, 0.3)
})

test_that("ratio_bits discards `NA`", {
  ratio <- ratio_bits(c(1, 0, 1, NA, NA), na.rm = TRUE)
  expect_equal(ratio, 1 / 3)
})
