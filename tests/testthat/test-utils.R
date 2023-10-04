test_that("rbits generates the correct size", {
  v <- rbits(10)
  m <- rbits(c(5, 10))
  arr <- rbits(c(5, 10, 3))

  expect_equal(length(v), 10)
  expect_equal(dim(m), c(5, 10))
  expect_equal(dim(arr), c(5, 10, 3))
})

test_that("rbits fails if number of probs does not match size", {
  P <- runif(10)
  expect_error(rbits(15, P))
})

test_that("hamming_weight handles `NA`", {
  v <- sample(c(0, 1, NA), 1000, replace = TRUE)
  weight <- hamming_weight(v, norm = TRUE)
  expect_lt(abs(weight - 0.5), 0.1)
})

test_that("hamming_dist fails on different lengths", {
  expect_error(hamming_dist(c(0, 0), c(0, 1, 0)))
})

test_that("ratio_bits discards `NA`", {
  ratio <- ratio_bits(c(1, 0, 1, NA, NA))
  expect_equal(ratio, 1 / 3)
})


test_that('compare_pairwise creates all pairs of rows', {
  m <- rbits(c(5, 10))
  npairs <- (nrow(m) * (nrow(m) - 1)) / 2
  res <- compare_pairwise(m, sum)
  expect_equal(length(res), npairs)
})
