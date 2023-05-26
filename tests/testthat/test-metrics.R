test_that("entropy of vector full of 0s", {
  v <- rbits(1000, p = 0)
  entropy <- entropy_bits(v)
  expect_equal(entropy, 0)
})

test_that("entropy of vector full of 1s", {
  v <- rbits(1000, p = 1)
  entropy <- entropy_bits(v)
  expect_equal(entropy, 0)
})

test_that("entropy of a unbiased binary vector", {
  v <- rbits(1000)
  entropy <- entropy_bits(v)
  expect_lt(1 - entropy, 0.05)
})

test_that("crps_weight handles `NA`", {
  v <- sample(c(0, 1, NA), 1000, replace = TRUE)
  unif <- crps_weight(v)
  expect_lt(abs(unif - 0.5), 0.1)
})

test_that("crps_weight of a bit vector", {
  v <- rbits(1000)
  unif <- crps_weight(v)
  expect_lt(abs(unif - 0.5), 0.1)
})

test_that("crps_weight of a crp table", {
  mat <- matrix(rbits(5000), nrow = 10, ncol = 500)
  bitalias <- mean(crps_weight(mat, 2))
  expect_lt(abs(bitalias - 0.5), 0.1)
})

test_that("crps_weight throw an error with an invalid margin", {
  mat <- matrix(0, nrow = 5, ncol = 10)
  expect_error(crps_weight(mat, margin = 10))
})

test_that("crps_weight throw an error with an invalid input", {
  expect_error(crps_weight(list(a = 1, b = 2)))
})

test_that("intra_hd works on a 2D matrix", {
  mat <- matrix(0, nrow = 5, ncol = 10)
  hd <- intra_hd(mat, ref_sample = 1)
  expect_length(hd, 10)
  expect_equal(mean(hd), 0)
})

test_that("intra_hd works on a 3D matrix", {
  arr <- array(0, dim = c(5, 10, 7))
  hds <- intra_hd(arr)
  expect_equal(length(dim(hds)), 2)
  expect_equal(dim(hds)[1], nrow(arr))
  expect_equal(dim(hds)[2], ncol(arr))
  expect_equal(mean(hds), 0)
})

test_that("intra_hd throw an error with an invalid ref_sample", {
  mat <- matrix(0, nrow = 5, ncol = 10)
  expect_error(intra_hd(mat, ref_sample = 10))
})

test_that("intra_hd throw an error with an invalid input", {
  expect_error(intra_hd(list(a = 1, b = 2)))
})

test_that("uniqueness works", {
  mat <- matrix(rbits(500), nrow = 5, ncol = 100)
  hd <- uniqueness(mat)
  npairs <- gamma(nrow(mat) + 1) / (2 * gamma(nrow(mat) - 1))
  expect_length(hd, npairs)
  expect_lt((mean(hd) - 0.5), 0.1)
})
