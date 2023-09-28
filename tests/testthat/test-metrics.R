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

test_that("hamming_weight handles `NA`", {
  v <- sample(c(0, 1, NA), 1000, replace = TRUE)
  unif <- hamming_weight(v, norm = TRUE)
  expect_lt(abs(unif - 0.5), 0.1)
})

test_that("uniformity of a bit vector", {
  v <- rbits(1000)
  unif <- uniformity(v)
  expect_lt(abs(unif - 0.5), 0.1)
})

test_that("uniformity of a crp table", {
  mat <- rbits(c(10, 500))
  bitalias <- mean(uniformity(mat))
  expect_lt(abs(bitalias - 0.5), 0.1)
})

test_that("bitaliasing of a crp table", {
  mat <- mat <- rbits(c(10, 500))
  bitalias <- mean(bitaliasing(mat))
  expect_lt(abs(bitalias - 0.5), 0.1)
})

test_that("intra_hd works on a 2D matrix", {
  mat <- mat <- rbits(c(5, 10), p = rep(0, 50))
  hd <- intra_hd(mat, ref = 1)
  expect_length(hd, (nrow(mat) - 1) * ncol(mat))
  expect_equal(mean(hd), 1)
})

test_that("intra_hd works on a 3D matrix", {
  arr <- array(0, dim = c(5, 10, 7))
  hds <- intra_hd(arr)
  expect_equal(length(dim(hds)), 2)
  expect_equal(dim(hds)[1], nrow(arr))
  expect_equal(dim(hds)[2], ncol(arr))
  expect_equal(mean(hds), 1)
})

test_that("intra_hd throw an error with an invalid ref_sample", {
  mat <- matrix(0, nrow = 5, ncol = 10)
  expect_error(intra_hd(mat, ref = 10))
})

test_that("intra_hd throw an error with an invalid input", {
  expect_error(intra_hd(list(a = 1, b = 2)))
})

test_that("uniqueness works", {
  mat <- rbits(c(5, 100))
  hd <- uniqueness(mat)
  npairs <- (nrow(mat) * (nrow(mat) - 1)) / 2
  expect_length(hd, npairs)
  expect_lt((mean(hd) - 0.5), 0.1)
})