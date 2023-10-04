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

test_that("uniformity of a non biased vector is 0.5", {
  v <- rbits(1000)
  unif_h <- entropy_p(uniformity(v))
  expect_gt(unif_h, 0.9)
})

test_that("uniformity only works on vectors and matrices", {
  v <- rbits(100)
  expect_no_error(uniformity(v))
  m <- rbits(c(5, 10))
  expect_no_error(uniformity(m))
  arr <- rbits(c(5, 10, 3))
  expect_error(uniformity(arr))
})

test_that("bitaliasing of a non biased vector is 0.5", {
  v <- rbits(1000)
  unif_h <- entropy_p(uniformity(v))
  expect_gt(unif_h, 0.9)
})

test_that("bitaliasing only works on matrices", {
  v <- rbits(10)
  expect_error(bitaliasing(v))
  m <- rbits(c(5, 10))
  expect_no_error(bitaliasing(m))
  arr <- rbits(c(5, 10, 3))
  expect_error(bitaliasing(arr))
})

test_that("intra_hd works on vectors", {
  v <- rbits(10)
  ihd <- intra_hd(v, 1)
  expect_true(is.vector(ihd))
  expect_equal(length(ihd), length(v) - 1)
})

test_that("intra_hd works on matrices", {
  m <- rbits(c(5, 10))
  ihd <- intra_hd(m, 1)
  expect_true(is.matrix(ihd))
  expect_equal(dim(ihd), c(4, 10))
})

test_that("intra_hd works on arrays", {
  arr <- rbits(c(3, 10, 5))
  ihd <- intra_hd(arr, 1)
  expect_true(is.matrix(ihd))
  expect_equal(dim(ihd), c(3, 10))
})

test_that("intra_hd fails when providing a wrong reference", {
  v <- rbits(10)
  expect_error(intra_hd(v, 12))
  m <- rbits(c(5, 10))
  expect_error(intra_hd(m, -2))
  arr <- rbits(c(5, 10, 3))
  expect_error(intra_hd(arr, 20))
})

test_that("uniqueness provides all results", {
  m <- rbits(c(5, 10))
  size <- (nrow(m) * (nrow(m) - 1)) / 2
  uniq <- uniqueness(m)
  expect_equal(length(uniq), size)
  expect_lt((mean(uniq) - 0.5), 0.1)
})

test_that("compare_pairwise behaves like uniqueness", {
  m <- rbits(c(5, 10))
  fn <- function(x, y) 1 - hamming_dist(x, y, TRUE)
  expect_true(all(uniqueness(m) == compare_pairwise(m, fn)))
})
