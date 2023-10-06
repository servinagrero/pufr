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

test_that("compare_pairwise creates all pairs of rows", {
  m <- rbits(c(5, 10))
  npairs <- (nrow(m) * (nrow(m) - 1)) / 2
  res <- compare_pairwise(m, sum)
  expect_equal(length(res), npairs)
})

test_that("equal_to_idx works properly", {
  x <- rbits(100)
  res <- equal_to_idx(x)
  expect_lt(abs(0.5 - mean(res)), 0.1)

  x <- rep(1, 100)
  expect_true(all(equal_to_idx(x) == 1))

  expect_error(equal_to_idx(rbits(5), 10))
  expect_error(equal_to_idx(rbits(5), -2))
})

test_that("crps_to_df works properly", {
  arr <- rbits(c(5, 20, 3))
  df <- crps_to_df(arr)
  expect_equal(length(unique(df$device)), dim(arr)[1])
  expect_equal(length(unique(df$challenge)), dim(arr)[2])
  expect_equal(length(unique(df$sample)), dim(arr)[3])

  bits <- unlist(lapply(seq_len(dim(arr)[3]), function(s) as.vector(t(arr[, , s]))))
  expect_true(is.data.frame(df))
  expect_equal(df$response, bits)
})

test_that("conversion from df to crps is reversible", {
  arr <- rbits(c(5, 20, 3))
  res <- df_to_crps(crps_to_df(arr))
  expect_equal(dim(res), dim(arr))
  expect_true(all(arr == res))
})
