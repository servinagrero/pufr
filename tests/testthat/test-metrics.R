test_that("metrics has the correct dimensions", {
  m <- rbits(c(5, 10))
  expect_equal(dim(metrics(m)), c(5, 10, 1))
  arr <- rbits(c(5, 10, 3))
  expect_equal(dim(metrics(arr)), c(5, 10, 3))
})


test_that("reliability is NA when a matrix is supplied", {
  arr <- rbits(c(5, 10, 3))
  met <- metrics(arr[,,1])
  expect_true(is.na(met$reliability))
  met <- metrics(arr)
  expect_true(is.matrix(met$reliability))
})

test_that("with_entropy modifies uniformity and bitaliasing", {
  m <- rbits(c(5, 10), p = 1)
  met <- metrics(m)
  expect_true(all(met$uniformity == 1))
  expect_true(all(met$bitaliasing == 1))

  met_ent <- with_entropy(met)
  expect_true(all(met_ent$uniformity == 0))
  expect_true(all(met_ent$bitaliasing == 0))
})
