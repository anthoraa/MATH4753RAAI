test_that("mu works", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expect_equal(result$mu, 0)
})

test_that("sigma works", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expect_equal(result$sigma, 1)
})

test_that("a works", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expect_equal(result$a, 2)
})
