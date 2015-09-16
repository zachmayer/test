context("Test Imputation")

test_that("median imputation works", {
  x <- matrix(runif(10), ncol=2)
  x[4,1] <- NA
  x[5,2] <- NA
  expect_error(medianImpute(x))
  x <- as.data.frame(x)
  expect_error(medianImpute(x[1,]), 'x must have at least 2 rows for imputation')
  expect_is(medianImpute(x), 'data.frame')
})

test_that('kknn imputation fails', {
  x <- matrix(runif(10), ncol=2)
  x[4,1] <- NA
  x[5,2] <- NA
  x <- as.data.frame(x)
  expect_error(knnImpute(x), 'Not implemented')
})
