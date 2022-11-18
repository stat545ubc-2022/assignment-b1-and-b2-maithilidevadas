#Using testthat() and expect_error() for an input other than numeric.
test_that("Return an error for non-numeric data ", {
  expect_error (MeanandMedian(penguins$island))})

#Using testthat() and expect_error we check for vector of length 0.
test_that("Return an error for vector of length 0 ", {
  expect_error (MeanandMedian(penguins$bill_length_mm, numeric(0)))})

test_that("Checking if the function works fine for numeric values", {
  expect_true (is.numeric(MeanandMedian(penguins$flipper_length_mm)))})




