library(testthat)

test_that("reflim_Sliding functions handle invalid inputs", {
  dataset_false <- "A"

  expect_error(sliding.reflim(dataset_false))
  expect_error(count.n.per.window(dataset_false))
  expect_error(draw.sliding.reflims(dataset_false))
})