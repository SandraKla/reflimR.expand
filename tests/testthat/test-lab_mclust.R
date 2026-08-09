test_that("lab_mclust identifies two mixture components", {
  set.seed(123)

  values <- c(
    rnorm(120, mean = 20, sd = 2),
    rnorm(120, mean = 40, sd = 3)
  )

  result <- lab_mclust(
    values,
    lognormal = FALSE,
    remove.extremes = FALSE,
    plot.it = FALSE,
    model = "V",
    n.cluster = 2,
    apply.rounding = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("n.cluster", "stats", "y.max", "BIC"))
  expect_s3_class(result$stats, "data.frame")
  expect_equal(nrow(result$stats), 2)
  expect_named(result$stats, c("ll", "ul", "percent", "mean", "sd"))
  expect_true(all(result$stats$ll < result$stats$ul))
  expect_equal(sum(result$stats$percent), 100, tolerance = 0.2)
  expect_null(result$y.max)
})

test_that("lab_mclust validates its input", {
  expect_error(
    lab_mclust("not numeric"),
    "x must be numeric"
  )

  expect_error(
    lab_mclust(c(NA, -1, 0)),
    "at least two positive finite values"
  )
})
