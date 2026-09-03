test_that("lab.mclust identifies two mixture components", {
  set.seed(123)

  values <- c(
    rnorm(120, mean = 20, sd = 2),
    rnorm(120, mean = 40, sd = 3)
  )

  result <- lab.mclust(
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

test_that("lab.mclust validates its input", {
  expect_error(
    lab.mclust("not numeric"),
    "x must be numeric"
  )

  expect_error(
    lab.mclust(c(NA, -1, 0)),
    "at least two positive finite values"
  )
})

test_that("lab.mclust works with lognormal distribution and proper rounding", {
  set.seed(42)
  vals <- stats::rlnorm(220, meanlog = log(50), sdlog = 0.2)

  res <- lab.mclust(
    vals,
    lognormal = TRUE,
    remove.extremes = FALSE,
    plot.it = FALSE,
    model = "V",
    n.cluster = 1,
    apply.rounding = TRUE
  )

  expect_type(res, "list")
  expect_s3_class(res$stats, "data.frame")
  expect_true(all(c("meanlog", "sdlog") %in% colnames(res$stats)))
  expect_true(res$stats$ll < res$stats$ul)
})