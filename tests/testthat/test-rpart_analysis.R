library(testthat)
library(reflimR.expand)

test_that("generate.rpart partitions patients and returns rpart object", {
  set.seed(42)
  n <- 100
  df_dummy <- data.frame(
    Age = sample(18:70, n, replace = TRUE),
    Sex = sample(c("m", "f"), n, replace = TRUE),
    ALT = rnorm(n, mean = 30, sd = 10)
  )

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  fit <- generate.rpart(df_dummy, analyte = "ALT", tree_cp = 0.05, tree_minsplit = 20)
  expect_s3_class(fit, "rpart")
  expect_true(!is.null(fit$frame))
})

test_that("generate.rpart handles stump trees without splits safely", {
  df_constant <- data.frame(
    Age = rep(30, 50),
    Sex = rep("m", 50),
    ALT = rep(25, 50)
  )

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_silent({
    fit_stump <- generate.rpart(df_constant, analyte = "ALT", tree_cp = 0.99, tree_minsplit = 100)
  })
  expect_s3_class(fit_stump, "rpart")
  expect_equal(nrow(fit_stump$frame), 1L)
})

test_that("generate.rpart validates missing inputs and bad types", {
  expect_error(generate.rpart(list(a = 1), analyte = "ALT"), "'df' muss ein data.frame sein")

  df_missing <- data.frame(Age = 20:25, Sex = c("m", "f", "m", "f", "m", "f"))
  expect_error(generate.rpart(df_missing, analyte = "ALT"), "Fehlende Spalten im Datensatz: ALT")
})