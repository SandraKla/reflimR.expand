library(testthat)

test_that("make_data generates valid normal dataset with LOD support", {
  df <- make_data(
    age = 2,
    age_steps = 365,
    distribution = "NO",
    n_ = 20,
    formula_mu = "linear(x, 0, 10)",
    formula_sigma = "linear(x, 0, 1)",
    lod = 9.5,
    seed = 42
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(c("AGE_YEARS", "AGE_DAYS", "VALUE", "IS_BELOW_LOD", "ID", "ANALYTE") %in% colnames(df)))
  expect_type(df$IS_BELOW_LOD, "logical")
})

test_that("generate_data_from_ri works with reference bounds", {
  ref_df <- data.frame(
    age = c(365, 730),
    down = c(2, 2),
    up = c(6, 6)
  )
  df <- generate_data_from_ri(ref_df, n_ = 10, seed = 123)

  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})