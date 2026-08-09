test_that("zlog standardizes values using reference limits", {
  expect_equal(
    zlog(c(2, 4, 8), L = 2, U = 8),
    c(-1.959964, 0, 1.959964),
    tolerance = 1e-6
  )
})

test_that("zlog returns NA for invalid measurements or limits", {
  expect_equal(
    zlog(c(-1, 0, NA, Inf), L = 2, U = 8),
    rep(NA_real_, 4)
  )

  expect_equal(
    zlog(c(2, 4), L = 0, U = 8),
    rep(NA_real_, 2)
  )

  expect_equal(
    zlog(c(2, 4), L = 8, U = 2),
    rep(NA_real_, 2)
  )
})

test_that("zlog rejects non-numeric measurements", {
  expect_error(
    zlog("4", L = 2, U = 8),
    "x must be numeric"
  )
})
