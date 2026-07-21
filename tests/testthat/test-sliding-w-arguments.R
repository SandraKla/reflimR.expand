test_that("sliding.w.reflim forwards arguments in normal mode", {
  captured <- new.env(parent = emptyenv()) #Container named captured create

  local_mocked_bindings(
    w.sliding.reflim = function(...) {
      captured$arguments <- list(...)

      data.frame(
        lower.lim = 1,
        upper.lim = 2
      )
    },
    draw.sliding.w.reflims = function(...) NULL,
    .package = "reflimR.expand"
  )

  sliding.w.reflim(
    x = 1,
    t = 1,
    perc.trunc = 3,
    n.min = 80,
    n.min.window = 150,
    apply.rounding = TRUE,
    verbose = FALSE
  )

  expect_equal(captured$arguments$perc.trunc, 3)
  expect_equal(captured$arguments$n.min, 80)
  expect_equal(captured$arguments$n.min.window, 150)
  expect_true(captured$arguments$apply.rounding)
})


test_that("comparison mode forwards arguments to both calculations", {
  captured <- new.env(parent = emptyenv())
  captured$calls <- list()

  local_mocked_bindings(
    w.sliding.reflim = function(...) {
      captured$calls[[length(captured$calls) + 1]] <- list(...)

      data.frame(
        lower.lim = 1,
        upper.lim = 2
      )
    },
    draw.sliding.w.reflims.compare = function(...) NULL,
    .package = "reflimR.expand"
  )

  sliding.w.reflim(
    x = 1,
    t = 1,
    standard_deviation_compare = 10,
    perc.trunc = 3,
    n.min = 80,
    n.min.window = 150,
    apply.rounding = TRUE,
    verbose = FALSE
  )

  expect_length(captured$calls, 2)

  for (arguments in captured$calls) {
    expect_equal(arguments$perc.trunc, 3)
    expect_equal(arguments$n.min, 80)
    expect_equal(arguments$n.min.window, 150)
    expect_true(arguments$apply.rounding)
  }
})
