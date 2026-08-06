library(testthat)

test_that("reflimLOD functions handle invalid inputs", {
  dataset_false <- "A"

  expect_error(box.cox.trans(dataset_false))
  expect_error(box.cox.inv.trans(dataset_false))
  expect_error(d.box.cox.trans(dataset_false))
  expect_error(d.box.cox.inv.trans(dataset_false))
  expect_error(dens.bcinv.bc(dataset_false))
  expect_error(dens.box.cox.inv(dataset_false))
  expect_error(modTrunc(dataset_false))
  expect_error(reflimLOD.MLE(dataset_false))
  expect_error(reflimLOD.Quant(dataset_false))
  expect_error(ci.reflimLOD.MLE(dataset_false))
  expect_error(compute.r.squared(dataset_false))
  expect_error(lod.qqplot(dataset_false))
  expect_error(plot.r.squared(dataset_false))
  expect_error(plot.reflims(dataset_false))
  expect_error(lod.hist(dataset_false))
  expect_error(lod.artificial.sample(dataset_false))
})
test_that("lod.hist draws the fitted density curve without an error", {
  set.seed(123)

  measured <- 5 + rlnorm(
    200,
    meanlog = log(5),
    sdlog = 0.2
  )

  fit <- reflimLOD.MLE(
    measured.values = measured,
    lod = 5,
    n.lod = 10,
    lambda = 0
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit({
    grDevices::dev.off()
    unlink(plot_file)
  }, add = TRUE)

  expect_error(
    lod.hist(
      fit,
      main = "LOD-adjusted distribution"
    ),
    NA
  )
})
