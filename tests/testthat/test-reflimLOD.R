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