library(testthat)

context("reflimLOD.R")
context("reflim_Sliding.R")
context("reflim_Weighted_Sliding.R")

dataset1  <- reflimR::livertests$ALB
dataset2  <- 5.4321
dataset_false <- "A"

test_that("Checking error messages",{

  # reflim_Sliding
  expect_error(sliding.reflim(dataset_false))
  expect_error(count.n.per.window(dataset_false))
  expect_error(draw.sliding.reflims(dataset_false))

  # reflimLOD
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
  #expect_error(fit.trunc.norm(dataset_false))
  expect_error(lod.artificial.sample(dataset_false))

  # reflim_Weighted_Sliding
  expect_error(sliding.w.reflim(dataset_false))
  expect_error(MLE(dataset_false))
  expect_error(w.reflimLOD.MLE(dataset_false))
  expect_error(w.modTrunc(dataset_false))
  expect_error(draw.sliding.w.reflims(dataset_false))
  expect_error(draw.sliding.w.reflims_compare(dataset_false))
  expect_error(dtriang(dataset_false))
  expect_error(dtrapezoid(dataset_false))
  expect_error(makeWeightFunction(dataset_false))
  expect_error(calculate_weight_threshold(dataset_false))
  expect_error(w.sliding.reflim(dataset_false))
  expect_error(w.sliding.reflim.plot(dataset_false))
  expect_error(w.reflim(dataset_false))
  expect_error(w.bowley(dataset_false))
  expect_error(w.IQR(dataset_false))
  expect_error(w.lognorm(dataset_false))
  expect_error(w.iboxplot(dataset_false))
  expect_error(w.bowley(dataset_false))
  expect_error(w.truncated_qqplot(dataset_false))
})
