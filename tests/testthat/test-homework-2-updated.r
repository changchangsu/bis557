library(testthat)

context("Test the output of homework 2.")

test_that("Ridge regression works.", {
  data(ridge_train)
  lambda <- 1.2121212
  ridge_train_scale <- as.data.frame(scale(ridge_train))
  fit_ridge <- ridge_reg(y ~. - 1, lambda, ridge_train_scale)
  expect_equivalent(fit_ridge$coef, 
    c(0.30854117, -0.27991787,  0.04860966, 0.03427351), tolerance = 1e-5)
})
