library(testthat)

context("Test the output of homework 1.")

test_that("The updated version of linear_model works.", {
  data(lm_patho)
  fit_linear_model <- linear_model(y ~., lm_patho)
  fit_lm <- lm(y ~., lm_patho)
  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients)
})
