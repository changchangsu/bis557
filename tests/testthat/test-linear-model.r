library(testthat)

context('Testing linear model functions')

test_that("The linear_model function works.", {
  form <- Sepal.Length ~ .
  data <- iris
  fit_lm <- lm(form, data)
  fit_linear_model <- linear_model(form, data) #fitLinearModel
  expect_is(fit_linear_model, "lm")
  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients)
})

