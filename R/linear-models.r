
#' Fit a linear model
#'
#' @description This function use QR decomposition to solve OLS.
#' @param formula formula of the regression model
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @importFrom stats model.matrix
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  X_design <- model.matrix(formula, data)
  qr_object = qr(X_design) 
  Q <- qr.Q(qr_object)
  R <- qr.R(qr_object)
  y <- unlist(data[all.vars(formula)[1]])
  Qty <- crossprod(Q, y)
  beta_hat <- backsolve(R, Qty)
  lm_list <- lm(formula,data)
  lm_list$coefficients <- beta_hat
  return (lm_list)
}
