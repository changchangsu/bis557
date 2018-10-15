
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
  
  # Create data
  X_design <- model.matrix(formula, data)
  y <- unlist(data[all.vars(formula)[1]])

  # Solve beta via QR decomposition
  beta_hat <- qr.solve(X_design, y)
  beta_hat[beta_hat==0] <- NA

  # Create a lm object 
  lm_list <- lm(formula,data)
  lm_list$coefficients <- beta_hat
  # lm_list <- list()
  # lm_list$coefficients <- beta_hat
  # lm_list$residuals <- y-X_design %*% beta_hat  
  # lm_list$fitted.values <- X_design %*% beta_hat
  # lm_list$rank <- qr(X_design)$rank
  # lm_list$weights <- NULL
  # lm_list$df.residual <- nrow(X_design) - qr(X_design)$rank
  # lm_list$call <- "Self Written lm function"
  # lm_list$terms <- formula
  # lm_list$contrasts <- NULL
  # lm_list$xlevels <- NULL
  # lm_list$offset <- NULL
  # lm_list$y <- y
  # lm_list$x <- X_design
  # lm_list$model <- NULL
  # lm_list$na.action <- NULL
  # lm_list$qr <- qr(X_design)
  # class(lm_list) <- "lm"
  return (lm_list)
}


# linear_model <- function(formula, data) {
#   X_design <- model.matrix(formula, data)
#   svd_object = svd(X_design)
#   U <- svd_object[["u"]]
#   V <- svd_object[["v"]]
#   Sinv <- diag(1 / svd_object[["d"]])
#   y <- unlist(data[all.vars(formula)[1]])
#   pseudo_inv <- V %*% Sinv %*% t(U)
#   beta_hat <- pseudo_inv %*% y
#   lm_list <- lm(formula,data)
#   lm_list$coefficients <- beta_hat
#   return (lm_list)
# }
