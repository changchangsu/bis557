#' Fit a Ridge Regression model with given lambda
#'
#' @description This function use SVD decomposition to solve ridge regression.
#' @param form formula of the regression model, where intercept term is dropped
#' @param d a data.frame, with each column scaled to have mean 0 and unit vairance
#' @param lambda tuning parameter of ridge regression, controlling panalty
#' @return An Ridge object
#' @importFrom stats model.matrix
#' @examples
#' data(ridge_train)
#' ridge_train_scale <- as.data.frame(scale(ridge_train))
#' fit <- ridge_reg(y ~. - 1, 0.01, ridge_train_scale)
#' summary(fit)
#' @export
#' 

ridge_reg <- function(form, lambda, d){
  
  rownames(d) <- NULL 
  # m <- model.matrix(form, d)
  m <- model.matrix(form, d)
  y <- matrix(d[,as.character(form)[2]], ncol=1)
  y <- y[as.numeric(rownames(m)), , drop=F] 
  n <- nrow(m)
  p<- ncol(m)
  
  # # Remove the intercept by centering variables
  # mean_x <- colMeans(m)
  # mean_y <- mean(y)
  # m <- m - matrix(rep(mean_x, n), nrow = n, byrow = T)
  # y <- y - mean_y
  # 
  # # Rescale the predictors to make the penalty 'fair'
  # scaling <- sqrt(colMeans(m^2))
  # m <- m / matrix(rep(scaling, n), nrow = n, byrow = T)
  
  # Fit via svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  
  # # Transform the beta back to the orignial scale of the data
  # beta <- beta / scaling
  # beta <- rbind(mean_y - beta * matrix(mean_x, ncol=1), beta)
  # rownames(beta)[1] <- 'Intercept'
  
  ret <- list(coef = beta, lambda = lambda)
  class(ret) <- "ridge_reg"
  ret
}
