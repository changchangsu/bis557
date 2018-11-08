#' Kernel Density Estimation
#'
#' @description This function is used for calulating the kernel density estimatior.
#' @param x training vector
#' @param h bandwidth for the Epanechnikov kernel
#' @param x_new test vector
#' @return Kernel density estimation for test data.
#' @examples
#' data(ridge_train)
#' ridge_train_scale <- as.data.frame(scale(ridge_train))
#' fit <- ridge_reg(y ~. - 1, 0.01, ridge_train_scale)
#' summary(fit)
#' @export
#' 

kern_density <- function(x, x_new, h=1){
  
  n <- length(x)
  sapply(x_new, function(new){
    1/(n*h) * sum(epan_kernel((x-new)/h, h))
  })
  
}

#' Evaluate the Epanechnikov kernel function
#'
#' @description This function is used to evaluate Epanechnikov kernel function for given data.
#' @param x data upon which Epanechnikov kernel should be evaluated
#' @param h bandwidth for the Epanechnikov kernel
#' @return a vector of values with same length as x
#' @export
#' 
epan_kernel <- function(x, h=1){
  u <- ifelse(abs(x)<h,1,0)
  val <- 1/h * 3/4 * (1-(x/h)^2) * u
  val
}