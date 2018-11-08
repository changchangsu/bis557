
#' Check KKT
#'
#' @description Check KKT condition for the beta for some iteration
#' @param x design matrix
#' @param y response vector
#' @param beta regression coefficients fitted in the last iteration
#' @param lambda penalty parameter 
#' @return a logical vector indicating where KKT the condition is violated 
#' by the zero-valued coeffcients.
#' @export
#' 

check_kkt <- function(x, y, beta, lambda){
  
  n <- nrow(x)
  resid <- y - x %*% beta
  s <- 1/n * t(x) %*% resid * 1/lambda
  
  return (list(stay_inactive = (beta == 0) & abs(s) < 1,
               add_to_active = (beta == 0) & abs(s) >= 1))
  
}