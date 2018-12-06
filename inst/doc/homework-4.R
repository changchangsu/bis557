## ------------------------------------------------------------------------
n = 10
X_design = matrix(c(-100000, rnorm(3 * n -1)), nrow = n, byrow = T)
X_design

## ------------------------------------------------------------------------
beta = c(1,2,-1)
p = 1/(1+exp(-X_design %*% beta))
W <- diag(as.vector(p * (1-p)))
qr(W)$rank
dim(W)

## ------------------------------------------------------------------------
H <- crossprod(X_design,X_design)
qr(H)$rank 
dim(H)

## ------------------------------------------------------------------------
# Solve generalized linear models with Newton-Ralphson method.
#
# Args:
#     X: A numeric data matrix.
#     y: Response vector.
#     family: Instance of an R ‘family‘ object.
#     maxit: Integer maximum number of iterations.
#     tol: Numeric tolerance parameter.
#
# Returns:
#     Regression vector beta of length ncol(X).
glm_irwls_l2 <-
function(X, y, family, maxit=25, tol=1e-10, lambda=0.01)
{
  beta <- rep(0,ncol(X))
  for(j in seq_len(maxit))
  {
    b_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    mu_p <- family$mu.eta(eta)
    z <- eta + (y - mu) / mu_p
W <- as.numeric(mu_p^2 / family$variance(mu))
XtX <- crossprod(X, diag(W) %*% X)
Xtz <- crossprod(X, W * z)
beta <- solve(XtX, Xtz)
    if(sqrt(crossprod(beta - b_old)) < tol) break
  }
beta }


## ------------------------------------------------------------------------
sparse_add <- function(a, b) {
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
}

a <- data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1))
b <- data.frame(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))
sparse_add(a, b)

## ------------------------------------------------------------------------
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  a = matrix(c(i,j,x), nrow = length(i), byrow = F)
  colnames(a) <- c("i", "j", "x")
  class(a) <- append(class(a),"sparse.matrix")
  attributes(a)$true_dims = dims
  return (a)
}

# a helper function for converting the data frame into a sparse.matrix object
# for more detail, use '?to_sparse_matrix' after loading the package
to_sparse_matrix <- function(df, dims){
  a <- as.matrix(df)
  class(a) <- append(class(a),"sparse.matrix")
  attributes(a)$true_dims = dims
  return (a)
}

## ------------------------------------------------------------------------
sparse_add <- function(a, b) {
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  out.df <- c[, c("i", "j", "x")][order(c$j),]
  to_sparse_matrix(out.df, 
                   dims = c(max(c[,1]), max(c[,2])))
}

sparse_multiply <- function(a, b){
  colnames(b) <- c("i2", "j2", "x2")
  c <- merge(a, b, by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j2, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  # d <- data.frame(i = sapply(key, getElement, 1),
  #                 j = sapply(key, getElement, 2),
  #                 x = as.numeric(x))
  j = as.numeric(sapply(key, getElement, 2))
  j_order <- order(j)
  d <- sparse.matrix(i = as.numeric(sapply(key, getElement, 1))[j_order], 
                     j = j[j_order], 
                     x = as.numeric(x)[j_order], 
                     dims = c(attributes(a)$true_dims[1], attributes(b)$true_dims[2]))
  return (d)
}

sparse_transpose <- function(a){
  t <- a[,1]
  a[,1] <- a[,2]
  a[,2] <- t
  attributes(a)$true_dims <- attributes(a)$true_dims[c(2,1)]
  return (a)
}

## ------------------------------------------------------------------------
# Overload addition / use generic functions
`+.sparse.matrix` = function(e1, e2){ sparse_add(e1,e2)} 

# Overload transpose
t.sparse.matrix = function(x){sparse_transpose(x)}

# Overload matrix multiplication
`%*%.default` = .Primitive("%*%") # assign default as current definition
`%*%` = function(x,...){ #make S3
  UseMethod("%*%",x)
}
`%*%.sparse.matrix` = function(x,y) {sparse_multiply(x,y)} # define for sparse.matrix

## ------------------------------------------------------------------------
a_ori <- matrix(c(3,0,1,0), nrow=2, ncol=2, byrow = T)
b_ori <- matrix(c(4.4,0,1.2,3.0), nrow=2, ncol=2, byrow = T)
a_ori %*% b_ori

a <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1))
b <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))

a + b
a_ori + b_ori

a %*% b
a_ori %*% b_ori

a
t(a)

