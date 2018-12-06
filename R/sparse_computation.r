#' Generate a sparse.matrix object from regular matrix
#'
#' @description Generate a sparse.matrix object from regular matrix
#' @param i the row coordinate of the entry
#' @param j the column coordinate of the entry
#' @param x the value at the entry
#' @param dims dimension of the matrix (if not specified, inferred from the largest i and j.)
#' @return a 3 column (i:ith coordinate, j:jth coordinate, x:value) representation of the matrix
#' @export
#' 

sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  a = matrix(c(i,j,x), nrow = length(i))
  colnames(a) <- c("i", "j", "x")
  class(a) <- append("sparse.matrix", class(a))
  attributes(a)$true_dims = dims
  return (a)
}

#' Convert a data.frame object into a sparse.matrix object
#'
#' @description Convert a data frame into a sparse.matrix object (because merge() is used in matrix algebra functions, 
#' which coerce matrices into dataframes)
#' @param df a data frame in the specific form: with (i,j,x) columns
#' @param dims the dimension of the underlying dense matrix
#' @return a sparse.matrix object
#' @export
#' 

to_sparse_matrix <- function(df, dims){
  a <- matrix(c(df$i, df$j, df$x), nrow = nrow(df), byrow = F)
  colnames(a) <- c("i", "j", "x")
  class(a) <- append("sparse.matrix", class(a))
  attributes(a)$true_dims = dims
  return (a)
}


#' Add two sparse matrices
#'
#' @description Add two sparse.matrix objects
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return the sum of a and b in the sparse.matrix form.
#' @export
#' 
sparse_add <- function(a, b) {
  if (any(attributes(a)$true_dim != attributes(b)$true_dim)) stop("error message")
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  out.df <- c[, c("i", "j", "x")][order(c$j),]
  to_sparse_matrix(out.df, 
                   dims = c(max(c[,1]), max(c[,2])))
}


#' Multiply two sparse matrices
#'
#' @description Multiply two sparse.matrix objects
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return the product of a and b in the sparse.matrix form
#' @export
#' 

sparse_multiply <- function(a, b){
  if (any(attributes(a)$true_dim[2] != attributes(b)$true_dim[1])) stop("error message")
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
  i = as.numeric(sapply(key, getElement, 1))
  d <- sparse.matrix(i = i[j_order], 
                     j = j[j_order], 
                     x = as.numeric(x)[j_order], 
                     dims = c(attributes(a)$true_dim[1], attributes(b)$true_dim[2]))
  # attributes(a)$true_dims[1], attributes(b)$true_dims[2]
  return (d)
}

#' Transpose a sparse matricx
#'
#' @description transpose a sparse matrix
#' @param a a sparse.matrix object
#' @return the trainsposed form of the sparse matrix
#' @export
#' 

sparse_transpose <- function(a){
  t <- a[,1]
  a[,1] <- a[,2]
  a[,2] <- t
  attributes(a)$true_dims <- attributes(a)$true_dims[c(2,1)]
  return (a)
}

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
