#' Derivative of the mean absolute deviation (MAE) function.
#'
#' @description Compute derivative of the mean absolute deviation (MAE) function
#' @param y a numeric vector of responses
#' @param a a numeric vector of predicted responses
#' @return the current derivative of MAE
#' @export
#' 

casl_util_mae_p <-
  function(y, a)
  {
    if (y>a) d = -1
    if (y<a) d = 1
    if (y==a) d = 0
    return(d)
  }

#' Derivative of the mean squared error (MSE) function.
#'
#' @description Compute derivative of the mean squared error (MSE) function
#' @param y a numeric vector of responses
#' @param a a numeric vector of predicted responses
#' @return the current derivative of MSE
#' @export
#' 
casl_util_mse_p <- 
  function(y, a)
  {
    return (a-y)
  }


#' Initialize list of weights to describe a dense neural network.
#'
#' @description Create list of weights to describe a dense neural network.
#' @param sizes A vector giving the size of each layer, including the input and output layer
#' @return A list containing initialized weights and biases.
#' @export
#' 

casl_nn_make_weights <-
function(sizes)
{
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L]),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                         b=rnorm(sizes[j + 1L]))
  }
  weights }


#' Apply a rectified linear unit (ReLU) to a vector/matrix.
#'
#' @description Apply a rectified linear unit (ReLU) to a vector/matrix
#' @param v A numeric vector or matrix
#' @return The original input with negative values truncated to zero
#' @export
#' 

casl_util_ReLU <-
  function(v)
  {
    v[v < 0] <- 0
    v }

#' Apply derivative of the rectified linear unit (ReLU).
#'
#' @description Apply derivative of the rectified linear unit (ReLU)
#' @param v A numeric vector or matrix
#' @return Sets positive values to 1 and negative values to zero.
#' @export
#' 

casl_util_ReLU_p <-
  function(v)
  {
    p <- v * 0
    p[v > 0] <- 1
    p
  }

#' Apply forward propagation to a set of NN weights and biases.
#'
#' @description Apply forward propagation to a set of NN weights and biases
#' @param x A numeric vector representing one row of the input
#' @param weights A list created by casl_nn_make_weights
#' @param sigma The activation function.
#' @return A list containing the new weighted responses (z) and activations (a).
#' @export
#' 

casl_nn_forward_prop <-
  function(x, weights, sigma)
  {
    L <- length(weights)
    z <- vector("list", L)
    a <- vector("list", L)
    for (j in seq_len(L))
    {
      a_j1 <- if(j == 1) x else a[[j - 1L]]
      z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
      a[[j]] <- if (j != L) sigma(z[[j]]) else z[[j]]
    }
    list(z=z, a=a)
  }

#' Apply backward propagation.
#'
#' @description Apply backward propagation algorithm
#' @param x A numeric vector representing one row of the input
#' @param y A numeric vector representing one row of the response.
#' @param weights A list created by casl_nn_make_weights.
#' @param f_obj Output of the function casl_nn_forward_prop
#' @param sigma_p Derivative of the activation function
#' @param f_p Derivative of the loss function
#' @return A list containing the new weighted responses (z) and activations (a).
#' @export
#' 

casl_nn_backward_prop <-
  function(x, y, weights, f_obj, sigma_p, f_p)
  {
    z <- f_obj$z; a <- f_obj$a
    L <- length(weights)
    grad_z <- vector("list", L)
    grad_w <- vector("list", L)
    for (j in rev(seq_len(L)))
    {
      if (j == L) {
        grad_z[[j]] <- f_p(y, a[[j]])
      } else {
        grad_z[[j]] <- (t(weights[[j + 1]]$w) %*%
                          grad_z[[j + 1]]) * sigma_p(z[[j]])
      }
      a_j1 <- if(j == 1) x else a[[j - 1L]]
      grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
    }
    list(grad_z=grad_z, grad_w=grad_w)
  }

#' Apply backward propagation.
#'
#' @description Apply stochastic gradient descent (SGD) to estimate NN.
#' @param X A numeric data matrix
#' @param y A numeric vector of responses
#' @param sizes A numeric vector giving the sizes of layers in the neural network
#' @param epochs Interger number of epochs for the algorithm to iterative for
#' @param eta Positive numeric learning rate
#' @param weights Optional list of starting weights
#' @param loss specifiy the loss metric to use, mean square error or mean absolute deviation
#' @return A list containing the trained weights for the network
#' @export
#' 

casl_nn_sgd <-
  function(X, y, sizes, epochs, eta, weights=NULL, loss = "mse")
  {
    if (is.null(weights))
    {
      weights <- casl_nn_make_weights(sizes)
    }
    for (epoch in seq_len(epochs))
    {
      for (i in seq_len(nrow(X)))
      {
        f_obj <- casl_nn_forward_prop(X[i,], weights,
                                      casl_util_ReLU)
        if (loss == "mse"){
          b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
                                       f_obj, casl_util_ReLU_p,
                                       casl_util_mse_p)
        }
        if(loss == "mae"){
          b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
                                         f_obj, casl_util_ReLU_p,
                                         casl_util_mae_p)
        }
        for (j in seq_along(b_obj))
        {
          weights[[j]]$b <- weights[[j]]$b -
            eta * b_obj$grad_z[[j]]
          weights[[j]]$w <- weights[[j]]$w -
            eta * b_obj$grad_w[[j]]
        }
      } }
    weights }

#' Predict values from a training neural network.
#'
#' @description Predict values from a training neural network
#' @param weights List of weights describing the neural network.
#' @param X_test A numeric data matrix for the predictions.
#' @return A matrix of predicted values.
#' @export
#' 

casl_nn_predict <-
  function(weights, X_test)
  {
    p <- length(weights[[length(weights)]]$b)
    y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
    for (i in seq_len(nrow(X_test)))
    {
      a <- casl_nn_forward_prop(X_test[i,], weights,
                                casl_util_ReLU)$a
      y_hat[i, ] <- a[[length(a)]]
    }
    y_hat }