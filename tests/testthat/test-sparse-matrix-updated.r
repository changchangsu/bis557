library(testthat)

context('Test the sparse matrix functions')

# If no dimensions are given, assume the largest coordinates are the
# dimensions.
sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1))
sm1 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
sm2 <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3),
                     dims = c(2, 3))
sm3 <- sparse.matrix(i = rep(1, 3), j = 1:3, x = 1:3, dims = c(2, 3))
test_that("We have sparse.matrix objects.", {
  expect_is(sm0, "sparse.matrix")
  expect_is(sm1, "sparse.matrix")
  expect_is(sm2, "sparse.matrix")
})

test_that("We can add sparse matrices.", {
  expect_equal(sm0 + sm0, 
               sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(2, 2), 
                             dims = c(2, 1)))
  expect_equal(sm2 + sm3,
               sparse.matrix(i = c(rep(1:2, 2), 1), j = c(1, 1, 2, 2, 3),
                             x = c(5.4, 1.2, 2, 3, 3)))

  expect_error(sm2 + sm0)
})

test_that("We can multiply and transpose", {
  expect_equal(sm1 %*% sm3,
               sparse.matrix(i = rep(1:2, 3), j = rep(1:3, each = 2),
                             x = c(3, 1, 6, 2, 9, 3), dims = c(3, 3)))
  expect_error(sm1 %*% sm1)
  expect_equal(t(sm1) %*% sm1, 
               sparse.matrix(i = 1, j = 1, x = 10, dims = c(2, 2)))
})

