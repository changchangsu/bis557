## ------------------------------------------------------------------------
library(bis557)
library(ggplot2)
data(ridge_train)
ridge_train_scale <- as.data.frame(scale(ridge_train))
lambda = sapply(seq(0,4, by = 0.1),function(x) exp(x)-1)
ridge_reg_coefs <- lapply(lambda, function(x) ridge_reg(y ~. - 1, x, ridge_train_scale)$coef)
data(ridge_test)
ridge_test_matrix <- matrix(unlist(ridge_test), ncol = 5, byrow = F)
ridge_test_matrix <- scale(ridge_test_matrix)



O_MSE <- sapply(ridge_reg_coefs, function(x) 
  sum((ridge_test_matrix[,2:5]%*% x - ridge_test_matrix[,1])^2))
plot_df <- data.frame(MSE = O_MSE, lambda = lambda)

ggplot(data = plot_df, aes(x = lambda, y = MSE)) +
  geom_point() +
  geom_line() +
  labs(y = 'out of sample sum of squares', title = 'ridge trace plot') + 
  theme(plot.title = element_text(hjust = 0.5))

