## ------------------------------------------------------------------------
# First, construct a dataset, where data is from a mixture of normal distribution
set.seed(1234)
d <- c(rnorm(500,10,1), rnorm(500,0,3))
hist(d)
# Split the data into train and test set (7:3)
train_index <- sample.int(1000,700)
dtrain <- d[train_index]
dtest <- d[!(1:1000 %in% train_index)]

## ---- fig.width=8, fig.height=6------------------------------------------
# Calculate the kernel density estimation for a bunch of h
hs <- seq(0.1,3.1,by=1)
kernel_estimates <- sapply(hs, function(h){
  kern_density(dtrain, dtest, h)
})
dtest_df <- data.frame(kern = as.vector(kernel_estimates),
                       bandwidth = rep(hs, each = length(dtest)),
                       data = dtest)
# Visualize the density estiamted by Epanechnikov kernel with 4 different bandwidths.
library(ggplot2)
ggplot(dtest_df) + geom_line(aes(x = data, y = kern, color = as.factor(bandwidth))) + scale_color_discrete(name = "Bandwidth") +
  labs(y = "Density Estimate", x = "Test Data", title = "Kernel Density Estimates for Different Choices of Bandwidths") +
  theme(legend.position="bottom", title = element_text(size=14), legend.text=element_text(size=12))

## ----message=FALSE-------------------------------------------------------
library(glmnet)
# Generate a dataset with n<p and with only few variables actually contributing to the prediction
# This idea credit to https://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
set.seed(1234)
n <- 100
p <- 200
p_true <- 10
x <- matrix(rnorm(n*p), nrow = n, ncol = p)
y <- apply(x[,1:p_true], 1, sum) + rnorm(n)

## ------------------------------------------------------------------------
# Try two similar penalty parameters and fit LASSO with glmnet
lambdas <- c(0.1, 0.11)
LASSO_fit <- glmnet(x, y, family="gaussian", alpha=1, lambda = lambdas)

kkt_status = check_kkt(x, y, as.vector(LASSO_fit$beta[,2]), lambdas[1])
# The predictor checked to be inactive would stay inactive.
all(LASSO_fit$beta[,1][kkt_status$stay_inactive] == 0)
# Indeed, active set screening save a lot of computation cost
sum(kkt_status$stay_inactive)

