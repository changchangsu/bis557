## ------------------------------------------------------------------------
library(bis557)
fit <- linear_model(Sepal.Length ~ ., iris)
summary(fit)

