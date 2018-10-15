[![Build Status](https://travis-ci.org/changchangsu/bis557.svg?branch=master)](https://travis-ci.org/changchangsu/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

The first thing we've done is create and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

Our second work is the ridge_reg function. You can use it like:

```{r}
library(bis557)
data(ridge_train)
ridge_train_scale <- as.data.frame(scale(ridge_train))
fit <- ridge_reg(y ~. - 1, 0.01, ridge_train_scale)
summary(fit)
```

