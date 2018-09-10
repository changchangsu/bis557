[![Build Status](https://travis-ci.org/changchangsu/bis557.svg?branch=master)](https://travis-ci.org/changchangsu/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

So far the only thing we've done is create and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```
