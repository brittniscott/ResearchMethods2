# y = mx + b
# brittsfunction <- b1 * predictor + b0

# function needs to have various values of:
# (1) slope, (2) sample size, (3) error rates


library(tidyverse)


# Q1: Develop a function (based on the above) that will generate, 
# for a range of each of ‘magnitude of B1’, ‘sample size’ and 
# ‘error’, a measure of the quality of the parameter estimate. 
# (Hint, that measure of quality is the t-value – the estimate 
# scaled by the standard error of the estimate).

## need to run it many many times 
predictor <- runif(100) - 0.5 #
B0 <- 1
B1 <- runif(100, 0.5, 3) #
error <- rnorm(100, mean = 0, sd = 1) #
brittsfunction <- B0 + B1 * predictor + error

qplot(predictor, brittsfunction)
