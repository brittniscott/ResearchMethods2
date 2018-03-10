library(tidyverse)


# QUESTION 1-3


# This is the function we will use to geneterate a dataframe for x and y values
gen.df <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1)  {
  predictor <- seq(-1, 1, length.out = n)
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

# We then need a dataframe containing the various values of the parameters we wish to apply to our function
n.times <- 1000
rep <- 1:n.times
n <- c(10, 100, 1000)
b1 <- c(0.1, 1, 2.5, 5, 10, 20)
sd.err <- c(0.1, 1, 2, 5)
test.df <- expand.grid(n.times = n.times, rep = rep, n = n, b1 = b1, sd.err = sd.err)

# First creates an empty matrix the length of test.df
# Then, applies the parameter values within test.df to the function gen.df
# Creates tmp.df, a dataframe of x and y values calculated using the various paraemter values
# Next, it summarizes tmp.df fit to a linear model and finds the summary of coefficient values throughout the model, adds est column in test.df
# Also summarizes t values for x and yvales for a linear model into a column called tval
for (i in 1:length(test.df$rep)) {
  tmp.df <- 
    gen.df(n = test.df$n[i], 
           b0 = 2, 
           b1 = test.df$b1[i], 
           sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  test.df$est[i] <- summary(tmp.lm)$coefficients["predictor", "Estimate"]
  test.df$tval[i] <- summary(tmp.lm)$coefficients["predictor", "t value"]
}

# Turns values to factors in test.df
test.df$b1 <- factor(test.df$b1)
test.df$n <- factor(test.df$n)
test.df$sd.err <- factor(test.df$sd.err)

# Creates a density graph of coefficient estimates for test.df
p <- ggplot(data = test.df, aes(est, group = b1, col = b1))
p + geom_density()

# Graph of how t-values are affected by the parameters
test.sum <- 
  group_by(test.df, n, sd.err, b1) %>%
  summarise(gm = exp(mean(log(abs(tval)))))

p <- ggplot(data = test.sum, aes(b1, gm, col = n, group = n))
p + geom_point() + geom_line() + facet_grid(sd.err ~ .) + scale_y_log10()



# QUESTION 4


gen.df4 <- function(n = 100, b0 = 0, b1 = 0, sd.err = 1)  {
  predictor <- seq(-1, 1, length.out = n)
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

n.times <- 5000
rep <- 1:n.times
n <- c(10, 100, 1000)
sd.err <- c(0.1, 1, 2, 5)
test.df4 <- expand.grid(n.times = n.times, n = n, rep = rep, sd.err = sd.err)


for (i in 1:length(test.df4$rep)) {
  tmp.df4 <- gen.df4(sd.err = test.df4$sd.err[i],
                     n = test.df4$n[i])
  tmp.lm4 <- lm(response ~ predictor, data = tmp.df4)
  test.df4$pval[i] <- summary(tmp.lm4)$coefficients["predictor", "Pr(>|t|)"]
}

test.df4$sd.err <- factor(test.df4$sd.err)

p <- ggplot(data = test.df4, aes(pval))
p + geom_density() + facet_grid(n ~ sd.err)

group_by(test.df4, sd.err, n) %>% 
  summarise(sum.tab = table(pval <= 0.05)[2]/length(pval))
