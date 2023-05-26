library(psych) # for descriptives
library(effsize) # for Cohen's d
library(BayesFactor) # to compute Bayes factors


# ----
# Analysis
# input = a vector
# works well with group_by %>% summarise()
vector_confint <- function(vector, interval = 0.95) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1) / 2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  # result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(error)
}

# atan2 using x and y
atan2_2d <- function(x, y, target_ang) {
  ang <- target_ang * -1 * pi / 180 # convert to rads
  
  x_r <- (x * cos(ang)) - (y * sin(ang))
  y_r <- (x * sin(ang)) + (y * cos(ang))
  
  return(atan2(y_r, x_r) * 180 / pi) # atan2(y,x) -- atan2 takes y first
}

# bootstrapped confidence intervals
# input = a vector
bootstr_confint <- function(vector, interval = 0.95) {
  # incomplete
}

# slope fitting function for the decay data
decay_fit <- function(x_vec, y_vec) {
  # Fit a line to the data
  lm_fit <- lm(y_vec ~ x_vec)
  
  # Get the slope and intercept
  coefs <- coef(lm_fit)
  
  # return the slope and intercept
  return(coefs)
}


reg_confints <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.995, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  # x_new2 <- 1:max(x + 100)
  # y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit + t.val * se)
  slope.lower <- suppressWarnings(y.fit - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c("Lower Confidence Band", "Upper Confidence Band")
  
  # Plot the fitted linear regression line and the computed confidence bands
  # plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  # lines(y.fit, col = 'black', lwd = 2)
  # lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  # lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
}


# get the magnitude (euclidian normal) of a vector (this is faster than R's built in norm)
norm_vec <- function(vector) {
  sqrt(crossprod(vector))
}

# load data using fread
loadData <- function(path) {
  data_df <- fread(path, stringsAsFactors = TRUE)
  
  return(data_df)
}
