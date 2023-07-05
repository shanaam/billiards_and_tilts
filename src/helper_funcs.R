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

# Exponential fits from thartbm/Reach on github with modifications
# Exponential Function -----

# fit a single exponential to learning data, with two parameters:
# - a learning rate
# - an asymptote (for incomplete learning)

# will replace asymptotic decay, but should do the same thing
# except that's it's slightly closer to an actual exponential
# and uses it behind the scenes, so that:
# it should run faster
# people can use the output for maths


#' @title Run an exponential function given parameters and a reach
#' deviation schedule. Errors decay exponentially.
#' @param par A named vector with the model parameter (see details).
#' @param timepoints An integer indicating the number of trials (N), or a vector
#' with N trial numbers (these can have missing values or be fractions). If an
#' integer, the timepoints at which the exponential will be evaluated is:
#' 0, 1 ... N-2, N-1
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @return A data frame with two columns: `timepoint` and `output`, and N rows,
#' so that each row has the output of the modeled process on each trial.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential decay model with asymptote.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: learning rate
#' - N0: asymptote
#' @examples
#' # write example!
#' @export
exponentialModel <- function(par, timepoints, mode, setN0=NULL) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] = setN0
  }
  
  if (mode == 'training_init' | mode == 'transfer_init') {
    output = par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints )
  }
  if (mode == 'washout_init') {
    output = par['N0'] * (1-par['lambda'])^timepoints
  }
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}

#' @title Get the MSE between an exponential and a series of reach deviations.
#' @param par A named numeric vector with the model parameters (see
#' exponentialModel).
#' @param signal A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @param timepoints Either an integer with the number of trials (N) or a vector
#' with N trial numbers (this can have missing values or fractions). The 
#' exponential will be evaluated at those timepoints.
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential function to describe a series of reach deviations.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: the learning rate
#' - N0: the asymptote
#' @examples
#' # write example?
#' @export
exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode='training_init', setN0=NULL) {
  
  MSE <- mean((exponentialModel(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

#' @title Fit an asymptotic decay model to reach deviations.
#' @param signal A vector of length N with reach deviation data. These should
#' start around 0 and go up (ideally they are baselined).
#' @param timepoints NULL or a vector of length N with the timepoints at which
#' to evaluate the exponential. If NULL, the N values in `signal` are placed
#' at: 0, 1, ... N-2, N-1.
#' @param mode A string, one of "learning" or "washout". For "learning" the
#' signal starts at 0 and increases with exponentially decaying errors, going
#' towards asymptote ("N0"), and for "washout" it starts at "N0" and approaches
#' 0 over time.
#' @param gridpoints Number of values for rate of change and asymptote, that
#' are tested in a grid.
#' @param gridfits Number of best results from gridsearch that are used for
#' optimizing a fit.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @return A named numeric vector with the optimal parameter that fits a simple
#' rate model to the data as best as possible, with these elements:
#' - lambda: the rate of change in the range [0,1]
#' - N0: the asymptote (or starting value) in the unit of the signal
#' @description This function is part of a set of functions to fit and
#' evaluate a simple exponential function to reach deviations.
#' @details
#' ?
#' @examples
#' # write example!
#' @import optimx
#' @export
exponentialFit <- function(signal, timepoints=length(signal), mode='training_init', gridpoints=11, gridfits=10, setN0=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (is.numeric(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals)
    lo <- c(0)
    hi <- c(1)
  }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
    lo <- c(0,asymptoteRange[1])
    hi <- c(1,asymptoteRange[2])
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:2])
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0)
  }
  
  names(winpar) <- c('exp_fit_lambda', 'exp_fit_N0')
  winpar <- as_tibble_row(winpar)
  
  # return the best parameters:
  return(winpar)
  
}


## 3-parameter versions of the above model, allows for a vertical shift

exponentialModel_3par <- function(par, timepoints, mode, setN0=NULL) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] = setN0
  }
  
  if (mode == 'training_init' | mode == 'transfer_init') {
    output = (par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints)) +  par['displace']
  }
  if (mode == 'washout_init') {
    output = (par['N0'] * (1 - par['lambda'])^timepoints) + par['displace'] # check the original exponentialModel
  }
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}

exponentialMSE_3par <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode, setN0=NULL) {
  
  MSE <- mean((exponentialModel_3par(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

exponentialFit_3par <- function(signal, timepoints=length(signal), mode, gridpoints=11, gridfits=10, setN0=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (is.numeric(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals)
    lo <- c(0)
    hi <- c(1)
  }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1],
                              'displace' = parvals * diff(asymptoteRange) + asymptoteRange[1])
    lo <- c(0,asymptoteRange[1],asymptoteRange[1])
    hi <- c(1,asymptoteRange[2],asymptoteRange[2])
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE_3par, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE_3par,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:3])
    # winpar[2] <- winpar[2] + winpar[3]
    
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0,
                 'displace' = unlist(win[3]))
  }
  
  names(winpar) <- c('exp_fit_lambda', 'exp_fit_N0', 'exp_fit_displace')
  winpar <- as_tibble_row(winpar)
  
  # return the best parameters:
  return(winpar)
  
}

# ### TEST ###
# signal <- c(20, 10, 5, 2.5, 1.25, 1, 1, 1, 1, 1, 1, 1)
# # signal <- c(1.589, 0.3014, -0.038, 0.324, 0.002, -0.002, 0.25, 0.03, 0.12, 0.08, -0.18, 0.1) # washout ppid 62
# 
# # signal <- c(0.757, 0.026, 0.122, 0.353, -.08, 0.05, 0.283, 0.223, 0.106, 0.027, 0.014, 0.299, 0.572, -0.204, 0, 0.208) # washout ppid 2
# 
# for (timepoint in c(0:(length(signal)-1))){
#   print((0.9437 * 0.7728^timepoint) + 0)
# }
