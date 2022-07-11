library(psych) # for descriptives
library(effsize) # for Cohen's d
library(BayesFactor) # to compute Bayes factors
library(tidyverse)
library(data.table)

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

# get the magnitude (euclidian normal) of a vector (this is faster than R's built in norm)
norm_vec <- function(vector) {
  sqrt(crossprod(vector))
}

# atan2 using x, y, home_x, home_y, target(degrees)
applyAtan2 <- function(df){
  x = df[1] 
  y = df[2]
  ang = df[3] * -1 *pi/180 #convert to rads
  
  x_r = (x*cos(ang)) - (y*sin(ang))
  y_r = (x*sin(ang)) + (y*cos(ang))
  
  return(atan2(y_r, x_r) * 180/pi) # atan2(y,x) -- atan2 takes y first
}


# load data using fread
loadData <- function(path) {
  data_df <- fread(path, stringsAsFactors = TRUE)

  return(data_df)
}

# Plotting functions
plot_desc_group_density <- function(df, group, y, subgroup = NULL, title = "Distributions within Groups") {
  # plot some density things for visualizing data in groups
  # group, and y are strings
  if (is.null(subgroup)) {
    p <- df %>%
      ggplot(aes(.data[[group]], .data[[y]])) +
      geom_violin(aes(fill = .data[[group]]), alpha = 0.2, draw_quantiles = c(.25, .5, .75), scale = "count") +
      geom_beeswarm(alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", size = 3, color = "red")
  } else {
    p <- df %>%
      ggplot(aes(.data[[group]], .data[[y]], colour = .data[[subgroup]])) +
      geom_beeswarm(dodge.width = .9, alpha = 0.3) +
      geom_violin(aes(fill = .data[[subgroup]]), alpha = 0.2, draw_quantiles = c(.25, .5, .75), scale = "count")
  }

  p <- p +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "#CCCCCC")) +
    ggtitle(title)

  return(p)
}

plot_nice_group_density <- function(df, group, y, subgroup = NULL, title = "Distributions within Groups") {
  # plot some density things for visualizing data in groups
  # group, and y are strings
  # plot some density things for visualizing data in groups
  # group, and y are strings
  group_tidy <- enquo(group)
  y_tidy <- enquo(y)
  subgroup_tidy <- enquo(subgroup)

  desc_temp <- df %>%
    group_by(!!group_tidy, !!subgroup_tidy) %>%
    summarise(
      means = mean(!!y_tidy),
      sd = sd(!!y_tidy),
      ci = vector_confint(!!y_tidy),
      n = n(), .groups = "drop"
    )

  if (is.null(subgroup)) {
    p <- df %>%
      ggplot(aes(!!group_tidy, !!y_tidy, colour = !!group_tidy)) +
      geom_beeswarm(alpha = 0.2) +
      geom_linerange(
        data = desc_temp,
        aes(!!group_tidy, means,
          ymin = means - ci, ymax = means + ci
        ),
        lwd = 5, alpha = 0.4,
        position = position_dodge(width = .6)
      ) +
      geom_point(
        data = desc_temp,
        aes(y = means),
        size = 5, alpha = 0.6,
        position = position_dodge(width = .6)
      )
  } else {
    p <- df %>%
      ggplot(aes(!!group_tidy, !!y_tidy, colour = !!subgroup_tidy)) +
      geom_beeswarm(dodge.width = .6, alpha = 0.2) +
      geom_linerange(
        data = desc_temp,
        aes(!!group_tidy, means,
          colour = !!subgroup_tidy,
          ymin = means - ci, ymax = means + ci
        ),
        lwd = 5, alpha = 0.4,
        position = position_dodge(width = .6)
      )
  }

  p <- p +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "#CCCCCC")) +
    ggtitle(title)

  return(p)
}


# ----
# from paper (find)
bayes_t_test <- function(df, group_title, group1, group2, dv) {
  # bayes t-test

  data <- df %>%
    group_by(NULL) %>%
    filter(.data[[group_title]] == group1 | .data[[group_title]] == group2) %>%
    select(group_title, dv)

  res.bayes <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  rownames(res.bayes) <- colnames(data)[-1]
  colnames(res.bayes) <- c("BF01", "Postd.Median", "Postd.LB", "Postd.UB")

  # the test

  i <- 1 # this is useful for iterating
  # Note: code from https://osf.io/gfdjy/
  # Removing NAs
  data.noNA <- data[!is.na(data[, i + 1]), ]

  # Identify independent groups,
  # BF01:
  tmp <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[, dv]),
    pull(filter(data.noNA, .data[[group_title]] == group2)[, dv]),
    rscale = .707
  )
  BF01 <- 1 / extractBF(tmp)$bf

  # Posterior distribution for BF01:
  set.seed(i)
  tmp <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[, dv]),
    pull(filter(data.noNA, .data[[group_title]] == group2)[, dv]),
    posterior = TRUE,
    iterations = 10000,
    rscale = .707
  )
  CI.bayes <- quantile(tmp[, "delta"], probs = c(.03, .97))

  res.bayes[i, ] <- c(BF01, median(tmp[, "delta"]), CI.bayes)

  # show
  res.bayes

  #    2.6 Posterior model probabilities for equal prior odds ----
  # post.H0:
  null <- res.bayes$BF01 / (1 + res.bayes$BF01)
  # post.H1:
  alt <- 1 / (1 + res.bayes$BF01)

  return(sprintf("BF01: %.3f, P(D|H0): %.3f, P(D|H1): %.3f, Groups: %s vs %s", res.bayes$BF01, null, alt, group1, group2))
}

#####