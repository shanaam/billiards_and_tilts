# Make Exponential Fit Curves using the Fit Dfs
rm(list = ls())
source("src/helper_funcs.R")
library(data.table)
library(tidyverse)
library(future)

make_fit_curve_df <- function(fit_df) {
  # make summary df with parameters for the curves
  per_phase_summary <- fit_df %>%
    group_by(experiment, phase) %>%
    summarise(
      mean_learning_rate = mean(exp_fit_lambda),
      ci_learning_rate = vector_confint(exp_fit_lambda),
      mean_high = mean(exp_fit_N0),
      ci_high = vector_confint(exp_fit_N0),
      .groups = "drop"
    )

  # determine the number of unique phases and experiments
  num_phases <- nrow(per_phase_summary)
  fit_curve_list <- vector("list", num_phases)

  for (i in 1:num_phases) {
    fit_curve_list[[i]] <- future(make_one_fit_curve(i, per_phase_summary))
  }

  future_output <- value(fit_curve_list)
  fit_curve_df <- do.call(rbind, future_output)

  return(fit_curve_df)
}

# for each experiment and phase, make a fit curve using
# 1000 points from 1 to 40
make_one_fit_curve <- function(i, per_phase_summary) {
  # get the experiment and phase
  experiment <- per_phase_summary$experiment[i]
  phase <- per_phase_summary$phase[i]

  # get the parameters for the curve
  lambda <- per_phase_summary$mean_learning_rate[i]
  N0 <- per_phase_summary$mean_high[i]

  # make the fitted curve
  if (phase == "washout") {
    fit_curve <- tibble(
      x = seq(1, 40, length.out = 1000),
      y = N0 * (1 - lambda)^x
    ) %>%
      mutate(
        experiment = experiment,
        phase = phase
      )
  } else {
    fit_curve <- tibble(
      x = seq(1, 40, length.out = 1000),
      y = N0 - (N0 * (1 - lambda)^x)
    ) %>%
      mutate(
        experiment = experiment,
        phase = phase
      )
  }

  # add the curve to the list
  return(fit_curve)
}

# function to load the fit dfs and create the fit curve dfs
make_all_fit_curve_dfs <- function() {
  # path to fit df directory
  fit_df_dir <- "data/processed/"
  to_save_dir <- "data/processed/fit_curves/"

  # list of fit dfs to load (2 param ones)
  fit_df_list <- c(
    "learning_rate_df.csv",
    "learning_rate_df_anim.csv",
    "exp_fits_errors_df.csv",
    "exp_fits_alt_washout_curves.csv",
    "exp_fits_alt_all_washout_curves.csv"
  )

  # add the path to each fit df
  fit_df_list <- paste0(fit_df_dir, fit_df_list)

  # loop through each fit df and make the fit curve df
  for (fit_df_path in fit_df_list) {
    # load the fit df
    fit_df <- read_csv(
      fit_df_path,
      col_types = cols(
        .default = col_double(),
        experiment = col_factor(),
        phase = col_factor()
      )
    )

    # make the fit curve df
    fit_curve_df <- make_fit_curve_df(fit_df)

    # add "fitted_" to the front of the name
    fit_df_name <- paste0("fitted_", basename(fit_df_path))

    # save the fit curve df
    fit_curve_df_path <- paste0(
      to_save_dir,
      fit_df_name
    )

    write_csv(fit_curve_df, fit_curve_df_path)
  }
}

#### Do ####
plan(multisession) # for parallel processing
make_all_fit_curve_dfs()

# #### Test ####
#
# alt_washout_rates <- read_csv(
#   "data/processed/exp_fits_alt_washout_curves.csv",
#   col_types = cols(
#     .default = col_double(),
#     experiment = col_factor(),
#     phase = col_factor()
#   )
# )
#
# fit_df <- alt_washout_rates
# fit_curve_df <- make_fit_curve_df(fit_df)
#
# # plot fit_curve_list[[1]]
# p <- ggplot(fit_curve_df, aes(x, y,
#   colour = experiment,
#   linetype = phase
# )) +
#   theme_bw() +
#   labs(
#     x = "Trial Number",
#     y = "Fit Decay Curve"
#   ) +
#   geom_line()
#
# p
#
# library(plotly)
# ggplotly(p)
