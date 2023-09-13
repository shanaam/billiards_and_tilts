# Make Exponential Fit Curves using the Fit Dfs
rm(list = ls())

library(data.table)
library(tidyverse)
library(furrr)

plan(multisession) # for parallel processing

# load one fit df 
# load exponential decay fits
init_learning_rates <- read_csv(
  "data/processed/learning_rate_df.csv",
  col_types = cols(
    .default = col_double(),
    experiment = col_factor(),
    phase = col_factor()
  )
) %>%
  mutate(experiment = factor(
    experiment, 
    levels = c(
      "accel_cued_tilt", "accel_uncued",
      "curved_cued_tilt", "curved_uncued",
      "rot30_cued_tilt", "rot30_uncued",
      "rot15_cued_tilt", "rot15_uncued",
      "a_ball_roll_animate_surface"
    )
  )
)

alt_all_washout_rates <- read_csv(
  "data/processed/exp_fits_alt_all_washout_curves.csv",
  col_types = cols(
    .default = col_double(),
    experiment = col_factor()
  )
) %>%
  mutate(experiment = factor(
    experiment, 
    levels = c(
      "accel_cued_tilt", "accel_uncued",
      "curved_cued_tilt", "curved_uncued",
      "rot30_cued_tilt", "rot30_uncued",
      "rot15_cued_tilt", "rot15_uncued",
      "a_ball_roll_animate_surface"
    )
  )
)

fit_df <- init_learning_rates

make_one_fit_curve <- function(fit_df){
  
  
}
