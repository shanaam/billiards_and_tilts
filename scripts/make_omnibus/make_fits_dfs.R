# Make Exponential fit dfs for error
source("src/helper_funcs.R")
library(data.table)
library(tidyverse)
library(furrr)

plan(multisession) # for parallel processing

# load omnibus dataframe
omnibus_df <- read_delim("data/processed/omnibus/omnibus_raw.csv",
                         delim = ",",
                         col_types = cols(
                           .default = col_double(),
                           type = col_factor(),
                           ppid = col_factor(),
                           exp_label = col_factor(),
                           experiment = col_factor(),
                           hand = col_factor(),
                           camera_tilt = col_factor(),
                           surface_tilt = col_factor(),
                           target = col_factor(),
                           test_type = col_factor(),
                           phase = col_factor(),
                           learning_and_decay_curves = col_factor(),
                           prior_anim = col_factor(),
                           baseline_block = col_factor(),
                           task_type = col_factor(),
                           surface = col_factor(),
                           anim_type = col_factor()
                         )) %>% # filter out practice blocks
  filter(block_num > 4,
         !(ppid %in% c(2, 81, 82, 83)))

omnibus_df$error_size <- omnibus_df$error_size / 10

print(Sys.time())
# 2 parameter error fits
apply_exponential_fit <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      phase = first(phase),
      exponentialFit = exponentialFit(
        error_size,
        mode = 'washout' # this is a special case where all curves are decaying
      )
    )
  }

fits_df <- omnibus_df %>%
  filter(learning_and_decay_curves == 1) %>%
  group_by(ppid, experiment, phase) %>%
  group_split() %>%
  future_map(apply_exponential_fit) %>%
  bind_rows() %>%
  unnest(cols = c("exponentialFit"))

print("done 2 param error fits")
print(Sys.time())

write_csv(fits_df, "data/processed/exp_fits_errors_df.csv")

# repeat using exponentialFit_3par
apply_exponential_fit_3par <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      phase = first(phase),
      exponentialFit = exponentialFit_3par(
        error_size,
        mode = 'washout' # this is a special case where all curves are decaying
      )
    )
  }

fits_df_3par <- omnibus_df %>%
  filter(learning_and_decay_curves == 1) %>%
  group_by(ppid, experiment, phase) %>%
  group_split() %>%
  future_map(apply_exponential_fit_3par) %>%
  bind_rows() %>%
  unnest(cols = c("exponentialFit"))

print("done 3 param error fits")
print(Sys.time())

write_csv(
  fits_df_3par,
  "data/processed/exp_fits_errors_df_3par.csv"
)

############### Throw Deviation ##############
apply_exponential_fit <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      phase = first(phase),
      exponentialFit = exponentialFit(
        norm_throw_deviation,
        mode = phase[1]
      )
    )
}

init_learning_rates <- omnibus_df %>%
  filter(learning_and_decay_curves == 1) %>%
  group_by(ppid, experiment, phase) %>%
  group_split() %>%
  future_map(apply_exponential_fit) %>%
  bind_rows() %>%
  unnest(cols = c("exponentialFit"))

print("done 2 param initial exp fits")
print(Sys.time())

write_csv(init_learning_rates, "data/processed/learning_rate_df.csv")


# repeat using exponentialFit_3par
apply_exponential_fit_3par <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      phase = first(phase),
      exponentialFit = exponentialFit_3par(
        norm_throw_deviation,
        mode = phase[1]
      )
    )
}

init_learning_rates_3par <- omnibus_df %>%
  filter(learning_and_decay_curves == 1) %>%
  group_by(ppid, experiment, phase) %>%
  group_split() %>%
  future_map(apply_exponential_fit_3par) %>%
  bind_rows() %>%
  unnest(cols = c("exponentialFit"))

print("done 3 param initial exp fits")
print(Sys.time())

write_csv(
  init_learning_rates_3par,
  "data/processed/learning_rate_df_3par.csv"
)

############### Animation ################
# first, isolate the data
data_per_ppt_anim <- omnibus_df %>%
  filter(
    exp_label == "animate_surface",
    baseline_block == FALSE,
    test_type == "washout_anim"
  ) %>%
  group_by(ppid, prior_anim, trial_num_in_block) %>%
  summarise(
    ppt_median_deviation = median(throw_deviation),
    .groups = "drop"
  ) %>% # prior_anim rename to experiment (to match the other comparisons)
  mutate(
    experiment = prior_anim,
    throw_deviation = ppt_median_deviation
  ) %>%
  select(-prior_anim, -ppt_median_deviation)

data_per_ppt_30 <- omnibus_df %>%
  filter(
    experiment %in% c("rot30_cued_tilt", "rot30_uncued"),
    test_type == "washout_init",
    trial_num_in_block <= 8
  ) %>%
  select(ppid, trial_num_in_block, experiment, throw_deviation)

# rbind data_per_ppt_anim and data_per_ppt_30
anim_comparison_df <- rbind(
  data_per_ppt_anim,
  data_per_ppt_30
)

# remove data_per_ppt_anim and data_per_ppt_30
rm(data_per_ppt_anim, data_per_ppt_30)

print(Sys.time())
apply_exponential_fit <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      exponentialFit = exponentialFit(throw_deviation,
                                      mode = "washout"
      )
    )
}

anim_learning_rates <- anim_comparison_df %>%
  group_by(ppid, experiment) %>%
  group_split() %>%
  future_map(apply_exponential_fit) %>%
  bind_rows() %>%
  unnest(cols = c("exponentialFit"))

print("done 2 param animation fits")
print(Sys.time())

write_csv(
  anim_learning_rates,
  "data/processed/learning_rate_df_anim.csv"
)
