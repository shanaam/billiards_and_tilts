# Make Exponential fit dfs for error
source("src/helper_funcs.R")
library(data.table)
library(tidyverse)

print(Sys.time())
# 2 parameter fits
apply_exponential_fit <- function(df) {
  df %>%
    summarise(
      ppid = first(ppid),
      experiment = first(experiment),
      phase = first(phase),
      exponentialFit = exponentialFit(
        error_size,
        mode = phase[1]
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

print("done 2 param fits")
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
        mode = phase[1]
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

print("done 3 param fits")
print(Sys.time())

write_csv(
  fits_df_3par,
  "data/processed/exp_fits_errors_df_3par.csv"
)
