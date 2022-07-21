## --------------------------------
##
## Script name: makeOmnibus.R
##
## Purpose of script: Make omnibus dataframes (all trials, for all participants)
##
## Author: Shanaa Modchalingam
##
## Date created: 2021-21-02
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
##
## --------------------------------

## ----
## Load packages
library(data.table)
library(tidyverse)

## Functions
#

# simple function for applying atan2 (removes baseline.. subtracts.. something)
applyAtan2 <- function(df) {
  x <- df[1]
  y <- df[2]
  ang <- df[3] * pi / 180 # then convert to rads

  # treat target as 0
  x_rotated <- (x * cos(ang)) - (y * sin(ang))
  y_rotated <- (x * sin(ang)) + (y * cos(ang))

  return(atan2(y_rotated, x_rotated) * 180 / pi) # atan2(y,x) -- atan2 takes y first
}

# function to load 1 participant's data
load_one_file <- function(path) {

  # isolate the trial_result file
  file_to_load <- list.files(
    path = path,
    pattern = glob2rx("*trial_results.csv"),
    full.names = TRUE
  )

  # load file
  df <- fread(file_to_load, stringsAsFactors = FALSE)

  # remove large string to save space
  # remove cursor x,y,z (this is direction indicator: not present in task)
  df <- df %>%
    select(
      experiment, ppid, trial_num, block_num,
      trial_num_in_block, type, hand, score,
      starts_with("per_block"), starts_with("flick"),
      starts_with("home"),
      starts_with("target"), error_size
      # starts_with("pinball_path")
    )

  # return  df
  return(df)
}

# function to load all files and merge
make_omnibus <- function() {

  # build path
  path <- "data/raw_data"

  # list to populate with dfs, and an iterator variable
  list_dfs <- list()
  i <- 1

  for (ppt_path in list.files(path = path, full.names = TRUE)) {
    # add session num to path
    ppt_path <- paste(ppt_path, "S001", sep = "/")

    # add each df to the lsit
    list_dfs[[i]] <- load_one_file(ppt_path)

    # increment i
    i <- i + 1
  }

  # merge list
  omnibus_df <- do.call(rbind, list_dfs)

  # recode the initial 4 experiments
  omnibus_df <- omnibus_df %>%
    mutate(experiment = recode(
      experiment,
      "tilt_cued_norot" = "tilt_cued_tilt",
      "tilt_uncued_norot" = "tilt_uncued",
      "tilt_cued_rot" = "rot30_cued_tilt",
      "tilt_uncued_rot" = "rot30_uncued",
    ))

  # replace any errors of 0.5 or more with NAs
  omnibus_df <- omnibus_df %>%
    mutate(error_size = replace(error_size, error_size > 0.70, NA))

  ####
  ####
  #### IMPORTANT
  # temporarily filter out ppt 81 and 82
  omnibus_df <- omnibus_df %>%
    filter(
      ppid != 81, ppid != 82,
      ppid != 83, ppid != 84
    )

  # add throw_angle
  omnibus_df <- omnibus_df %>%
    mutate(throw_angle = apply(omnibus_df[, c(
      "flick_direction_x", "flick_direction_z",
      "per_block_targetListToUse"
    )],
    1,
    FUN = applyAtan2
    ))

  # fix trial numbers
  omnibus_df$trial_num <- omnibus_df$trial_num - 213

  # add a column for trial_set
  omnibus_df <- omnibus_df %>%
    mutate(trial_set = ifelse(
      omnibus_df$trial_num %in% c(1, 2, 3, 4),
      "init",
      ifelse(omnibus_df$trial_num %in% c(77, 78, 79, 80),
        "end", "other"
      )
    ))

  # add throw_magnitude
  omnibus_df <- omnibus_df %>%
    mutate(throw_magnitude = sqrt(
      flick_direction_x^2 +
        flick_direction_y^2 +
        flick_direction_z^2
    ))

  return(omnibus_df)
}


# test
omnibus_df <- make_omnibus()
# save the omnibus df
fwrite(omnibus_df, file = "data/omnibus/omnibus_throws.csv")
