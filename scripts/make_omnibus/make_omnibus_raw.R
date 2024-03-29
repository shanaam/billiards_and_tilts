## --------------------------------
## Purpose of script: Make omnibus (per_trial) CSVs (no baseline correction)
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: uses data from data/preprocessed
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing

##### Variables #####
to_load_dir_path <- "data/raw_data"
to_save_dir_path <- "data/processed/omnibus"
original_exps <- c(
  "rot15_cued_tilt", "rot15_uncued", "tilt_uncued_rot",
  "tilt_uncued_norot", "tilt_cued_rot", "tilt_cued_norot"
)

##### All Experiments #####
make_omnibus_raw_file <- function(to_load_dir_path) {
  ppt_list <- list.dirs(to_load_dir_path, recursive = FALSE)
  # make a list of length length(ppt_list)
  trial_df_list <- vector("list", length(ppt_list))

  # loop through all directories in the to_load_dir_path
  for (i in 1:length(ppt_list)) {
    trial_df_list[[i]] <- future(make_one_ppt_file(i, ppt_list))
  }

  # evaluate
  future_output <- value(trial_df_list)

  # row bind all the trial_dfs
  omnibus_df <- do.call(rbind, future_output)

  # rename the "target_list" column to "target"
  omnibus_df <- omnibus_df %>%
    rename(
      target = per_block_targetListToUse,
      camera_tilt = per_block_list_camera_tilt,
      surface_tilt = per_block_list_surface_tilt,
      surface = per_block_surface_materials
    ) %>%
    # mutate(raw_error_size = raw_error_size * 100) %>% # convert error_size to cm
### Outlier removal: filter out throws that never got closer than 70cm to the target
    filter(raw_error_size < 0.70) %>% 
    mutate(task_type = recode( # recode tasktype
      type,
      "aligned" = "roll_to_target",
      "rotated" = "roll_to_target"
    ))


  ### Baseline correction ###
  ## For throw_deviation ##
  # make a baseline_df
  bl_df_summary <- omnibus_df %>%
    filter(baseline_block == TRUE) %>%
    group_by(ppid, task_type, hand, prior_anim, target) %>%
    summarise(
      bl_deviation = median(raw_throw_deviation, na.rm = TRUE)
    )

  # convert to data.table
  bl_df_summary <- as.data.table(bl_df_summary)
  omnibus_df <- as.data.table(omnibus_df)

  # non equi join omnibus_df and bl_df_summary
  omnibus_df <- omnibus_df[
    bl_df_summary,
    on = .(ppid, task_type, hand, prior_anim, target),
    nomatch = 0
  ]

  # subtract the baseline from the raw_throw_deviation
  omnibus_df <- omnibus_df %>%
    mutate(
      throw_deviation = raw_throw_deviation - bl_deviation
    )

  ## For error_size##
  # make baseline_df
  bl_df_summary <- omnibus_df %>%
    filter(baseline_block == TRUE) %>%
    group_by(ppid, task_type, hand, prior_anim, target) %>%
    summarise(
      bl_error = median(raw_error_size, na.rm = TRUE)
    )

  # convert to data.table
  bl_df_summary <- as.data.table(bl_df_summary)
  omnibus_df <- as.data.table(omnibus_df)

  # non equi join omnibus_df and bl_df_summary
  omnibus_df <- omnibus_df[
    bl_df_summary,
    on = .(ppid, task_type, hand, prior_anim, target),
    nomatch = 0
  ]

  # subtract the baseline from the raw_throw_deviation
  omnibus_df <- omnibus_df %>%
    mutate(
      error_size = raw_error_size - bl_error
    )

  ### Normalization ###
  # make max_learning_df
  max_learning_df_summary <- omnibus_df %>%
    filter(
      learning_and_decay_curves == 1,
      phase == "training",
      trial_num_in_block %in% (21:40) # training trials used for normalization
    ) %>%
    group_by(ppid) %>%
    summarise(
      max_learning = median(throw_deviation, na.rm = TRUE)
    )

  # convert to data.table
  max_learning_df_summary <- as.data.table(max_learning_df_summary)
  omnibus_df <- as.data.table(omnibus_df)

  # non equi join omnibus_df and max_learning_df_summary
  omnibus_df <- omnibus_df[
    max_learning_df_summary,
    on = .(ppid),
    nomatch = 0
  ]

  # divide the throw_deviation by the max_learning
  omnibus_df <- omnibus_df %>%
    mutate(
      norm_throw_deviation = throw_deviation / max_learning
    )

  # arrange by ppid and trial_num
  omnibus_df <- omnibus_df %>%
    arrange(ppid, trial_num)

  # save the omnibus_df
  fwrite(omnibus_df, file = paste(
    to_save_dir_path, "omnibus_raw.csv",
    sep = "/"
  ))
}

make_one_ppt_file <- function(directory_index, ppt_list) {
  ppt_dir <- ppt_list[directory_index]

  # print(ppt_dir) # debug

  # load in all trial_results.csv files
  trial_df <- fread(paste(ppt_dir, "S001/trial_results.csv", sep = "/"))


  ## ALL EXPS ##
  ### remove and add things ###
  trial_df <- trial_df %>%
    filter(!(type %in% c("instruction", "animate_surface"))) %>%
    select(
      -"session_num", -"indicator_angle", -"show_path", -"tilt_after_fire",
      -"flick_multiplier", -"step_timestamp"
    ) %>% # add a column for the throw_deviation
    mutate(
      raw_throw_deviation = atan2_2d(
        flick_velocity_x, flick_velocity_z,
        per_block_targetListToUse
      ),
      raw_error_size = error_size
    )

  ## ORIGIAL EXPS ##
  if (trial_df$experiment[1] %in% original_exps) {
    ### remove things ###
    trial_df <- trial_df %>%
      select(
        -starts_with("tracking_"), -starts_with("hand_"),
        -starts_with("pinball_path")
      ) %>%
      filter(block_num > 4) %>% # filter out practice blocks
      mutate(
        anim_type = "none", # add anim_type and exp_label columns
        exp_label = "original_exps"
      ) %>%
      mutate(test_type = case_when( # add test_type column
        (trial_num_in_block %in% (1:8) & block_num == 11) ~ "training_init",
        (trial_num_in_block %in% (73:80) & block_num == 11) ~ "training_end",
        (trial_num_in_block %in% (1:8) & block_num == 12) ~ "washout_init",
        (trial_num_in_block %in% (33:40) & block_num == 12) ~ "washout_end",
        (trial_num_in_block %in% (1:8) & block_num == 14) ~ "transfer_init",
        (trial_num_in_block %in% (33:40) & block_num == 14) ~ "transfer_end",
        TRUE ~ "other"
      )) %>%
      mutate(phase = case_when(
        block_num == 11 ~ "training",
        block_num == 12 ~ "washout",
        block_num == 14 ~ "transfer",
        TRUE ~ "other"
      )) %>%
      # add training and decay column
      mutate(learning_and_decay_curves = case_when(
        ((trial_num_in_block %in% (1:40) & block_num == 11) |
          (trial_num_in_block %in% (1:40) & block_num == 12) |
          (trial_num_in_block %in% (1:40) & block_num == 14)) ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(prior_anim = "none") %>%
      mutate(baseline_block = case_when( # label baseline blocks
        block_num %in% (6:8) ~ TRUE,
        TRUE ~ FALSE
      )) %>% # recode experiment
      mutate(experiment = recode(
        experiment,
        "tilt_uncued_rot" = "rot30_uncued",
        "tilt_uncued_norot" = "accel_uncued",
        "tilt_cued_rot" = "rot30_cued_tilt",
        "tilt_cued_norot" = "accel_cued_tilt"
      ))
  } else if (trial_df$experiment[1] == "a_ball_roll_animate_surface") {
    ## ANIMATE SURFACE EXP ##
    ### remove and add things ###
    trial_df <- trial_df %>%
      select(
        -starts_with("hand_pos_flick"),
        -starts_with("ball_pos_step1")
      ) %>% # remove columns
      filter(block_num > 4) %>% # filter out practice blocks
      mutate(exp_label = "animate_surface") %>% # add exp_label column
      mutate(test_type = case_when( # add test_type column
        (trial_num_in_block %in% (1:8) & block_num == 14) ~ "training_init",
        (trial_num_in_block %in% (73:80) & block_num == 14) ~ "training_end",
        (block_num %in% c(8, 16, 24, 32, 40)) ~ "washout_anim",
        (block_num %in% c(11, 20, 28, 36, 44)) ~ "washout_anim",
        (block_num == 48) ~ "washout_no_anim",
        TRUE ~ "other"
      )) %>%
      mutate(phase = case_when(
        block_num == 14 ~ "training",
        block_num %in% c(
          8, 11, # do we need these?
          20, 28, 36, 44,
          16, 24, 32, 40,
          48
        ) ~ "washout",
        TRUE ~ "other"
      )) %>%
      # add learning and decay column
      mutate(learning_and_decay_curves = case_when(
        (trial_num_in_block %in% (1:40) & block_num == 14) ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(prior_anim = case_when(
        (block_num %in% c(8, 16, 24, 32, 40)) ~ "half_anim",
        (block_num %in% c(11, 20, 28, 36, 44)) ~ "full_anim",
        (block_num == 48) ~ "half_anim",
        TRUE ~ "none"
      )) %>%
      mutate(baseline_block = case_when( # label baseline blocks
        block_num %in% (5:11) ~ TRUE,
        TRUE ~ FALSE
      ))
  } else if (trial_df$experiment[1] %in% c(
    "a_curved_cued_tilt", "a_curved_uncued"
  )) {
    ## CURVED PATH EXP ##
    ### remove and add things ###
    trial_df <- trial_df %>%
      select(
        -starts_with("hand_pos_flick"),
        -starts_with("ball_pos_step1")
      ) %>% # remove columns
      filter(block_num > 4) %>% # filter out practice blocks
      mutate(exp_label = "curved_path", anim_type = "none") %>%
      # add exp_label and anim_type column
      mutate(test_type = case_when( # add test_type column
        (trial_num_in_block %in% (1:8) & block_num == 11) ~ "training_init",
        (trial_num_in_block %in% (73:80) & block_num == 11) ~ "training_end",
        (trial_num_in_block %in% (1:8) & block_num == 12) ~ "washout_init",
        (trial_num_in_block %in% (33:40) & block_num == 12) ~ "washout_end",
        (trial_num_in_block %in% (1:8) & block_num == 14) ~ "transfer_init",
        (trial_num_in_block %in% (33:40) & block_num == 14) ~ "transfer_end",
        TRUE ~ "other"
      )) %>%
      mutate(phase = case_when(
        block_num == 11 ~ "training",
        block_num == 12 ~ "washout",
        block_num == 14 ~ "transfer",
        TRUE ~ "other"
      )) %>%
      # add learning and decay column
      mutate(learning_and_decay_curves = case_when(
        ((trial_num_in_block %in% (1:40) & block_num == 11) |
          (trial_num_in_block %in% (1:40) & block_num == 12) |
          (trial_num_in_block %in% (1:40) & block_num == 14)) ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(prior_anim = "none") %>%
      mutate(baseline_block = case_when( # label baseline blocks
        block_num %in% (6:8) ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      # remove the first 2 letters from the experiment column
      mutate(experiment = substr(experiment, 3, nchar(experiment)))
  }

  # if trial_df$experiment[1] contains "cued_tilt"
  if (grepl("cued_tilt", trial_df$experiment[1])) {
    # add a column for alt_washout_block
    trial_df <- trial_df %>%
      mutate(
        alt_washout_block = case_when(
          ((block_num == 12 & trial_num_in_block != 40) |
            (block_num == 11 & trial_num_in_block == 80))
          ~ TRUE,
          TRUE ~ FALSE
        ),
        alt_all_washout_block = case_when(
          (block_num == 12 |
            (block_num == 11 & trial_num_in_block == 80))
          ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else if (grepl("uncued", trial_df$experiment[1])) {
    trial_df <- trial_df %>%
      mutate(
        alt_washout_block = case_when(
          (block_num == 12) ~ TRUE,
          TRUE ~ FALSE
        ),
        alt_all_washout_block = case_when(
          (block_num == 12 |
            (block_num == 11 & trial_num_in_block == 80))
          ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else {
    trial_df <- trial_df %>%
      mutate(
        alt_washout_block = case_when(
          (block_num %in% c(
            20, 28, 36, 44,
            16, 24, 32, 40
          ) |
            (block_num %in% c(18, 22, 26, 30, 34, 38, 42) &
              trial_num_in_block == 20) |
            (block_num == 14 & trial_num_in_block == 80)) ~ TRUE,
          TRUE ~ FALSE
        ),
        alt_all_washout_block = case_when(
          (block_num %in% c(
            20, 28, 36, 44,
            16, 24, 32, 40
          ) |
            (block_num %in% c(18, 22, 26, 30, 34, 38, 42) &
              trial_num_in_block == 20) |
            (block_num == 14 & trial_num_in_block == 80)) ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }

  # return the trial_df
  return(trial_df)
}

# #####  Test #####
# directory_index = 120
#
# ppt_list <- list.dirs(to_load_dir_path, recursive = FALSE)
# # make a list of length length(ppt_list)
# trial_df_list <- vector("list", length(ppt_list))
#
# for (i in 1:length(ppt_list)) {
#   trial_df_list[[i]] <- make_one_ppt_file(i, ppt_list)
# }
#
# omnibus_df <- do.call(rbind, trial_df_list)

##### Do #####
plan(multisession)
make_omnibus_raw_file(to_load_dir_path)
