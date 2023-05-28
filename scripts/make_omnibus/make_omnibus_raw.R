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
original_exps <- c("rot15_cued_tilt", "rot15_uncued", "tilt_uncued_rot", 
                   "tilt_uncued_norot", "tilt_cued_rot", "tilt_cued_norot")

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
    rename(target = per_block_targetListToUse,
    camera_tilt = per_block_list_camera_tilt,
    surface_tilt = per_block_list_surface_tilt,
    surface = per_block_surface_materials)
  
  # save the omnibus_df
  fwrite(omnibus_df, file = paste(to_save_dir_path, "omnibus_raw.csv", sep = "/"))
}

make_one_ppt_file <- function(directory_index, ppt_list) {
  
  ppt_dir <- ppt_list[directory_index]
  
  # print(ppt_dir) # debug 
  
  # load in all trial_results.csv files
  trial_df <- fread(paste(ppt_dir, "S001/trial_results.csv", sep = "/"))
  
  
  ## ALL EXPS ## 
  ### remove things ###
  trial_df <- trial_df %>% 
    filter(type != "instruction") %>%
    select(-"session_num", -"indicator_angle", -"show_path", -"tilt_after_fire",
           -"flick_multiplier", -"step_timestamp")
  
  ### add things ###
  # add a column for the throw_deviation
  trial_df <- trial_df %>% 
    mutate(throw_deviation = atan2_2d(flick_velocity_x, flick_velocity_z, 
                                      per_block_targetListToUse))
  
  ## ORIGIAL EXPS ##
  if (trial_df$experiment[1] %in% original_exps){
    ### remove things ###
    trial_df <- trial_df %>%
      select(-starts_with("tracking_"), -starts_with("hand_"),
             -starts_with("pinball_path"))
    
    ### add things ###
    trial_df$anim_type <- "none"
    trial_df$exp_label <- "original_exps"
  } else if (trial_df$experiment[1] == "a_ball_roll_animate_surface"){
    ## ANIMATE SURFACE EXP ##
    ### remove things ###
    trial_df <- trial_df %>%
      select(-starts_with("hand_pos_flick"), -starts_with("ball_pos_step1"))
    
    ### add things ###
    trial_df$exp_label <- "animate_surface"
  }

  ### remove things final
  
  # # make a per trial summary and join it to the trial_df 
  # hand_df_summary <- hand_df %>%
  #   group_by(trial_num) %>%
  #   summarise(
  #     hand_time_start_move = first(time_start_move),
  #     hand_time_reach = first(time_reach),
  #     hand_time_total_move = first(time_total_move),
  #     hand_angle_3cm_move = first(angle_3cm_move),
  #   )
  # 
  # trial_df <- trial_df %>%
  #   left_join(hand_df_summary, by = "trial_num")
  
  # return the trial_df
  

  
  
  
  return(trial_df)
}

#####  Test ##### 
# directory_index = 120
# 
# 
# ppt_list <- list.dirs(to_load_dir_path, recursive = FALSE)
# # make a list of length length(ppt_list) 
# trial_df_list <- vector("list", length(ppt_list))
# for (i in 1:length(ppt_list)) {
#   trial_df_list[[i]] <- make_one_ppt_file(i, ppt_list)
# }
# 
# omnibus_df <- do.call(rbind, trial_df_list)

##### Do #####
plan(multisession)
make_omnibus_raw_file(to_load_dir_path)















# function to load all files and merge
DEPRECATED <- function() {

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
        "end",
        ifelse(omnibus_df$trial_num %in% c(81, 82, 83, 84),
          "washout_init",
          ifelse(omnibus_df$trial_num %in% c(117, 118, 119, 120),
            "washout_end",
            ifelse(omnibus_df$trial_num %in% c(122, 123, 124, 125),
              "transfer_init",
              ifelse(omnibus_df$trial_num %in% c(158, 159, 160, 161),
                "transfer_end", "other"
              )
            )
          )
        )
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
