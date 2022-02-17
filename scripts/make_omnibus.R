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
applyAtan2 <- function(df){
  
  x = df[1] - df[3]
  y = df[2] - df[4]
  ang = df[5] * -1 *pi/180 #convert to rads
  
  x_r = (x*cos(ang)) - (y*sin(ang))
  y_r = (x*sin(ang)) + (y*cos(ang))
  
  return(atan2(y_r, x_r) * 180/pi) # atan2(y,x) -- atan2 takes y first
}

# function to load 1 participant's data
load_one_file <- function(path){
  
  # isolate the trial_result file
  file_to_load <- list.files(path = path, 
                             pattern = glob2rx("*trial_results.csv"), 
                             full.names = TRUE)
  
  # load file
  df <- fread(file_to_load, stringsAsFactors = FALSE)
  
  # remove large string to save space
  df <- df %>%
    select(experiment, ppid, trial_num, block_num,
           trial_num_in_block, type, hand, score, 
           starts_with("per_block"), starts_with("flick"),
           starts_with("cursor"), starts_with("home"), 
           starts_with("target"), error_size, magnitude)
  
  # return  df
  return(df)
}

# function to load all files and merge
make_omnibus <- function(){
  
  #build path
  path <- "data/raw_data"
  
  # list to populate with dfs, and an iterator variable
  list_dfs <- list()
  i <- 1
  
  for (ppt_path in list.files(path = path, full.names = TRUE)){
    # add session num to path
    ppt_path <- paste(ppt_path, "S001", sep = '/')
    
    # add each df to the lsit
    list_dfs[[i]] <- load_one_file(ppt_path)
    
    # increment i
    i <- i+1
  }
  
  # merge list
  omnibus_df <- do.call(rbind, list_dfs)
  
  # replace any errors of 0.5 or more with NAs
  omnibus_df <- omnibus_df %>%
    mutate(error_size = replace(error_size, error_size > 0.5, NA))
  
  return(omnibus_df)
}


# test
omnibus_df <- make_omnibus()
# save the omnibus df
fwrite(omnibus_df, file = "data/omnibus/omnibus_throws.csv")


# plot distribution of error size
omnibus_df$experiment <- as.factor(omnibus_df$experiment)

p <- omnibus_df %>%
  ggplot(aes(x = trial_num,
             y = error_size, colour = experiment)) +
  stat_summary(geom = "point") +
  stat_summary(geom = "line", alpha = 0.3)
  
  # geom_density(alpha=.2) 
p

