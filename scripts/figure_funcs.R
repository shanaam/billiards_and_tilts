#### Script for making figures
#### Author: Shanaa Modchalingam
#### Date: April 2022

# clean environment
# rm(list = ls())
# source("src/helper_funcs.R")
library(data.table)
library(tidyverse)
library(ggbeeswarm)
library(ggnewscale)
# palette for plotting:
# https://venngage-wordpress.s3.amazonaws.com/uploads/2019/08/color-blind-friendly-palette-9.png # nolint

# note: R has no has tables, but environments can work like one
# (they are hashed under the hood)
pallete <- new.env()
pallete$rot_c <- "#d40000"
pallete$rot_nc <- "#f9982c"
pallete$tilt_c <- "#07509b"
pallete$tilt_nc <- "#5fb696"

omnibus_path <- "data/omnibus/omnibus_throws.csv"
with_path_omnibus <- "data/omnibus/omnibus_throws_with_path.csv"

#### HELPER FUNCTIONS
load_main_experiments <- function(omnibus_path) {
  # load the data
  data <- read_delim(omnibus_path,
    delim = ",",
    col_types = cols(
      .default = col_double(),
      type = col_factor(),
      ppid = col_factor(),
      experiment = col_factor(),
      hand = col_factor(),
      per_block_list_camera_tilt = col_factor(),
      # per_block_list_surface_tilt = col_factor(),
      per_block_targetListToUse = col_factor(),
      per_block_surface_materials = col_factor(),
      trial_set = col_factor(),
      pinball_path_x = col_character(),
      pinball_path_y = col_character(),
      pinball_path_z = col_character()
    ), show_col_types = FALSE
  ) %>%
    filter(experiment != "rot15_cued_tilt", experiment != "rot15_uncued")

  data$error_size <- data$error_size * 100

  # rename column targetListToUse to target
  data <- data %>% rename(target = "per_block_targetListToUse")

  return(data)
}


#### PLOTTING FUNCTIONS
# function to plot the learning curve
plot_learning_curve <- function() {
  # load the data
  data <- load_main_experiments(omnibus_path)

  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # set up the plot
  p <- data_group %>%
    ggplot(aes(
      x = trial_num, y = group_mean,
      colour = experiment, fill = experiment
    ))

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # make backgrount from 0 to 80 grey
  p <- p +
    geom_rect(
      xmin = 0, xmax = 80,
      ymin = 0, ymax = 60,
      fill = "#CCCCCC", colour = NA,
      alpha = 0.5
    )

  # add confidence intervals and data
  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num >= 0, trial_num <= 80),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num >= 0, trial_num <= 80))

  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num >= 81, trial_num <= 120),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num >= 81, trial_num <= 120))

  # theme changes
  p <- p + theme_classic() +
    xlab("Trial Number") +
    ylab("Error Size (cm)") +
    scale_x_continuous(limits = c(1, 120), breaks = seq(0, 120, by = 40)) +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    theme(text = element_text(size = 35))

  # set colour palette
  p <- p + scale_colour_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  )) + scale_fill_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  ))

  # remove legend
  p <- p + theme(legend.position = "none")

  return(p)
}

# function to plot the learning curve
plot_rebound <- function() {
  # load the data
  data <- load_main_experiments(omnibus_path)

  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # set up the plot
  p <- data_group %>%
    ggplot(aes(
      x = trial_num, y = group_mean,
      colour = experiment, fill = experiment
    ))

  # make background from 0 to 40 grey
  p <- p +
    geom_rect(
      xmin = 0, xmax = 40,
      ymin = 0, ymax = 60,
      fill = "#CCCCCC", colour = NA,
      alpha = 0.5
    )

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # add confidence intervals and data
  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num > 0),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num > 0))

  # theme changes
  p <- p + theme_classic() +
    xlab("Trial Number") +
    ylab("Error Size (cm)") +
    scale_x_continuous(limits = c(1, 40), breaks = seq(0, 40, by = 40)) +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    theme(text = element_text(size = 35))

  # set colour palette
  p <- p + scale_colour_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  )) + scale_fill_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  ))

  # remove legend
  p <- p + theme(legend.position = "none")

  return(p)
}

# make comparison figures
plot_comparison <- function() {
  # load the data
  data <- read_delim("data/omnibus/omnibus_throws.csv",
    delim = ",",
    col_types = cols(
      .default = col_double(),
      type = col_factor(),
      ppid = col_factor(),
      experiment = col_factor(),
      hand = col_factor(),
      per_block_list_camera_tilt = col_factor(),
      per_block_list_surface_tilt = col_factor(),
      per_block_targetListToUse = col_factor(),
      per_block_surface_materials = col_factor(),
      trial_set = col_factor()
    )
  )

  # fix error size
  data$error_size <- data$error_size * 100

  # filter in the 3 trial numbers: 214, 294, and 335
  data <- data %>%
    filter(trial_num %in% c(214, 215, 294, 295, 335, 336))

  # make trial_num a factor
  data$trial_num <- factor(data$trial_num)

  # unite exp and trial_num
  data <- data %>%
    unite(exp_trial_set, experiment, trial_num)

  # make summary df
  data_group <- data %>%
    group_by(exp_trial_set) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # reorder data_group by exp_trial_set
  data_group <- data_group %>%
    arrange(match(
      exp_trial_set,
      c(
        "rot30_cued_tilt_214", "rot30_cued_tilt_294", "rot30_cued_tilt_335",
        "rot30_uncued_214", "rot30_uncued_294", "rot30_uncued_335", # nolint
        "tilt_cued_tilt_214", "tilt_cued_tilt_294", "tilt_cued_tilt_335", # nolint
        "tilt_uncued_214", "tilt_uncued_294", "tilt_uncued_335" # nolint
      )
    ))

  data_group$exp_trial_set <- factor(c(
    "rot30_cued_tilt_214", "rot30_cued_tilt_294", "rot30_cued_tilt_335",
    "rot30_uncued_214", "rot30_uncued_294", "rot30_uncued_335",
    "tilt_cued_tilt_214", "tilt_cued_tilt_294", "tilt_cued_tilt_335",
    "tilt_uncued_214", "tilt_uncued_294", "tilt_uncued_335" # nolint
  ),
  levels = c(
    "rot30_cued_tilt_214", "rot30_cued_tilt_294", "rot30_cued_tilt_335",
    "rot30_uncued_214", "rot30_uncued_294", "rot30_uncued_335",
    "tilt_cued_tilt_214", "tilt_cued_tilt_294", "tilt_cued_tilt_335",
    "tilt_uncued_214", "tilt_uncued_294", "tilt_uncued_335" # nolint
  )
  )


  # set up the plot
  p <- data_group %>%
    ggplot(aes(
      x = exp_trial_set, y = group_mean,
      colour = exp_trial_set, fill = exp_trial_set
    ))

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # add confidence intervals and data
  p <- p +
    geom_linerange(
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      alpha = 0.3, lwd = 5,
    ) +
    geom_point(size = 5)

  # theme changes
  p <- p + theme_classic() +
    xlab("Select Trials") +
    ylab("Error Size (cm)") +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    scale_x_discrete(
      labels = c(
        "PR", "AR", "PL",
        "PR", "AR", "PL",
        "PR", "AR", "PL",
        "PR", "AR", "PL"
      )
    ) +
    theme(text = element_text(size = 35))

  # make x labels vertical
  p <- p + theme(axis.text.x = element_text(
    angle = 90
  ))

  # set colour palette
  p <- p + scale_colour_manual(values = c(
    pallete$rot_c, pallete$rot_c, pallete$rot_c,
    pallete$rot_nc, pallete$rot_nc, pallete$rot_nc,
    pallete$tilt_c, pallete$tilt_c, pallete$tilt_c,
    pallete$tilt_nc, pallete$tilt_nc, pallete$tilt_nc
  )) + scale_fill_manual(values = c(
    pallete$rot_c, pallete$rot_c, pallete$rot_c,
    pallete$rot_nc, pallete$rot_nc, pallete$rot_nc,
    pallete$tilt_c, pallete$tilt_c, pallete$tilt_c,
    pallete$tilt_nc, pallete$tilt_nc, pallete$tilt_nc
  ))

  # remove legend
  p <- p + theme(legend.position = "none")

  return(p)
}


# function to plot the learning curve
plot_learning_curve_15 <- function() {
  # load the data
  data <- read_delim("data/omnibus/omnibus_throws.csv",
    delim = ",",
    col_types = cols(
      .default = col_double(),
      type = col_factor(),
      ppid = col_factor(),
      experiment = col_factor(),
      hand = col_factor(),
      per_block_list_camera_tilt = col_factor(),
      per_block_list_surface_tilt = col_factor(),
      per_block_targetListToUse = col_factor(),
      per_block_surface_materials = col_factor(),
      trial_set = col_factor()
    )
  ) %>%
    filter(experiment != "rot30_cued_tilt", experiment != "rot30_uncued")

  # subtract 121 from trial number
  data$trial_num <- data$trial_num
  data$error_size <- data$error_size * 100

  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # set up the plot
  p <- data_group %>%
    ggplot(aes(
      x = trial_num, y = group_mean,
      colour = experiment, fill = experiment
    ))

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # make backgrount from 0 to 80 grey
  p <- p +
    geom_rect(
      xmin = 0, xmax = 80,
      ymin = 0, ymax = 60,
      fill = "#CCCCCC", colour = NA,
      alpha = 0.5
    )

  # add confidence intervals and data
  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num >= 0, trial_num <= 80),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num >= 0, trial_num <= 80))

  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num >= 81, trial_num <= 120),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num >= 81, trial_num <= 120))
  # theme changes
  p <- p + theme_classic() +
    xlab("Trial Number") +
    ylab("Error Size (cm)") +
    scale_x_continuous(limits = c(1, 120), breaks = seq(0, 120, by = 40)) +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    theme(text = element_text(size = 35))

  # set colour palette
  p <- p + scale_colour_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  )) + scale_fill_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  ))

  # remove legend
  p <- p + theme(legend.position = "none")

  return(p)
}

# function to plot the learning curve
plot_rebound_15 <- function() {
  # load the data
  data <- read_delim("data/omnibus/omnibus_throws.csv",
    delim = ",",
    col_types = cols(
      .default = col_double(),
      type = col_factor(),
      ppid = col_factor(),
      experiment = col_factor(),
      hand = col_factor(),
      per_block_list_camera_tilt = col_factor(),
      per_block_list_surface_tilt = col_factor(),
      per_block_targetListToUse = col_factor(),
      per_block_surface_materials = col_factor(),
      trial_set = col_factor()
    )
  ) %>%
    filter(experiment != "rot30_cued_tilt", experiment != "rot30_uncued")

  # subtract 121 from trial number
  data$trial_num <- data$trial_num - 121
  data$error_size <- data$error_size * 100

  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # set up the plot
  p <- data_group %>%
    ggplot(aes(
      x = trial_num, y = group_mean,
      colour = experiment, fill = experiment
    ))

  # make background from 0 to 40 grey
  p <- p +
    geom_rect(
      xmin = 0, xmax = 40,
      ymin = 0, ymax = 60,
      fill = "#CCCCCC", colour = NA,
      alpha = 0.5
    )

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # add confidence intervals and data
  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num > 0),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num > 0))

  # theme changes
  p <- p + theme_classic() +
    xlab("Trial Number") +
    ylab("Error Size (cm)") +
    scale_x_continuous(limits = c(1, 40), breaks = seq(0, 40, by = 40)) +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    theme(text = element_text(size = 35))

  # set colour palette
  p <- p + scale_colour_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  )) + scale_fill_manual(values = c(
    pallete$tilt_c, pallete$tilt_nc,
    pallete$rot_c, pallete$rot_nc
  ))

  # remove legend
  p <- p + theme(legend.position = "none")

  return(p)
}

# target by tartget plot
plot_lc_target <- function() {
  # load the data
  data <- load_main_experiments(omnibus_path)

  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num, target) %>%
    summarise(
      group_mean = mean(error_size, na.rm = TRUE),
      sd = sd(error_size, na.rm = TRUE),
      ci = vector_confint(error_size),
      n = n(), .groups = "drop"
    )

  # set up the plot, split by experiment
  p <- data_group %>%
    ggplot(aes(
      x = trial_num, y = group_mean,
      colour = target, fill = target,
    )) +
    facet_wrap(~experiment)

  # add helper lines
  # add horizontal line at 0
  p <- p +
    geom_hline(
      yintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # add confidence intervals and data
  p <- p +
    geom_ribbon(
      data = filter(data_group, trial_num >= 0, trial_num <= 80),
      aes(ymin = group_mean - ci, ymax = group_mean + ci),
      colour = NA, alpha = 0.3
    ) +
    geom_line(data = filter(data_group, trial_num >= 0, trial_num <= 80))

  # theme changes
  p <- p + theme_classic() +
    xlab("Trial Number") +
    ylab("Error Size (cm)") +
    scale_x_continuous(limits = c(1, 80), breaks = seq(0, 80, by = 40)) +
    scale_y_continuous(limits = c(-10, 60), breaks = seq(0, 60, 10)) +
    theme(text = element_text(size = 35))

  # remove legend
  # p <- p + theme(legend.position = "none")

  return(p)
}

# success manifold
plot_success_manifold_no_tilt <- function() {
  # load the data
  data <- load_main_experiments(omnibus_path)

  # filter only one target and filter out rotated type and NA error sizes
  data <- data %>%
    filter(
      target %in% c(92, 96), type == "rotated",
      error_size != "NA", per_block_list_surface_tilt < 10,
      trial_num >= -169 # remove practice trials
    )

  # set up the plot
  p <- data %>%
    ggplot(aes(
      text = paste("ppt:", ppid, " trial:", trial_num),
      x = throw_angle, y = throw_magnitude,
      colour = error_size
    ))

  # add a vertical line at 0
  p <- p +
    geom_vline(
      xintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # set colour palette
  p <- p + scale_colour_gradient(
    low = "#7eaee2", high = "#001a35",
    limits = c(5, 70), na.value = "white"
  )

  # add data
  p <- p +
    geom_point(
      size = 3,
      alpha = 0.9
    )

  # theme changes
  p <- p + theme_classic() +
    xlab("Release Angle (deg)") +
    ylab("Release Velocity (m/s)") +
    theme(
      text = element_text(size = 20),
      panel.background = element_rect(fill = "#868686", colour = "#000000")
    ) + facet_wrap(~ fct_relevel(target, "96", "92", "88", "84"),
      ncol = 1, strip.position = "right"
    ) +
    scale_y_continuous(limits = c(1.0, 2.2)) +
    scale_x_reverse(limits = c(50, -50))

  # set new colour scale
  p <- p + new_scale_colour() +
    scale_color_manual(values = c(
      "#d58c2c", "#e85531",
      "#53beca", "#2c6ebf"
    )) +
    scale_shape_manual(values = c(16, 16, 1, 1))

  # isolate just the trial_sets we need
  data <- data %>%
    filter(trial_set != "other")

  # add the new data to plot
  p <- p +
    geom_point(
      data = data,
      aes(colour = trial_set, shape = trial_set),
      size = 2
    )

  # add title
  p <- p +
    ggtitle(
      "Throws with 30 deg Rotation"
    )

  return(p)
}

# success manifold
plot_success_manifold_tilt <- function() {
  # load the data
  data <- load_main_experiments(omnibus_path)

  # filter only one target and filter out rotated type and NA error sizes
  data <- data %>%
    filter(
      target %in% c(92, 96), type != "rotated",
      error_size != "NA", per_block_list_surface_tilt > 10
    )

  # set up the plot
  p <- data %>%
    ggplot(aes(
      text = paste("ppt:", ppid, " trial:", trial_num),
      x = throw_angle, y = throw_magnitude,
      colour = error_size
    ))

  # add a vertical line at 0
  p <- p +
    geom_vline(
      xintercept = 0, colour = "#CCCCCC",
      linetype = "dashed"
    )

  # set colour palette
  p <- p + scale_colour_gradient(
    low = "#7eaee2", high = "#001a35",
    limits = c(5, 70), na.value = "white"
  )

  # add data
  p <- p +
    geom_point(
      size = 3,
      alpha = 0.9,
    )

  # theme changes
  p <- p + theme_classic() +
    xlab("Release Angle (deg)") +
    ylab("Release Velocity (m/s)") +
    theme(
      text = element_text(size = 20),
      panel.background = element_rect(fill = "#868686", colour = "#000000")
    ) + facet_wrap(~ fct_relevel(target, "96", "92", "88", "84"),
      ncol = 1, strip.position = "right"
    ) +
    scale_y_continuous(limits = c(1.0, 2.2)) +
    scale_x_reverse(limits = c(50, -50))


  # set new colour scale
  p <- p + new_scale_colour() +
    scale_color_manual(values = c(
      "#d58c2c", "#e85531",
      "#53beca", "#2c6ebf"
    )) +
    scale_shape_manual(values = c(16, 16, 1, 1))

  # isolate just the trial_sets we need
  data <- data %>%
    filter(trial_set != "other")

  # add the new data to plot
  p <- p +
    geom_point(
      data = data,
      aes(colour = trial_set, shape = trial_set),
      size = 2
    )

  return(p)
}

# add one ball path to plot, given ppt and trial number
plot_one_ball_path <- function(ppt, trial, experiment, target,
                               trial_set, p) {
  trial <- trial + 213

  # add leading zeros to ppt
  ppt <- str_pad(ppt, 3, "0", side = "left")

  # load data
  data <- read_delim(paste("data/raw_data/", ppt,
    "/S001/trial_results.csv",
    sep = ""
  ), delim = ",", show_col_types = FALSE)

  row <- data %>%
    filter(
      trial_num == trial
    )

  path_x <- convert_cell_to_numvec(row$pinball_path_x)
  path_z <- convert_cell_to_numvec(row$pinball_path_z)

  # we should TILT VALUES HERE
  # print(paste(ppt, " ", trial))

  if (length(path_x) > 30) {
    # create empty tibble
    to_plot <- data.frame(
      path_x = path_x,
      path_z = path_z,
      experiment = experiment,
      target = target,
      trial_set = trial_set
    )

    return(
      geom_path(
        data = to_plot,
        size = 0.5,
        alpha = 0.5
      )
    )
  }
}

# plot initial and final ball paths
plot_init_end_paths <- function() {
  # load in the data
  data <- load_main_experiments(omnibus_path) %>%
    filter(
      trial_set == "transfer_init" | trial_set == "transfer_end",
      ppid != 2, error_size != "NA"
    )

  # number of rows in data

  # set up the plot
  p <- ggplot(
    NULL,
    aes(
      x = path_x,
      y = path_z - 0.4,
      linetype = trial_set,
      colour = trial_set
    )
  ) +
    theme_classic() +
    xlab("X Position") +
    ylab("Z Position") +
    theme(text = element_text(size = 20)) +
    scale_x_continuous(limits = c(-1, 0.5))

  # for n rows in data
  for (i in 1:nrow(data)) {
    # get the row in data
    row <- data[i, ]

    # get the ppt and trial number
    ppt <- row$ppid
    trial <- row$trial_num
    exp <- row$experiment
    target <- row$target
    trial_set <- row$trial_set

    # add the path
    p <- p +
      plot_one_ball_path(
        ppt, trial, exp,
        target, trial_set, p
      )
  }

  # get rows for targets
  target_summary <- data %>%
    filter(trial_set == "transfer_init") %>%
    group_by(experiment, target, trial_set) %>%
    summarise(
      n = nrow(experiment),
      mean_target_x = mean(target_x),
      mean_target_z = mean(target_z)
    )

  # add targets
  p <- p +
    geom_point(
      data = target_summary,
      aes(x = mean_target_x, y = mean_target_z - 0.4),
      colour = "#e73e3e", size = 3
    )

  # facet wrap experiment and target
  p <- p + facet_grid(
    vars(experiment),
    vars(fct_relevel(target, "96", "92", "88", "84"))
  )

  return(p)

  # add target as a poiint

  # facet by experiment
}


#### SAVE PLOTS
# # save learning curve
# ggsave(plot_learning_curve(),
#   height = 12, width = 19, device = "pdf",
#   filename = "data/figs/learning_curve15.pdf"
# )
#
# # save rebound curve
# ggsave(plot_rebound(),
#   height = 12, width = 7, device = "pdf",
#   filename = "data/figs/rebound_curve15.pdf"
# )
#
# # save lc target
# ggsave(plot_lc_target(),
#   height = 36, width = 21, device = "pdf",
#   filename = "data/figs/lc_target.pdf"
# )
#
# # save success manifolds
# ggsave(plot_success_manifold_no_tilt(),
#   height = 8, width = 10.5, device = "png",
#   filename = "data/figs/success_manifold_no_tilt.png"
# )
# ggsave(plot_success_manifold_tilt(),
# height = 8, width = 10.5, device = "png",
# filename = "data/figs/success_manifold_tilt.png"
# )

# save comparison figures
# ggsave(plot_comparison(),
#     height = 8, width = 12, device = "svg",
#     filename = "data/figs/comparison_figures.svg"
# )

# save initial and final ball paths
# ggsave(plot_init_end_paths(),
# height = 8, width = 10.5, device = "png",
# filename = "data/figs/init_end_paths.png"
# )



# -----------


# function


# plot_one_ball_path("082", 54)
# plot_one_ball_path("037", 66)

















plot_misc <- function() {
  # load the data
  data <- read_delim("data/omnibus/omnibus_throws.csv",
    delim = ",",
    col_types = cols(
      .default = col_double(),
      type = col_factor(),
      ppid = col_factor(),
      experiment = col_factor(),
      hand = col_factor(),
      per_block_list_camera_tilt = col_factor(),
      per_block_list_surface_tilt = col_factor(),
      per_block_surface_materials = col_factor()
    )
  ) %>%
    filter(type != "instruction")

  # get angles
  data$flick_ang_dev <- apply(data[
    ,
    c("flick_velocity_x", "flick_velocity_y", "per_block_targetListToUse")
  ],
  1,
  FUN = applyAtan2
  )
  # make summary df
  data_group <- data %>%
    group_by(experiment, trial_num) %>%
    summarise(
      group_mean = mean(flick_ang_dev, na.rm = TRUE),
      sd = sd(flick_ang_dev, na.rm = TRUE),
      ci = vector_confint(flick_ang_dev),
      n = n(), .groups = "drop"
    )

  # set up the plot
  p <- data %>%
    ggplot(aes(
      x = trial_num, y = flick_ang_dev,
      colour = experiment, fill = experiment
    )) +
    geom_point(alpha = 0.1) +
    geom_point(data = data_group, aes(y = group_mean))

  p
}
