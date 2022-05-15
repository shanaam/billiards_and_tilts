#### Script for making publication figures (training data)
#### Author: Shanaa Modchalingam
#### Date: April 2022

# clean environment
rm(list = ls())
source("src/helper_funcs.R")
library(data.table)
library(tidyverse)
library(ggbeeswarm)

# palette for plotting:
# https://venngage-wordpress.s3.amazonaws.com/uploads/2019/08/color-blind-friendly-palette-9.png # nolint

# note: R has no has tables, but environments can work like one
# (they are hashed under the hood)
pallete <- new.env()
pallete$rot_c <- "#d40000"
pallete$rot_nc <- "#f9982c"
pallete$tilt_c <- "#07509b"
pallete$tilt_nc <- "#5fb696"

# function to plot the learning curve
plot_learning_curve <- function() {
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
            per_block_surface_materials = col_factor()
        )
    )

    # subtract 213 from trial number
    data$trial_num <- data$trial_num - 213
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
            data = filter(data_group, trial_num > 0, trial_num <= 80),
            aes(ymin = group_mean - ci, ymax = group_mean + ci),
            colour = NA, alpha = 0.3
        ) +
        geom_line(data = filter(data_group, trial_num > 0, trial_num <= 80))

    p <- p +
        geom_ribbon(
            data = filter(data_group, trial_num > 81, trial_num <= 120),
            aes(ymin = group_mean - ci, ymax = group_mean + ci),
            colour = NA, alpha = 0.3
        ) +
        geom_line(data = filter(data_group, trial_num > 81, trial_num <= 120))
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
            per_block_surface_materials = col_factor()
        )
    )

    # subtract 213 from trial number
    data$trial_num <- data$trial_num - 213 - 121
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

    # make backgrount from 0 to 40 grey
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
            per_block_surface_materials = col_factor()
        )
    )

    # subtract 213 from trial number
    data$error_size <- data$error_size * 100

    # filter in the 3 trial numbers: 214, 294, and 335
    data <- data %>%
        filter(trial_num == 214 | trial_num == 294 | trial_num == 335)

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
                "tilt_cued_rot_214", "tilt_cued_rot_294", "tilt_cued_rot_335",
                "tilt_uncued_rot_214", "tilt_uncued_rot_294", "tilt_uncued_rot_335", # nolint
                "tilt_cued_norot_214", "tilt_cued_norot_294", "tilt_cued_norot_335", # nolint
                "tilt_uncued_norot_214", "tilt_uncued_norot_294", "tilt_uncued_norot_335" # nolint
            )
        ))

    data_group$exp_trial_set <- factor(c(
        "tilt_cued_rot_214", "tilt_cued_rot_294", "tilt_cued_rot_335",
        "tilt_uncued_rot_214", "tilt_uncued_rot_294", "tilt_uncued_rot_335",
        "tilt_cued_norot_214", "tilt_cued_norot_294", "tilt_cued_norot_335",
        "tilt_uncued_norot_214", "tilt_uncued_norot_294", "tilt_uncued_norot_335" # nolint
    ),
    levels = c(
        "tilt_cued_rot_214", "tilt_cued_rot_294", "tilt_cued_rot_335",
        "tilt_uncued_rot_214", "tilt_uncued_rot_294", "tilt_uncued_rot_335",
        "tilt_cued_norot_214", "tilt_cued_norot_294", "tilt_cued_norot_335",
        "tilt_uncued_norot_214", "tilt_uncued_norot_294", "tilt_uncued_norot_335" # nolint
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

# save learning curve
ggsave(plot_learning_curve(),
    height = 12, width = 19, device = "svg",
    filename = "data/figs/learning_curve.svg"
)

# save rebound curve
ggsave(plot_rebound(),
    height = 12, width = 7, device = "svg",
    filename = "data/figs/rebound_curve.svg"
)

# save comparison figures
ggsave(plot_comparison(),
    height = 8, width = 12, device = "svg",
    filename = "data/figs/comparison_figures.svg"
)