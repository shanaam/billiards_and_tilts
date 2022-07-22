#### Script for making one large csv with all the ball paths
#### (one per exeperiment to save space)??
#### Author: Shanaa Modchalingam
#### Date: July 2022

# clean environment
rm(list = ls())
source("src/helper_funcs.R")
library(data.table)
library(tidyverse)

# load in the omnibus data with paths
with_path_omnibus <- "data/omnibus/omnibus_throws_with_path.csv"

#### HELPER FUNCTIONS
load_omnibus <- function(omnibus_path) {
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
        filter(type != "instruction")

    # rename column targetListToUse to target
    data <- data %>% rename(target = "per_block_targetListToUse")

    return(data)
}

data <- load_omnibus(with_path_omnibus)

# make an empty list to hold vectorized paths
paths_list_x <- list()

# for each row in the data table, vectorize the path
# and add it to the list (start with x)
for (i in 1:nrow(data)) {
    paths_list_x[[i]] <- convert_cell_to_numvec(
        as.character(data[i, "pinball_path_x"])
    )
}
# repeat for y and z
paths_list_y <- list()
for (i in 1:nrow(data)) {
    paths_list_y[[i]] <- convert_cell_to_numvec(
        as.character(data[i, "pinball_path_y"])
    )
}
paths_list_z <- list()
for (i in 1:nrow(data)) {
    paths_list_z[[i]] <- convert_cell_to_numvec(
        as.character(data[i, "pinball_path_z"])
    )
}

data$pinball_path_x <- paths_list_x
data$pinball_path_y <- paths_list_y
data$pinball_path_z <- paths_list_z

# remove ppt (bug? look into these and replace if needed)
data <- data %>% filter(
    ppid != 2, ppid != 6,
    ppid != 36
)

# unnest
data <- data %>%
    unnest(cols = c(pinball_path_x, pinball_path_y, pinball_path_z))

# save the data (this will be a 1+ GB file)
# write.csv(data, "data/omnibus/omnibus_throws_path_unnested.csv")



# save as a data file
save(data, file = "data/omnibus/omnibus_throws_path_unnested.Rdata")
