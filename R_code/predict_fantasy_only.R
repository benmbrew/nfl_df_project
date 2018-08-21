
# this script will be used to predict fantasy points ONLY using fantasy data (more years.)

# load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# source funtions
source('functions.R')

# read in data
dat_off <- readRDS('../data/model_data/dat_fan_off.rda')
dat_def <- readRDS('../data/model_data/dat_fan_def.rda')

# -----------------------------------------------------------------
# offense data

# remove unneeded columns
dat_off$draft_kings_position <- NULL

# change year to numeric for correct order
dat_off$year <- as.numeric(dat_off$year)

# create an indicator for each week played from the beginning of the data, to use as folds in the model



# unique_years <- sort(unique(dat_off$year))
# i = 1
# for(i in 1:length(unique_years)) {
#   this_year <- unique_years[i]
#   sub_year <- dat_off[dat_off$year == this_year,]
# }