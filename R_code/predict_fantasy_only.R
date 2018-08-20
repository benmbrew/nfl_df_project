# this script will be used to predict fantasy points ONLY using fantasy data (more years.)

# load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# read in data
data_off <- readRDS('../data/model_data/dat_fan_off.rda')
data_def <- readRDS('../data/model_data/dat_fan_def.rda')

# -----------------------------------------------------------------
# offense data
