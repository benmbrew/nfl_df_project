# load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)

# read in fantasy data
dat_off <- readRDS('../data/model_data/fantasy_offense.csv')
dat_def <- readRDS('../data/model_data/fantasy_defense.csv')

# make sure to only keep variables necessary for prediction

