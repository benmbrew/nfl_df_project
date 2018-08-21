
# this script will be used to predict fantasy points ONLY using fantasy data (more years.)

# load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(tidyverse)
library(preprocessCore)
library(pROC)
library(doParallel) 
library(e1071)
library(nnet)
library(glmnet)
library(PRROC)
library(broom)

# source funtions
source('functions.R')

# read in data
dat_off <- readRDS('../data/model_data/dat_fan_off.rda')
dat_def <- readRDS('../data/model_data/dat_fan_def.rda')

# -----------------------------------------------------------------
# offense data

# remove unneeded columns
dat_off$draft_kings_position <- NULL
dat_off$game_id <- NULL

# change year to numeric for correct order
dat_off$year <- as.numeric(dat_off$year)
dat_off$week <- as.numeric(dat_off$week)

# get folds - where each fold is the first week in my data and the final fold is the last week, regardless of year.
dat_off <- get_fantasy_off_folds(dat_off, season_length = 17)

# break up into position level data
dat_qb <- dat_off %>% filter(fan_duel_position == 'QB')
dat_rb <- dat_off %>% filter(fan_duel_position == 'RB')
dat_wr <- dat_off %>% filter(fan_duel_position == 'WR')
dat_te <- dat_off %>% filter(fan_duel_position == 'TE')
dat_k <- dat_off %>% filter(fan_duel_position == 'K')



#### ---------------------------------------------------------------------------------------
# predict points for all positions using all players, use posistion as a feature though



#### ---------------------------------------------------------------------------------------
# predict points for each position using all players, but not as a feature

#### ---------------------------------------------------------------------------------------
# predict points for each position using inidividual player models for each position
