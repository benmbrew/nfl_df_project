# this script will read in the cleaned version of data from the two seasons 2016 and 2017 with joined 
# fantasy data from only 2017 - this leaves lots of NAs for 2016. After reading in the data, I'll analyze data by # position and build models for each position or player. In the latter case build a function that takes a players # and predicts fantasy points

# load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(ggridges)

# load in preprocessed data
dat_all <- readRDS('data/all_data.rda')

# convert number variables to type numeric
dat_all$Fgm_0_19 <- as.numeric(dat_all$Fgm_0_19)
dat_all$Fga_0_19 <- as.numeric(dat_all$Fga_0_19)
dat_all$safties <- as.numeric(dat_all$safties)
dat_all$draft_kings_salary <- as.numeric(dat_all$draft_kings_salary)
dat_all$draft_kings_fan_pts <- as.numeric(dat_all$draft_kings_fan_pts)
dat_all$fan_duel_salary <- as.numeric(dat_all$fan_duel_salary)
dat_all$fan_duel_fan_pts <- as.numeric(dat_all$fan_duel_fan_pts)

# convert columns to lower case

