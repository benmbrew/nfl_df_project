# this script will read in fantasy defensive data as well as player data subsetted by defensive positions

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

# read in fantasy defense
dat_fan_def <- readRDS('data/fan_def.rda')


# convert number variables to type numeric
dat_all$Fgm_0_19 <- as.numeric(dat_all$Fgm_0_19)
dat_all$Fga_0_19 <- as.numeric(dat_all$Fga_0_19)
dat_all$safties <- as.numeric(dat_all$safties)
dat_all$draft_kings_salary <- as.numeric(dat_all$draft_kings_salary)
dat_all$draft_kings_fan_pts <- as.numeric(dat_all$draft_kings_fan_pts)
dat_all$fan_duel_salary <- as.numeric(dat_all$fan_duel_salary)
dat_all$fan_duel_fan_pts <- as.numeric(dat_all$fan_duel_fan_pts)

# convert columns to lower case
names(dat_all) <- tolower(names(dat_all))

# remove all defensive players (for now, might want them)

# split data into sub datasets by player and position
