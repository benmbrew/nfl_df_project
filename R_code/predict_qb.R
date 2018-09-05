# this script will predict on qb data

# source functions scrip to get functions and libraries
source('functions.R')

# fantasy and playerr data 
qb_data <- readRDS('../data/model_data/qb_data.rda')

# fantasy, player,  and team data
qb_data_team <- readRDS('../data/model_data/qb_data_team.rda')

# recode qb_data_team
names(qb_data_team) <- gsub('.x', '', names(qb_data_team), fixed = TRUE)
qb_data_team$date.y <- qb_data_team$last_game.y <- NULL

# remove unnecessary columns
qb_data$position <- qb_data$counts <- qb_data$draft_kings_position <- qb_data$fan_duel_position <- NULL
