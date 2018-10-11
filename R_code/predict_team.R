# this script will read in already cleaned and featurized team level data and predict 
# win as a classification as well as point spread and over/under.
# source functions script to get custom functions 
source('model_functions.R')

# read in team level data
dat_team <- readRDS('../data/cleaned_data/team_data_combined.rda')

#### ----------------------------------------------------------------------
# prepare model for predictions

#  loop through each game id (2 teams per game id) and create a variable indicating the away team, and then
#  remove the away team row, so that all necessary data is in the home team row. 

# get data in a form where we can predict one row as one game
dat_team <- get_matrix_structure(dat_team)

# remove date, venue, game_id, 
dat_team$date <- dat_team$venue <- dat_team$venue_ind_team <- 
  dat_team$game_id <- dat_team$game_num <- NULL


# force all character to be factors for the model
dat_team$team <- as.factor(dat_team$team)
dat_team$win_loss <- as.factor(dat_team$win_loss)
dat_team$year <- as.factor(dat_team$year)
dat_team$team_opp <- as.factor(dat_team$team_opp)

#### ----------------------------------------------------------------------
# Strategy 1
# Classify wins and losses 

# get folds 
dat_team <- get_data_folds(dat_team, season_length = 17)


# model_matrix = dat_team
# train_window = c(1:30)
# test_window = c(31:34)
# include_team = TRUE
# include_opp = TRUE
# model_type = 'elastic_net'
# importance_fac = TRUE
# fixed_window = TRUE
# initial_window = 100
# horizon_window = 50
# lm_aic = TRUE
# predict at game level
temp <- pred_team(dat_team, )

#### ----------------------------------------------------------------------
# Strategy 2
# Estimate point spread 



#### ----------------------------------------------------------------------
# Strategy 3
# Estimate over under  



