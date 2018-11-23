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

# first remove week 1 and from 2016
# get remove index
dat_team$remove_index <- paste0(dat_team$week,'_', dat_team$year)
dat_team <- dat_team[dat_team$remove_index != '1_2016',]
dat_team$remove_index <- NULL

# loop through unique week and year and assign fold
unique_years <- as.character(unique(dat_team$year))
year_list <- list()
for(i in 1:length(unique_years)){
  
  this_year <- unique_years[i]
  sub_dat <- dat_team[dat_team$year == this_year, ]

  if(this_year == '2016'){
    sub_dat$fold <- (sub_dat$week -1)
  }
  
  if(this_year == '2017'){
    sub_dat$fold <- (sub_dat$week + 16)
    
  }
  
  if(this_year == '2018'){
    sub_dat$fold <- (sub_dat$week +33)
  }
  year_list[[i]] <- sub_dat
  print(i)
 
}

mod_dat <- do.call('rbind', year_list)

# create outcome that is differnece between home and away points
mod_dat$home_diff <- (mod_dat$final_team - mod_dat$final_opp)
mod_dat$final_opp <- mod_dat$final_team <- NULL

# remove closing spread for oppostion and keep for team and make outcome
mod_dat$closing_spread_opp <- NULL

# multiply closing_spread_team  by -1 to homogenized predictions and real outcome 
mod_dat$closing_spread_team <- mod_dat$closing_spread_team*(-1)

# predict at game level

temp <- pred_team(modxel_matrix = mod_dat,
                  train_window = c(1:39),
                  test_window = c(40:44),
                  include_team = TRUE,
                  include_opp = TRUE,
                  model_type = 'elastic_net',
                  importance_fac = FALSE,
                  fixed_window = TRUE,
                  initial_window = 100,
                  horizon_window = 50,
                  lm_aic = FALSE)

temp_results <- temp[[1]]
temp_imp <- temp[[2]]

# take the absolute value of preds and real points and then both should have same condition?
# if negative preds are greater than real points than good
# if positive preds are less than real points than good
temp_results$winner <- NA
data_results <- list()
for(i in 1:nrow(temp_results)){
  sub_row <- temp_results[i,]
  if(sub_row$predicted_points < 0 ){
    sub_row$winner <- ifelse(sub_row$predicted_points > sub_row$real_points, TRUE, FALSE)
  } else {
    sub_row$winner <- ifelse(sub_row$predicted_points < sub_row$real_points, TRUE, FALSE)
  }
  data_results[[i]] <- sub_row
}

final_results <- do.call('rbind', data_results)
# WHAT IS REAL POINTS??
# get a winner column
#### ----------------------------------------------------------------------
# Strategy 2
# Estimate point spread 



#### ----------------------------------------------------------------------
# Strategy 3
# Estimate over under  



