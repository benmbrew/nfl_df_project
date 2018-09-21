# this script will read in already cleaned and featurized team level data and predict 
# win as a classification as well as point spread and over/under.
# source functions script to get custom functions 
source('functions.R')

# read in team level data
dat_team <- readRDS('../data/cleaned_data/team_data_combined.rda')

#  loop through each game id (2 teams per game id) and create a variable indicating the away team, and then
#  remove the away team row, so that all necessary data is in the home team row. 

# get data in a form where we can predict one row as one game
dat_team <- get_matrix_structure(dat_team)
