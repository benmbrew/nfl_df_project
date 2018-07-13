# this script will read in the nfl data we have and clean, combine, and explore

# load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(sqldf)

# -----------------------------------------------------------
# read in nba statistics data
dat_2016 <- read_csv('../data/player_2016.csv')
dat_2017 <- read_csv('../data/player_2017.csv')

# add year column
dat_2016$year <- '2016'
dat_2017$year <- '2017'

# plan: combine player level data and join with fantasy data, drop all non fantasy players
dat_all <- rbind(dat_2016,
                 dat_2017)

# remove datasets
rm(dat_2016, dat_2017)

# make lower case 
names(dat_all) <- tolower(names(dat_all))

# remove playoffs from dataset
dat_all <- dat_all[!grepl('Playoffs', dat_all$dataset),]
dat_all$dataset <- NULL


# convert date to date object
dat_all$date <- as.Date(dat_all$date, format = '%m/%d/%Y')

# remove all defensive players 
names(dat_all) <- tolower(names(dat_all))

# remove all defensive players (for now, might want them)
dat_all <- dat_all[grepl('QB|WR|^TE$|^K$|FB|RB|PR-WR', dat_all$position),]

# ------------------------------------------------------------
# read in team data

# read in season data for 2016 and 2017
dat_team_2016 <- read_csv('../data/team_2016.csv')
dat_team_2017 <- read_csv('../data/team_2017.csv')

# create a game id for each game present int he data
dat_team_2016$game_id <- rep(1:(nrow(dat_team_2016)/2), each=2)
dat_team_2017$game_id <- rep(1:(nrow(dat_team_2017)/2), each=2)

# remove opening_odds, closing odds, spread_odds_movts,
dat_team_2016$opening_odds <- dat_team_2016$closing_odds <- dat_team_2016$spread_odds_movts <- 
  dat_team_2016$halftime <- NULL
dat_team_2017$opening_odds <- dat_team_2017$closing_odds <- dat_team_2017$spread_odds_movts <- 
  dat_team_2017$halftime <- NULL

# convert date to date object
dat_team_2016$date <- as.Date(dat_team_2016$date, format = '%m/%d/%Y')
dat_team_2017$date <- as.Date(dat_team_2017$date, format = '%m/%d/%Y')

# create a function to create a variable to indicate win or loss
get_win_loss <- function(temp_dat){
  
  # create list to store loop results
  data_list <- list()
  
  for(i in 1:length(unique(temp_dat$game_id))){
    # get individual game 
    sub_game <- temp_dat[temp_dat$game_id == i,]
    
    # create win loss variable 
    sub_game$win_loss <- NA
    # condition for winning
    if(sub_game$final[1] > sub_game$final[2]){
      sub_game$win_loss[1] <- 'W'
      sub_game$win_loss[2] <- 'L'
      
    } else {
      sub_game$win_loss[1] <- 'L'
      sub_game$win_loss[2] <- 'W'
    }
    data_list[[i]] <- sub_game
  }
  
  final_dat <- as.data.frame(do.call('rbind', data_list))
  return(final_dat)
}

dat_team_2016 <- get_win_loss(dat_team_2016)
dat_team_2017 <- get_win_loss(dat_team_2017)

# get game number for each team 
get_game_num <- function(temp_dat) {
  
  result_list <- list()
  unique_teams <- unique(temp_dat$team)
  
  for(i in 1:length(unique_teams)){
    sub_dat <- temp_dat[temp_dat$team == unique_teams[i],]
    sub_dat$game_num <- seq(1, nrow(sub_dat), 1)
    result_list[[i]] <- sub_dat
  }
  
  result_dat <- do.call('rbind', result_list)
  return(result_dat)
}

# apply function to get game number for each team
dat_team_2016 <- get_game_num(dat_team_2016)
dat_team_2017 <- get_game_num(dat_team_2017)

# remove playoffs from dataset
dat_team_2016 <- dat_team_2016[!grepl('Wild|Division|Conference|Super', dat_team_2016$week),]
dat_team_2017 <- dat_team_2017[!grepl('Wild|Division|Conference|Super', dat_team_2017$week),]

# create a function that uses lag to get previous weeks data 
get_lag_data <- function(temp_dat, variable){
  temp_dat[, variable] <- lag(temp_dat[, variable])
  temp_dat[, variable] <- ifelse(is.na(temp_dat[, variable]), 0, temp_dat[, variable])
  return(temp_dat[, variable])
}

# loop though each team and restructure data so each row shows historical stats 
# (week before, 3 weeks before cumulative, etc)
temp_dat <- dat_team_2016
i = 1
featurize_data <- function(temp_dat){
  # get a vector of team names to loop through
  unique_teams <- unique(temp_dat$team)
  # loop through unique teams and grab sub team and opponenet data to featurize
  data_list <- list()
  for(i in 1:length(unique_teams)){
    # get team name and subset
    this_team <- unique_teams[i]
    sub_team <- temp_dat[temp_dat$team == this_team, ]
    sub_team_name <- unique(sub_team$team)
    
    # begin generating features
    # to start: days since last game
    sub_team <- sub_team %>% mutate(last_game=round(c(100,diff(date)), 1))
    
    # create a numeric win column and use the lag function 
    sub_team$win_ind <- ifelse(sub_team$win_loss == 'W', 1, 0 )
    sub_team$win_ind <- get_lag_data(sub_team, 'win_ind')
    
    # create a numeric venue column and use the lag function 
    sub_team$venue_ind <- ifelse(sub_team$venue == 'Home', 1, 0 )
    sub_team$venue_ind <- get_lag_data(sub_team, 'venue_ind')
    
    # get cumulative sum of lagged wins and winning percentage 
    sub_team$cum_wins_lag <- cumsum(sub_team$win_ind)
    sub_team$cum_wins_per_lag <- cumsum(sub_team$win_ind)/get_lag_data(sub_team, 'game_num')
    sub_team$cum_wins_per_lag <- ifelse(sub_team$cum_wins_per_lag == 'NaN', 0, sub_team$cum_wins_per_lag)
    
    # create a momentum variable off of lagged cumulative wins
    sub_team$momentum <- diff(c(0,sub_team$cum_wins_per_lag))
    
    # take the inverse
    sub_team$momentum <- ifelse(sub_team$momentum == 0, 0, 1/sub_team$momentum)
    
    # win streak
    setDT(sub_team)[, win_streak:=  .N, rleid(win_ind)]
    sub_team$win_streak <- ifelse(sub_team$win_ind == 0, 0, sub_team$win_streak)
    sub_team_dt[,  win_streak:= cumsum(Sum), by=list(Year, ID)]
    
  
  }

}
# # create a function to restructure both data sets so each row is a game 
# # function that takes every other row and attaches to the dataframe 
# get_by_game <- function(temp_dat) {
#   
#   # make column names lower case 
#   colnames(temp_dat) <- tolower(colnames(temp_dat))
#   
#   # create list to stroe loops results
#   temp_new_game <- list()
#   
#   # loop through by 2 and combine 
#   for(i in unique(temp_dat$game_id)){
#     # subset temp_data 
#     temp_game <- temp_dat[temp_dat$game_id == i,]
#     
#     # get first and second row
#     temp_1st_row <- as.data.frame(temp_game[1,])
#     temp_2nd_row <- as.data.frame(temp_game[2,])
#     
#     # add "away" to 1st row columns
#     colnames(temp_1st_row) <- paste0(colnames(temp_1st_row), '_away')
#     colnames(temp_2nd_row) <- paste0(colnames(temp_2nd_row), '_home')
#     
#     # bind them together 
#     temp_new_game[[i]] <- cbind(temp_2nd_row, temp_1st_row)
#     
#   }
#   final_game <- do.call(rbind, temp_new_game)
#   return(final_game)
# }
# 
# # apply function to both data sets to get a wide data set makring hom and away by column
# dat_team_2016 <- get_by_game(dat_team_2016)
# dat_team_2017 <- get_by_game(dat_team_2017)

# combined data
dat_team <- rbind(dat_team_2016,
                  dat_team_2017)
rm(dat_team_2016, dat_team_2017)

# remove playoff games from week (away or home)
dat_team <- dat_team[!grepl('Wild|Division|Conference|Super', dat_team$week_home),]

# keep only necessary columns
dat_team$dataset_away <- dat_team$date_away <- dat_team$week_away <-dat_team$game_id_away <-
  dat_team$venue_home <- dat_team$venue_away <- NULL

# create variable indicating home team win
dat_team$home_team_win <- 

# ------------------------------------------------------------
# read in fantasy data
dat_fan_off <- read_csv('../data/player_fan_offense.csv')
dat_fan_def <- read_csv('../data/player_fan_defense.csv')

# read in draft kings scraped data from 2014-2017
dat_dk <- read_csv('../data/draft_kings_scrape.csv')

# read in fanduel scraped data from 2011-2016
dat_fd <- read_csv('../data/fan_duel_scrape.csv')

# add year
dat_fan_off$year <- '2017'
dat_fan_def$year <- '2017'

# remove playoffs from dataset
dat_fan_off <- dat_fan_off[!grepl('Playoffs', dat_fan_off$dataset),]
dat_fan_def <- dat_fan_def[!grepl('Wild|Divis|Conf|Super', dat_fan_def$week),]

dat_fan_def$dataset <- NULL
dat_fan_off$dataset <- NULL

# convert to date type
dat_fan_off$date <- as.Date(dat_fan_off$date, format = '%m/%d/%Y')
dat_fan_def$date <- as.Date(dat_fan_def$date, format = '%m/%d/%Y')



# # join two fantasy with all data, by date, player, and team
# dat_all <- left_join(dat_all, dat_fan_off, by = c('date' = 'date',
#                                                   'player' = 'player',
#                                                   'team' = 'team'))
# 
# # remove unneeded columns
# dat_all$week.y <- dat_all$opponent.y <- dat_all$venue.y <-dat_all$year.y <- NULL
# 
# # remove .x from columns
# names(dat_all) <- gsub('.x', '', names(dat_all), fixed = TRUE)

