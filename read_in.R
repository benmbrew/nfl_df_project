# this script will read in the nfl data we have and clean, combine, and explore

# load libraries
library(tidyverse)
library(dplyr)
library(readr)

# -----------------------------------------------------------
# read in nba statistics data
dat_2016 <- read_csv('data/player_2016.csv')
dat_2017 <- read_csv('data/player_2017.csv')

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
dat_team_2016 <- read_csv('data/team_2016.csv')
dat_team_2017 <- read_csv('data/team_2017.csv')

# create a game id for each game present int he data
dat_team_2016$game_id <- rep(1:(nrow(dat_team_2016)/2), each=2)
dat_team_2017$game_id <- rep(1:(nrow(dat_team_2017)/2), each=2)



# create a function to restructure both data sets so each row is a game 
# function that takes every other row and attaches to the dataframe 
get_by_game <- function(temp_dat) {
  
  # make column names lower case 
  colnames(temp_dat) <- tolower(colnames(temp_dat))
  
  # create list to stroe loops results
  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(i in unique(temp_dat$game_id)){
    # subset temp_data 
    temp_game <- temp_dat[temp_dat$game_id == i,]
    
    # get first and second row
    temp_1st_row <- as.data.frame(temp_game[1,])
    temp_2nd_row <- as.data.frame(temp_game[2,])
    
    # add "away" to 1st row columns
    colnames(temp_1st_row) <- paste0(colnames(temp_1st_row), '_away')
    colnames(temp_2nd_row) <- paste0(colnames(temp_2nd_row), '_home')
    
    # bind them together 
    temp_new_game[[i]] <- cbind(temp_2nd_row, temp_1st_row)
    
  }
  final_game <- do.call(rbind, temp_new_game)
  return(final_game)
}

# apply function to both data sets to get a wide data set makring hom and away by column
dat_team_2016 <- get_by_game(dat_team_2016)
dat_team_2017 <- get_by_game(dat_team_2017)

# combined data
dat_team <- rbind(dat_team_2016,
                  dat_team_2017)
rm(dat_team_2016, dat_team_2017)

# remove playoff games from week (away or home)
dat_team <- dat_team[!grepl('Wild|Division|Conference|Super', dat_team$week_home),]

# ------------------------------------------------------------
# read in fantasy data
dat_fan_off <- read_csv('data/player_fan_offense.csv')
dat_fan_def <- read_csv('data/player_fan_defense.csv')

# read in draft kings scraped data from 2014-2017
dat_dk <- read_csv('data/draft_kings_scraped.csv')

# read in fanduel scraped data from 2011-2016
dat_fd <- read_csv('data/fan_duel_scraped.csv')

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

