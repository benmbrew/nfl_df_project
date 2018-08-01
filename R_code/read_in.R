# this script will read in the nfl data we have and clean, combine, and explore

# load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(ggthemes)
library(smooth)
library(pracma)

# source functions script to get custom functions 
source('functions.R')

# -----------------------------------------------------------
# read in player level data
dat_2016 <- read_csv('../data/player_2016.csv')
dat_2017 <- read_csv('../data/player_2017.csv')

# add year column
dat_2016$year <- '2016'
dat_2017$year <- '2017'

# make lower case 
names(dat_2016) <- tolower(names(dat_2016))
names(dat_2017) <- tolower(names(dat_2017))

# remove playoffs from dataset
dat_2016 <- dat_2016[!grepl('Playoffs|Postseason', dat_2016$dataset),]
dat_2016$dataset <- NULL
dat_2017 <- dat_2017[!grepl('Playoffs|Postseason', dat_2017$dataset),]
dat_2017$dataset <- NULL

# convert date to date object
dat_2016$date <- as.Date(dat_2016$date, format = '%m/%d/%Y')
dat_2017$date <- as.Date(dat_2017$date, format = '%m/%d/%Y')

# remove all defensive players (for now, might want them)
dat_2016 <- dat_2016[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2016$position),]
dat_2017 <- dat_2017[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2017$position),]

# remove defensive columns as well
dat_2016 <- dat_2016[,!grepl('def_', names(dat_2016))]
dat_2017 <- dat_2017[,!grepl('def_', names(dat_2017))]

# first recode position PR-WR
dat_2016$position <- gsub('PR-WR', 'WR', dat_2016$position)

# get position statistics
# create list for results
position_data_list_2016 <- list()
position_data_list_2017 <- list()

# get all positions
all_positions <- unique(dat_2016$position)

# loop through each position and get corresponding statitics 
for(position in 1:length(all_positions)){
  position_name <- all_positions[position]
  position_data_list_2016[[position]] <- get_position_stats(dat_2016, pos_type = position_name)
  position_data_list_2017[[position]] <- get_position_stats(dat_2017, pos_type = position_name)
}

# get data from list for 2016
wr_16 <- position_data_list_2016[[1]]
qb_16 <- position_data_list_2016[[2]]
te_16 <- position_data_list_2016[[3]]
k_16 <- position_data_list_2016[[4]]
rb_16 <- position_data_list_2016[[5]]

# get data from list 2017
wr_17 <- position_data_list_2017[[1]]
qb_17 <- position_data_list_2017[[2]]
te_17 <- position_data_list_2017[[3]]
k_17 <- position_data_list_2017[[4]]
rb_17 <- position_data_list_2017[[5]]

# create a function that loops through the position list and featurizes data
# this is the main function that featurizes player data.
temp_dat <- k_16
position_name <- 'K'
j = 1
featurize_player_data <- function(temp_dat,  position_name){
  
  # subset by position
  sub_pos <- temp_dat[temp_dat$position == position_name,]
  # restruture data 
  sub_pos <- sub_pos %>% mutate_if(is.integer, as.numeric)

  # remove players with low frequency
  sub_pos <- remove_low_frequency_players(sub_pos)
  
  # get vector of unique player names 
  player_names <- unique(sub_pos$player)
  
  # create list to store player results
  player_result_list <- list()
  
  #loop throug players
  for(j in 1:length(player_names)){
    
    # get player names and subset
    individual_player <- player_names[j]
    
    message('-- working on ', individual_player)
    sub_player <- sub_pos[sub_pos$player == individual_player,]
    
    # condition to featurize by position
    if(position_name == 'WR'){
      sub_player <- get_wr_data(sub_player)
    }
    if(position_name == 'QB'){
      sub_player <- get_qb_data(sub_player)
    }
    if(position_name == 'TE'){
      sub_player <- get_te_data(sub_player)
    }
    if(position_name == 'K'){
      sub_player <- get_k_data(sub_player)
    }
    
    
    player_result_list[[j]] <- sub_player
  }
  player_data <- do.call('rbind', player_result_list)
  return(player_data)
}

wr_16_all <- featurize_player_data(wr_16, position_name = 'WR')

# plan: combine player level data and join with fantasy data, drop all non fantasy players
dat_all <- rbind(dat_2016,
                 dat_2017)

# remove datasets
rm(dat_2016, dat_2017)

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

# use win_lost function from functions.R
dat_team_2016 <- get_win_loss(dat_team_2016)
dat_team_2017 <- get_win_loss(dat_team_2017)

# apply function to get game number for each team
dat_team_2016 <- get_game_num(dat_team_2016)
dat_team_2017 <- get_game_num(dat_team_2017)

# remove playoffs from dataset
dat_team_2016 <- dat_team_2016[!grepl('Wild|Division|Conference|Super', dat_team_2016$week),]
dat_team_2017 <- dat_team_2017[!grepl('Wild|Division|Conference|Super', dat_team_2017$week),]

# Use custom function 'featurize_team_data' sourced from functions.R
# this function loops through each team and creates features for each row (game) with 
# previous weeks statistics 
dat_team_2016 <- featurize_team_data(dat_team_2016)
dat_team_2017 <- featurize_team_data(dat_team_2017)

dat_team_2016 <- get_opposing_team_stats(dat_team_2016)
dat_team_2017 <- get_opposing_team_stats(dat_team_2017)

# combine data sets and save for joining and modelling
dat_team <- rbind(dat_team_2016,
                  dat_team_2017)

rm(dat_team_2016,
   dat_team_2017)

saveRDS(dat_team, '../data/mod_dat_team.rda')

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

# rename columns for all data sets
names(dat_dk) <- c('week', 'year', 'game_id', 'player_name','player_position', 'team', 'venue', ' opponent', 'dk_fan_points', 'dk_fan_salary')

dat_dk <- dat_dk[dat_dk$dk_fan_salary> 0,]
dat_dk <- dat_dk[dat_dk$dk_fan_points > 0,]

plot(dat_dk$dk_fan_points, dat_dk$dk_fan_salary)
abline(lm(dk_fan_salary ~ dk_fan_points, data = dat_dk))

ggplot(dat_dk[dat_dk$year == '2016',], aes(dk_fan_points, dk_fan_salary)) +
  geom_point(size = 3, alpha =0.2, color = 'black') +
  geom_smooth(se = T, color = 'white') + 
  labs(x = 'Fantasy points', y = 'Fantasy Salary') + 
  theme_pander()
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

