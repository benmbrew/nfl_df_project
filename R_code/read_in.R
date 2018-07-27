# this script will read in the nfl data we have and clean, combine, and explore

# load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(sqldf)
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
dat_2016 <- dat_2016[grepl('QB|WR|^TE$|^K$|FB|RB|PR-WR', dat_2016$position),]
dat_2017 <- dat_2017[grepl('QB|WR|^TE$|^K$|FB|RB|PR-WR', dat_2017$position),]

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

# get all players
all_players_2016 <- unique(dat_2016$player)
all_players_2017 <- unique(dat_2017$player)

# take the union of these two sets and delete year specifics
all_players <- union(all_players_2016,
                     all_players_2017)
rm(all_players_2016, all_players_2017)


# loop through each position and get corresponding statitics 
for(position in 1:length(all_positions)){
  position_name <- all_positions[position]
  position_data_list_2016[[position]] <- get_position_stats(dat_2016, pos_type = position_name)
  position_data_list_2017[[position]] <- get_position_stats(dat_2017, pos_type = position_name)
}

# create a function that loops through the position list and featurizes data
# this is the main function that featurizes player data.
temp_list <- position_data_list_2016
pos_type = 'WR'
i = 1
j = 1
featurize_player_data <- function(temp_list){
  
  # create a result_list
  result_list <- list()
  
  # loop through positions 
  for(i in 1:length(all_positions)){
    
    sub_pos <- temp_list[[i]]
    
    #loop throug players
    for(j in 1:length(sub_pos$player)){
      all_position_players <- unique(sub_pos$player)
      individual_player <- all_position_players[j]
      sub_player <- sub_pos[sub_pos$player == individual_player,]
      # to start: days since last game
      sub_player <- sub_player %>% mutate(last_game=round(c(100,diff(date)), 1))
      
      
      # get cumulative sum of lagged wins and winning percentage 
      sub_player$cum_wins_lag <- cumsum(sub_player$win_ind)
      sub_player$cum_wins_per_lag <- cumsum(sub_player$win_ind)/get_lag_data(sub_player, 'game_num')
      sub_player$cum_wins_per_lag <- ifelse(sub_player$cum_wins_per_lag == 'NaN', 0, sub_player$cum_wins_per_lag)
      
      # create a momentum variable off of lagged cumulative wins
      sub_player$momentum <- diff(c(0,sub_player$cum_wins_per_lag))
      
      # take the inverse
      sub_player$momentum <- ifelse(sub_player$momentum == 0, 0, 1/sub_player$momentum)
      
      # get win streak using "streak" function from functions.R
      sub_player$win_streak <- streak(sub_player$win_loss, value = 'W')
      
      
      # first_downs
      sub_player$mov_avg_first_downs <- movavg(sub_player$first_downs, n = 5, type= 's')
      sub_player$mov_avg_first_downs <- get_lag_data(sub_player,'mov_avg_first_downs')
      
      
      # only keep the variables that are created with the correct format = each row is previous weeks data
      # either in the form of cumulative sums or moving avgerages
      column_string <- c('mov_avg|^cum|win_streak|game_id|lose_streak|win_loss|game_num|last_game|momentumdate|week|player|^venue$')
      sub_player <- sub_player[, grepl(column_string, names(sub_player))]
      
      # store data in data_list
      data_list[[j]] <- sub_player
    }
    
  }
  
  final_data <- do.call('rbind', data_list)
  return(final_data)
}


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

