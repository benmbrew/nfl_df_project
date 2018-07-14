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
# read in nba statistics data
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

# create function to loop through positions and get cumulative and mov average stats for each player
temp_dat <- dat_2016
i = 1
get_position_stats <- function(temp_dat){
  
  # create a column string for each position, that is the position that is not 
  all_cols <- c('date', 'year','week', 'player', 'position','team', 'opponent', 'starter', 'venue', 
                'fumbles', 'fumbles_fl')
  qb_cols <- c('pass_comp', 'pass_att', 'pass_yds', 'pass_td', 'pass_int', 'pass_sack', 'pass_sack_yds_lost', 
               'pass_lg', 'snap_counts_offense', 'snap_counts_offense_pct')
  rb_cols <- c('rush_att', 'rush_yds', 'rush_td', 'rush_lg', 'rec_target', 'rec_reception', 'rec_yds', 
               'rec_td', 'rec_lg', 'snap_counts_offense', 
               'snap_counts_offense_pct')
  wr_cols <- c('rec_target', 'rec_reception', 'rec_yds', 'rec_td', 'rec_lg', 'snap_counts_offense', 
               'snap_counts_offense_pct')
  pr_wr_cols <- c('rec_target', 'rec_reception', 'rec_yds', 'rec_td', 'rec_lg', 'snap_counts_offense', 
                  'snap_counts_offense_pct', 'kick_return', 'kick_return_yds', 'kick_return_td', 
                  'kick_return_lg', 'punt_return', 'punt_return_yds', 'punt_ret_td', 'punt_return_lg')
  te_cols <- c('rec_target', 'rec_reception', 'rec_yds', 'rec_td', 'rec_lg', 'snap_counts_offense', 
               'snap_counts_offense_pct')
  k_cols <- c('scoring_extra_points_made','scoring_extra_points_att', 'fgm', 'fga', 'fgm_0_19',
              'fga_0_19', 'fgm_20_29', 'fgma_20_29', 'fgm_30_39', 'fga_30_39',  'fgm_40_49', 'fga_40_49',
              'fgm_50_plus', 'fga_50_plus')

  # get unique positions
  position_names <- unique(temp_dat$position)
  # creat list to store results
  result_list <- list()
  
  # loop through each name and get historical dat
  for(i in 1:length(position_names)){
    this_position <- position_names[i]
    sub_dat <- temp_dat[temp_dat$position == this_position,]
  }
  
 
  
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

