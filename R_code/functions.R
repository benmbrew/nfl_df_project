# load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(ggthemes)
library(smooth)
library(utils)
library(pracma)
library(stringdist)

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


# create a function that uses lag to get previous weeks data 

get_lag_data <- function(column_variable){
  column_variable <- lag(as.numeric(column_variable))
  column_variable <- ifelse(is.na(column_variable), 0, column_variable)
  
  return(column_variable)
}

# create function to find winning streaks
streak <- function(x, value = 0)  {
  x <- x == value
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  final_x <- x*cumsum(z)
  final_x <- final_x[-length(final_x)]
  final_x <- c(0, final_x)
  return(final_x)
}

# this is the main function that featurizes team data .
featurize_team_data <- function(temp_dat){
  # get a vector of team names to loop through
  unique_teams <- unique(temp_dat$team)
  # loop through unique teams and grab sub team and opponenet data to featurize
  data_list <- list()
  for(i in 1:length(unique_teams)){
    # get team name and subset
    this_team <- unique_teams[i]
    sub_team <- temp_dat[temp_dat$team == this_team, ]
    sub_team_name <- unique(sub_team$team)
    
    message('Creating features for ', sub_team_name)
    
    # begin generating features
    # to start: days since last game
    sub_team <- sub_team %>% mutate(last_game=round(c(100,diff(date)), 1))
    
    # create a numeric win column and use the lag function 
    sub_team$win_ind <- ifelse(sub_team$win_loss == 'W', 1, 0 )
    sub_team$win_ind <- get_lag_data(sub_team$win_ind)
    
    # create a numeric venue column and use the lag function 
    sub_team$venue_ind <- ifelse(sub_team$venue == 'Home', 1, 0 )
    sub_team$venue_ind <- get_lag_data(sub_team$venue_ind)
    
    # get cumulative sum of lagged wins and winning percentage 
    sub_team$cum_wins_lag <- cumsum(sub_team$win_ind)
    sub_team$cum_wins_per_lag <- cumsum(sub_team$win_ind)/get_lag_data(sub_team$game_num)
    sub_team$cum_wins_per_lag <- ifelse(sub_team$cum_wins_per_lag == 'NaN', 0, sub_team$cum_wins_per_lag)
    
    # get cumulative sum of points scored and points allowed 
    sub_team$cum_points <- cumsum(sub_team$final)
    sub_team$cum_points <- get_lag_data(sub_team$cum_points)
    sub_team$cum_points_allowed_by_def <- cumsum(sub_team$points_allowed_by_def)
    sub_team$cum_points_allowed_by_def <- get_lag_data(sub_team$cum_points_allowed_by_def)
    
    # get cumulative sum of yds 
    sub_team$cum_total_yds <- cumsum(sub_team$total_yds)
    sub_team$cum_total_yds <- get_lag_data(sub_team$cum_total_yds)
    
    # create a momentum variable off of lagged cumulative wins
    sub_team$momentum <- diff(c(0,sub_team$cum_wins_per_lag))
    
    # take the inverse
    sub_team$momentum <- ifelse(sub_team$momentum == 0, 0, 1/sub_team$momentum)
    
    # get win streak using "streak" function from functions.R
    sub_team$win_streak <- streak(sub_team$win_loss, value = 'W')
    
    # get losing streak
    sub_team$lose_streak <- streak(sub_team$win_loss, value = 'L')
    
    # moving average for first downs,rush_yds, rush_tds, pass_comp, pass_yds, pass_tds, qb_interceptions, qb_sacked, 
    # fumbles, turnovers, penalties, 3rd (third_downs_made, third_down_att) and 4th,
    # def_sack, def_interceptions
    
    # first_downs
    sub_team$mov_avg_first_downs <- movavg(sub_team$first_downs, n = 5, type= 's')
    sub_team$mov_avg_first_downs <- get_lag_data(sub_team$mov_avg_first_downs)
    
    # rush_yds
    sub_team$mov_avg_rush_yds <- movavg(sub_team$rush_yds, n = 5, type = 's')
    sub_team$mov_avg_rush_yds <- get_lag_data(sub_team$mov_avg_rush_yds)
    
    # rush_tds
    sub_team$mov_avg_rush_tds <- movavg(sub_team$rush_tds, n = 5, type = 's')
    sub_team$mov_avg_rush_tds <- get_lag_data(sub_team$mov_avg_rush_tds)
    
    # pass_comp
    sub_team$mov_avg_pass_comp <- movavg(sub_team$pass_comp, n = 5, type = 's')
    sub_team$mov_avg_pass_comp <- get_lag_data(sub_team$mov_avg_pass_comp)
    
    # pass_yds
    sub_team$mov_avg_pass_yds <- movavg(sub_team$pass_yds, n = 5, type = 's')
    sub_team$mov_avg_pass_yds <- get_lag_data(sub_team$mov_avg_pass_yds)
    
    # pass_tds
    sub_team$mov_avg_pass_tds <- movavg(sub_team$pass_tds, n = 5, type = 's')
    sub_team$mov_avg_pass_tds <- get_lag_data(sub_team$mov_avg_pass_tds)
    
    # qb_interceptions
    sub_team$mov_avg_qb_interceptions <- movavg(sub_team$qb_interceptions, n = 5, type = 's')
    sub_team$mov_avg_qb_interceptions <- get_lag_data(sub_team$mov_avg_qb_interceptions)
    
    # qb_sacked
    sub_team$mov_avg_qb_sacked <- movavg(sub_team$qb_sacked, n = 5, type = 's')
    sub_team$mov_avg_qb_sacked <- get_lag_data(sub_team$mov_avg_qb_sacked)
    
    # fumbles
    sub_team$mov_avg_fumbles <- movavg(sub_team$fumbles, n = 5, type = 's')
    sub_team$mov_avg_fumbles <- get_lag_data(sub_team$mov_avg_fumbles)
    
    # turnovers
    sub_team$mov_avg_turnovers <- movavg(sub_team$turnovers, n = 5, type = 's')
    sub_team$mov_avg_turnovers <- get_lag_data(sub_team$mov_avg_turnovers)
    
    # penalties
    sub_team$mov_avg_penalties <- movavg(sub_team$penalties , n = 5, type = 's')
    sub_team$mov_avg_penalties <- get_lag_data(sub_team$mov_avg_penalties)
    
    # def_interception
    sub_team$mov_avg_def_interception <- movavg(sub_team$def_interception , n = 5, type = 's')
    sub_team$mov_avg_def_interception <- get_lag_data(sub_team$mov_avg_def_interception)
    
    # def_sack
    sub_team$mov_avg_def_sack <- movavg(sub_team$def_sack , n = 5, type = 's')
    sub_team$mov_avg_def_sack <- get_lag_data(sub_team$mov_avg_def_sack)
    
    # first
    sub_team$mov_avg_first <- movavg(sub_team$first , n = 5, type = 's')
    sub_team$mov_avg_first <- get_lag_data(sub_team$mov_avg_first)
    
    # second
    sub_team$mov_avg_second <- movavg(sub_team$second , n = 5, type = 's')
    sub_team$mov_avg_second <- get_lag_data(sub_team$mov_avg_second)
    
    # third
    sub_team$mov_avg_third <- movavg(sub_team$third , n = 5, type = 's')
    sub_team$mov_avg_third <- get_lag_data(sub_team$mov_avg_third)
    
    # fourth
    sub_team$mov_avg_fourth <- movavg(sub_team$fourth , n = 5, type = 's')
    sub_team$mov_avg_fourth <- get_lag_data(sub_team$mov_avg_fourth)
    
    # final
    sub_team$mov_avg_final <- movavg(sub_team$final , n = 5, type = 's')
    sub_team$mov_avg_final <- get_lag_data(sub_team$mov_avg_fourth )
    
    # points_allowed_by_def
    sub_team$mov_avg_points_allowed_by_def <- movavg(sub_team$points_allowed_by_def , n = 5, type = 's')
    sub_team$mov_avg_points_allowed_by_def <- get_lag_data(sub_team$mov_avg_points_allowed_by_def)
    
    # third_downs_made, att, and efficiency
    sub_team$mov_avg_third_down_made <- movavg(sub_team$third_down_made , n = 5, type = 's')
    sub_team$mov_avg_third_down_made <- get_lag_data(sub_team$mov_avg_third_down_made)
    sub_team$mov_avg_third_down_att <- movavg(sub_team$third_down_att , n = 5, type = 's')
    sub_team$mov_avg_third_down_att <- get_lag_data(sub_team$mov_avg_third_down_att )
    sub_team$third_down_per <- round((sub_team$third_down_made/sub_team$third_down_att)*100,2)
    sub_team$mov_avg_third_down_per <- movavg(sub_team$third_down_per , n = 5, type = 's')
    sub_team$mov_avg_third_down_per <- get_lag_data(sub_team$mov_avg_third_down_per)
    
    # fourth_downs_made, att, and efficiency
    sub_team$mov_avg_fourth_down_made <- movavg(sub_team$fourth_down_made , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_made <- get_lag_data(sub_team$mov_avg_fourth_down_made)
    sub_team$mov_avg_fourth_down_att <- movavg(sub_team$fourth_down_att , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_att <- get_lag_data(sub_team$mov_avg_fourth_down_att)
    sub_team$fourth_down_per <- round((sub_team$fourth_down_made/sub_team$fourth_down_att)*100,2)
    sub_team$mov_avg_fourth_down_per <- movavg(sub_team$fourth_down_per , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_per <- get_lag_data(sub_team$mov_avg_fourth_down_per)
    sub_team$fourth_down_per <- sub_team$third_down_per <- NULL
    
    # only keep the variables that are created with the correct format = each row is previous weeks data
    # either in the form of cumulative sums or moving avgerages
    column_string <- c('mov_avg|^cum|win_streak|game_id|lose_streak|win_loss|game_num|last_game|momentumdate|week|team|^venue$')
    sub_team <- sub_team[, grepl(column_string, names(sub_team))]
    
    # store data in data_list
    data_list[[i]] <- sub_team
    
  }
  
  final_data <- do.call('rbind', data_list)
  return(final_data)
}




# creat function to go through each game id and get data on opposing team
get_opposing_team_stats <- function(temp_dat){
  result_list <- list()
  
  for(i in 1:nrow(temp_dat)){
    sub_dat <- temp_dat[temp_dat$game_id == i, ]
    sub_dat$dup_ind <- duplicated(sub_dat$game_id)
    sub_dat_1 <- sub_dat[sub_dat$dup_ind == TRUE,]
    sub_dat_2 <- sub_dat[sub_dat$dup_ind == FALSE,]
    
    # grab features for sub_dat_1
    sub_dat_1$team_opp <- sub_dat_2$team
    sub_dat_1$last_game_opp <- sub_dat_2$last_game
    sub_dat_1$last_game_opp <- sub_dat_2$last_game
    sub_dat_1$cum_wins_lag_opp <- sub_dat_2$cum_wins_lag
    sub_dat_1$cum_wins_per_lag_opp <- sub_dat_2$cum_wins_per_lag
    sub_dat_1$cum_points_opp <- sub_dat_2$cum_points
    sub_dat_1$cum_points_allowed_by_def_opp <- sub_dat_2$cum_points_allowed_by_def
    sub_dat_1$cum_total_yds_opp <- sub_dat_2$cum_total_yds
    sub_dat_1$win_streak_opp <- sub_dat_2$win_streak
    sub_dat_1$lose_streak_opp <- sub_dat_2$lose_streak
    sub_dat_1$mov_avg_first_downs_opp <- sub_dat_2$mov_avg_first_downs
    sub_dat_1$mov_avg_rush_yds_opp <- sub_dat_2$mov_avg_rush_yds
    sub_dat_1$mov_avg_rush_tds_opp <- sub_dat_2$mov_avg_rush_tds
    sub_dat_1$mov_avg_pass_comp_opp <- sub_dat_2$mov_avg_pass_comp
    sub_dat_1$mov_avg_pass_yds_opp <- sub_dat_2$mov_avg_pass_yds
    sub_dat_1$mov_avg_pass_tds_opp <- sub_dat_2$mov_avg_pass_tds 
    sub_dat_1$mov_avg_qb_interceptions_opp <- sub_dat_2$mov_avg_qb_interceptions
    sub_dat_1$mov_avg_qb_sacked_opp <- sub_dat_2$mov_avg_qb_sacked
    sub_dat_1$mov_avg_fumbles_opp <- sub_dat_2$mov_avg_fumbles
    sub_dat_1$mov_avg_turnovers_opp <- sub_dat_2$mov_avg_tunorvers
    sub_dat_1$mov_avg_penalties_opp <- sub_dat_2$mov_avg_penalties
    sub_dat_1$mov_avg_def_interception_opp <- sub_dat_2$mov_avg_def_interception
    sub_dat_1$mov_avg_def_sack_opp <- sub_dat_2$mov_avg_def_sack
    sub_dat_1$mov_avg_first_opp <- sub_dat_2$mov_avg_first
    sub_dat_1$mov_avg_second_opp <- sub_dat_2$mov_avg_second
    sub_dat_1$mov_avg_third_opp <- sub_dat_2$mov_avg_third
    sub_dat_1$mov_avg_fourth_opp <- sub_dat_2$mov_avg_fourth
    sub_dat_1$mov_avg_final_opp <- sub_dat_2$mov_avg_final
    sub_dat_1$mov_avg_points_allowed_by_def_opp <- sub_dat_2$mov_avg_points_allowed_by_def
    sub_dat_1$mov_avg_third_down_made_opp <- sub_dat_2$mov_avg_third_down_made
    sub_dat_1$mov_avg_third_down_att_opp <- sub_dat_2$mov_avg_third_down_att
    sub_dat_1$mov_avg_third_down_per_opp <- sub_dat_2$mov_avg_third_down_per
    sub_dat_1$mov_avg_fourth_down_made_opp <- sub_dat_2$mov_avg_fourth_down_made
    sub_dat_1$mov_avg_fourth_down_att_opp <- sub_dat_2$mov_avg_fourth_down_att
    sub_dat_1$mov_avg_fourth_down_per_opp <- sub_dat_2$mov_avg_fourth_down_per
    
    # grab features for sub_dat_2
    sub_dat_2$team_opp <- sub_dat_1$team
    sub_dat_2$last_game_opp <- sub_dat_1$last_game
    sub_dat_2$last_game_opp <- sub_dat_1$last_game
    sub_dat_2$cum_wins_lag_opp <- sub_dat_1$cum_wins_lag
    sub_dat_2$cum_wins_per_lag_opp <- sub_dat_1$cum_wins_per_lag
    sub_dat_2$cum_points_opp <- sub_dat_1$cum_points
    sub_dat_2$cum_points_allowed_by_def_opp <- sub_dat_1$cum_points_allowed_by_def
    sub_dat_2$cum_total_yds_opp <- sub_dat_1$cum_total_yds
    sub_dat_2$win_streak_opp <- sub_dat_1$win_streak
    sub_dat_2$lose_streak_opp <- sub_dat_1$lose_streak
    sub_dat_2$mov_avg_first_downs_opp <- sub_dat_1$mov_avg_first_downs
    sub_dat_2$mov_avg_rush_yds_opp <- sub_dat_1$mov_avg_rush_yds
    sub_dat_2$mov_avg_rush_tds_opp <- sub_dat_1$mov_avg_rush_tds
    sub_dat_2$mov_avg_pass_comp_opp <- sub_dat_1$mov_avg_pass_comp
    sub_dat_2$mov_avg_pass_yds_opp <- sub_dat_1$mov_avg_pass_yds
    sub_dat_2$mov_avg_pass_tds_opp <- sub_dat_1$mov_avg_pass_tds 
    sub_dat_2$mov_avg_qb_interceptions_opp <- sub_dat_1$mov_avg_qb_interceptions
    sub_dat_2$mov_avg_qb_sacked_opp <- sub_dat_1$mov_avg_qb_sacked
    sub_dat_2$mov_avg_fumbles_opp <- sub_dat_1$mov_avg_fumbles
    sub_dat_2$mov_avg_turnovers_opp <- sub_dat_1$mov_avg_tunorvers
    sub_dat_2$mov_avg_penalties_opp <- sub_dat_1$mov_avg_penalties
    sub_dat_2$mov_avg_def_interception_opp <- sub_dat_1$mov_avg_def_interception
    sub_dat_2$mov_avg_def_sack_opp <- sub_dat_1$mov_avg_def_sack
    sub_dat_2$mov_avg_first_opp <- sub_dat_1$mov_avg_first
    sub_dat_2$mov_avg_second_opp <- sub_dat_1$mov_avg_second
    sub_dat_2$mov_avg_third_opp <- sub_dat_1$mov_avg_third
    sub_dat_2$mov_avg_fourth_opp <- sub_dat_1$mov_avg_fourth
    sub_dat_2$mov_avg_final_opp <- sub_dat_1$mov_avg_final
    sub_dat_2$mov_avg_points_allowed_by_def_opp <- sub_dat_1$mov_avg_points_allowed_by_def
    sub_dat_2$mov_avg_third_down_made_opp <- sub_dat_1$mov_avg_third_down_made
    sub_dat_2$mov_avg_third_down_att_opp <- sub_dat_1$mov_avg_third_down_att
    sub_dat_2$mov_avg_third_down_per_opp <- sub_dat_1$mov_avg_third_down_per
    sub_dat_2$mov_avg_fourth_down_made_opp <- sub_dat_1$mov_avg_fourth_down_made
    sub_dat_2$mov_avg_fourth_down_att_opp <- sub_dat_1$mov_avg_fourth_down_att
    sub_dat_2$mov_avg_fourth_down_per_opp <- sub_dat_1$mov_avg_fourth_down_per
    
    # combine date frames and store in list result_list
    sub_dat_both <- rbind(sub_dat_1,
                          sub_dat_2)
    
    result_list[[i]] <- sub_dat_both
    
    message('finished with game_id = ', i)
    
  }
  
  final_data <- do.call('rbind', result_list)
  return(final_data)
}

# function to remove players that have less than 4 games in a season
remove_low_frequency_players <- function(temp_dat){
  temp_dat <- temp_dat %>% 
    group_by(player) %>%
    mutate(counts = n()) %>%
    filter(counts >= 4)
  
  return(temp_dat)
  
}


# create function to loop through positions and get cumulative and mov average stats for each player
get_position_stats <- function(temp_dat, pos_type){
  
  
  # create a column string for each position, that is the position that is not 
  all_cols <- c('date', 'year','week', 'player', 'position','team', 'opponent', 'starter', 'venue', 
                'fumbles', 'fumbles_fl')
  QB <- c('pass_comp', 'pass_att', 'pass_yds', 'pass_td', 'pass_int', 'pass_sack', 'pass_sack_yds_lost', 
          'pass_lg', 'snap_counts_offense', 'snap_counts_offense_pct')
  RB<- c('rush_att', 'rush_yds', 'rush_td', 'rush_lg', 'rec_target', 'rec_reception', 'rec_yds', 
         'rec_td', 'rec_lg', 'snap_counts_offense', 
         'snap_counts_offense_pct')
  WR <- c('rec_target', 'rec_reception', 'rec_yds', 'rec_td', 'rec_lg', 'snap_counts_offense', 
          'snap_counts_offense_pct', 'kick_return', 'kick_return_yds', 'kick_return_td', 
          'kick_return_lg', 'punt_return', 'punt_return_yds', 'punt_ret_td', 'punt_return_lg')
  TE <- c('rec_target', 'rec_reception', 'rec_yds', 'rec_td', 'rec_lg', 'snap_counts_offense', 
          'snap_counts_offense_pct')
  K <- c('scoring_extra_points_made','scoring_extra_points_att', 'fgm', 'fga', 'fgm_0_19',
         'fga_0_19', 'fgm_20_29', 'fga_20_29', 'fgm_30_39', 'fga_30_39',  'fgm_40_49', 'fga_40_49',
         'fgm_50_plus', 'fga_50_plus')
  
  
  # subset by position
  sub_dat <- temp_dat[temp_dat$position == pos_type,]
  
  # get complete data - HERE why isthis not working?!?!
  complete_ind <- rowSums(is.na(sub_dat[, 9:(ncol(sub_dat) - 7)])) != ncol(sub_dat[, 9:(ncol(sub_dat) - 7)])
  
  # get column names for that position
  sub_dat <- sub_dat[complete_ind,]
  
  # select columns by position, using condition
  if(pos_type == 'QB'){
    sub_dat <- sub_dat[, c(all_cols, QB)]
  } else if(pos_type == 'WR'){
    sub_dat <- sub_dat[, c(all_cols, WR)]
  } else if(pos_type == 'RB'){
    sub_dat <- sub_dat[, c(all_cols, RB)]
  } else if(pos_type == 'TE'){
    sub_dat <- sub_dat[, c(all_cols, TE)]
  } else if(pos_type == 'K'){
    sub_dat <- sub_dat[, c(all_cols, K)]
  } 

  return(sub_dat)
  
}


# function for featurizing wr data
get_wr_data <- function(temp_player_dat){
  
  # to start: days since last game
  temp_player_dat <- temp_player_dat %>% mutate(last_game=round(c(100,diff(date)), 1))
  
  # get starting streak and H/A streak
  temp_player_dat$starter_streak <- streak(temp_player_dat$starter, value = 'Y')
  temp_player_dat$venue_streak <- streak(temp_player_dat$venue, value = 'Home')
  # FUMBLES
  # fumbles, fumbles_fl
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles <- cumsum(temp_player_dat$fumbles)
  temp_player_dat$cum_sum_fumbles <- get_lag_data(temp_player_dat$cum_sum_fumbles)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles <- movavg(temp_player_dat$fumbles, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles <- get_lag_data(temp_player_dat$mov_avg_fumbles)
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles_fl <- cumsum(temp_player_dat$fumbles_fl)
  temp_player_dat$cum_sum_fumbles_fl <- get_lag_data(temp_player_dat$cum_sum_fumbles_fl)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles_fl <- movavg(temp_player_dat$fumbles_fl, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles_fl <- get_lag_data(temp_player_dat$mov_avg_fumbles_fl)
  
  # RECEPTIONS
  # rec_target, rec_reception, rec_yds, rec_td, rec_lg,
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_target <- cumsum(temp_player_dat$rec_target)
  temp_player_dat$cum_sum_rec_target <- get_lag_data(temp_player_dat$cum_sum_rec_target)
  
  # moving average
  temp_player_dat$mov_avg_rec_target <- movavg(temp_player_dat$rec_target, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_target <- get_lag_data(temp_player_dat$mov_avg_rec_target)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_reception <- cumsum(temp_player_dat$rec_reception)
  temp_player_dat$cum_sum_rec_reception <- get_lag_data(temp_player_dat$cum_sum_rec_reception)
  
  # moving average
  temp_player_dat$mov_avg_rec_reception <- movavg(temp_player_dat$rec_reception, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_reception <- get_lag_data(temp_player_dat$mov_avg_rec_reception)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_yds <- cumsum(temp_player_dat$rec_yds)
  temp_player_dat$cum_sum_rec_yds <- get_lag_data(temp_player_dat$cum_sum_rec_yds)
  
  # moving average
  temp_player_dat$mov_avg_rec_yds <- movavg(temp_player_dat$rec_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_yds <- get_lag_data(temp_player_dat$mov_avg_rec_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_lg <- cumsum(temp_player_dat$rec_lg)
  temp_player_dat$cum_sum_rec_lg <- get_lag_data(temp_player_dat$cum_sum_rec_lg)
  
  # moving average
  temp_player_dat$mov_avg_rec_lg <- movavg(temp_player_dat$rec_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_lg <- get_lag_data( temp_player_dat$mov_avg_rec_lg)
  
  # SNAP COUNTS
  # snap_counts_offense, snap_counts_offense_pct,
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense <- cumsum(temp_player_dat$snap_counts_offense)
  temp_player_dat$cum_sum_snap_counts_offense <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense_pct <- cumsum(temp_player_dat$snap_counts_offense_pct)
  temp_player_dat$cum_sum_snap_counts_offense_pct <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense_pct)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  # KICK RETURNS
  # kick_return, kick_return_yds, kick_return_td, 
  # kick_return_lg
  
  # cumulative sum
  temp_player_dat$cum_sum_kick_return <- cumsum(temp_player_dat$kick_return)
  temp_player_dat$cum_sum_kick_return <- get_lag_data(temp_player_dat$cum_sum_kick_return)
  
  # moving avg
  temp_player_dat$mov_avg_kick_return <- movavg(temp_player_dat$kick_return, n = 3, type = 's')
  temp_player_dat$mov_avg_kick_return <- get_lag_data(temp_player_dat$mov_avg_kick_return)
  
  # cumulative sum
  temp_player_dat$cum_sum_kick_return_yds <- cumsum(temp_player_dat$kick_return_yds)
  temp_player_dat$cum_sum_kick_return_yds <- get_lag_data(temp_player_dat$cum_sum_kick_return_yds)
  
  # moving avg
  temp_player_dat$mov_avg_kick_return_yds <- movavg(temp_player_dat$kick_return_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_kick_return_yds <- get_lag_data(temp_player_dat$mov_avg_kick_return_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_kick_return_td <- cumsum(temp_player_dat$kick_return_td)
  temp_player_dat$cum_sum_kick_return_td <- get_lag_data(temp_player_dat$cum_sum_kick_return_td)
  
  # moving avg
  temp_player_dat$mov_avg_kick_return_td <- movavg(temp_player_dat$kick_return_td, n = 3, type = 's')
  temp_player_dat$mov_avg_kick_return_td <- get_lag_data(temp_player_dat$mov_avg_kick_return_td)
  
  # cumulative sum
  temp_player_dat$cum_sum_kick_return_lg <- cumsum(temp_player_dat$kick_return_lg)
  temp_player_dat$cum_sum_kick_return_lg <- get_lag_data(temp_player_dat$cum_sum_kick_return_lg)
  
  # moving avg
  temp_player_dat$mov_avg_kick_return_lg <- movavg(temp_player_dat$kick_return_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_kick_return_lg <- get_lag_data(temp_player_dat$mov_avg_kick_return_lg)
  
  # PUNTS
  # punt_return, punt_return_yds, punt_ret_td, punt_return_lg
  
  # cumulative sum
  temp_player_dat$cum_sum_punt_return <- cumsum(temp_player_dat$punt_return)
  temp_player_dat$cum_sum_punt_return <- get_lag_data(temp_player_dat$cum_sum_punt_return)
  
  # moving avg
  temp_player_dat$mov_avg_punt_return <- movavg(temp_player_dat$punt_return, n = 3, type = 's')
  temp_player_dat$mov_avg_punt_return <- get_lag_data(temp_player_dat$mov_avg_punt_return )
  
  # cumulative sum
  temp_player_dat$cum_sum_punt_return_yds <- cumsum(temp_player_dat$punt_return_yds)
  temp_player_dat$cum_sum_punt_return_yds <- get_lag_data(temp_player_dat$cum_sum_punt_return_yds)
  
  # moving avg
  temp_player_dat$mov_avg_punt_return_yds <- movavg(temp_player_dat$punt_return_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_punt_return_yds <- get_lag_data(temp_player_dat$mov_avg_punt_return_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_punt_return_td <- cumsum(temp_player_dat$punt_ret_td)
  temp_player_dat$cum_sum_punt_return_td <- get_lag_data(temp_player_dat$cum_sum_punt_return_td)
  
  # moving avg
  temp_player_dat$mov_avg_punt_return_td <- movavg(temp_player_dat$punt_ret_td, n = 3, type = 's')
  temp_player_dat$mov_avg_punt_return_td <- get_lag_data(temp_player_dat$mov_avg_punt_return_td)
  
  # cumulative sum
  temp_player_dat$cum_sum_punt_return_lg <- cumsum(temp_player_dat$punt_return_lg)
  temp_player_dat$cum_sum_punt_return_lg <- get_lag_data(temp_player_dat$cum_sum_punt_return_lg)
  
  # moving avg
  temp_player_dat$mov_avg_punt_return_lg <- movavg(temp_player_dat$punt_return_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_punt_return_lg <- get_lag_data(temp_player_dat$mov_avg_punt_return_lg)
  
  # remove static variables 
  temp_player_dat$starter <- temp_player_dat$venue <- temp_player_dat$fumbles <- temp_player_dat$fumbles_fl <-
    temp_player_dat$rec_target <- temp_player_dat$rec_reception <- temp_player_dat$rec_yds <- temp_player_dat$rec_td <- 
    temp_player_dat$rec_lg <- temp_player_dat$snap_counts_offense <- temp_player_dat$snap_counts_offense_pct <- 
    temp_player_dat$kick_return <- temp_player_dat$kick_return_yds <- temp_player_dat$kick_return_td <- 
    temp_player_dat$kick_return_lg <- temp_player_dat$punt_ret_td <- temp_player_dat$punt_return <- 
    temp_player_dat$punt_return_yds <- temp_player_dat$punt_return_lg <- NULL
  
  return(temp_player_dat)
}


# function for featurizing qb data
get_qb_data <- function(temp_player_dat){
  
  # to start: days since last game
  temp_player_dat <- temp_player_dat %>% mutate(last_game=round(c(100,diff(date)), 1))
  
  # get starting streak and H/A streak
  temp_player_dat$starter_streak <- streak(temp_player_dat$starter, value = 'Y')
  temp_player_dat$venue_streak <- streak(temp_player_dat$venue, value = 'Home')
  # FUMBLES
  # fumbles, fumbles_fl
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles <- cumsum(temp_player_dat$fumbles)
  temp_player_dat$cum_sum_fumbles <- get_lag_data(temp_player_dat$cum_sum_fumbles)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles <- movavg(temp_player_dat$fumbles, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles <- get_lag_data(temp_player_dat$mov_avg_fumbles)
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles_fl <- cumsum(temp_player_dat$fumbles_fl)
  temp_player_dat$cum_sum_fumbles_fl <- get_lag_data(temp_player_dat$cum_sum_fumbles_fl)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles_fl <- movavg(temp_player_dat$fumbles_fl, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles_fl <- get_lag_data(temp_player_dat$mov_avg_fumbles_fl)
  
  # PASSING 
  # pass_comp, pass_att, pass_yds, pass_td, pass_int, pass_sack, pass_sack_yds_lost, pass_lg
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_comp <- cumsum(temp_player_dat$pass_comp)
  temp_player_dat$cum_sum_pass_comp <- get_lag_data(temp_player_dat$cum_sum_pass_comp)
  
  # moving avg
  temp_player_dat$mov_avg_pass_comp <- movavg(temp_player_dat$pass_comp, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_comp <- get_lag_data(temp_player_dat$mov_avg_pass_comp)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_att <- cumsum(temp_player_dat$pass_att)
  temp_player_dat$cum_sum_pass_att <- get_lag_data(temp_player_dat$cum_sum_pass_att)
  
  # moving avg
  temp_player_dat$mov_avg_pass_att <- movavg(temp_player_dat$pass_att, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_att <- get_lag_data(temp_player_dat$mov_avg_pass_att)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_yds <- cumsum(temp_player_dat$pass_yds)
  temp_player_dat$cum_sum_pass_yds <- get_lag_data(temp_player_dat$cum_sum_pass_yds)
  
  # moving avg
  temp_player_dat$mov_avg_pass_yds <- movavg(temp_player_dat$pass_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_yds <- get_lag_data(temp_player_dat$mov_avg_pass_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_td <- cumsum(temp_player_dat$pass_td)
  temp_player_dat$cum_sum_pass_td <- get_lag_data(temp_player_dat$cum_sum_pass_td)
  
  # moving avg
  temp_player_dat$mov_avg_pass_td <- movavg(temp_player_dat$pass_td, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_td <- get_lag_data(temp_player_dat$mov_avg_pass_td)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_int <- cumsum(temp_player_dat$pass_int)
  temp_player_dat$cum_sum_pass_int <- get_lag_data(temp_player_dat$cum_sum_pass_int)
  
  # moving avg
  temp_player_dat$mov_avg_pass_int <- movavg(temp_player_dat$pass_int, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_int <- get_lag_data(temp_player_dat$mov_avg_pass_int)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_sack <- cumsum(temp_player_dat$pass_sack)
  temp_player_dat$cum_sum_pass_sack <- get_lag_data(temp_player_dat$cum_sum_pass_sack)
  
  # moving avg
  temp_player_dat$mov_avg_pass_sack <- movavg(temp_player_dat$pass_sack, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_sack <- get_lag_data(temp_player_dat$mov_avg_pass_sack)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_sack_yds_lost <- cumsum(temp_player_dat$pass_sack_yds_lost)
  temp_player_dat$cum_sum_pass_sack_yds_lost <- get_lag_data(temp_player_dat$cum_sum_pass_sack_yds_lost)
  
  # moving avg
  temp_player_dat$mov_avg_pass_sack_yds_lost <- movavg(temp_player_dat$pass_sack_yds_lost, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_sack_yds_lost <- get_lag_data(temp_player_dat$mov_avg_pass_sack_yds_lost)
  
  # cumulative sum
  temp_player_dat$cum_sum_pass_lg <- cumsum(temp_player_dat$pass_lg)
  temp_player_dat$cum_sum_pass_lg <- get_lag_data(temp_player_dat$cum_sum_pass_lg)
  
  # moving avg
  temp_player_dat$mov_avg_pass_lg <- movavg(temp_player_dat$pass_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_pass_lg <- get_lag_data(temp_player_dat$mov_avg_pass_lg)
  
  

  # SNAP COUNTS
  # snap_counts_offense, snap_counts_offense_pct,
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense <- cumsum(temp_player_dat$snap_counts_offense)
  temp_player_dat$cum_sum_snap_counts_offense <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense_pct <- cumsum(temp_player_dat$snap_counts_offense_pct)
  temp_player_dat$cum_sum_snap_counts_offense_pct <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense_pct)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  
  # remove static variables 
  temp_player_dat$starter <- temp_player_dat$venue <- temp_player_dat$fumbles <- temp_player_dat$fumbles_fl <- 
    temp_player_dat$pass_comp  <- temp_player_dat$pass_att  <- temp_player_dat$pass_yds <- 
    temp_player_dat$pass_td <- temp_player_dat$pass_int <- temp_player_dat$pass_sack  <- temp_player_dat$pass_sack_yds_lost <-
    temp_player_dat$pass_lg  <- temp_player_dat$snap_counts_offense <- temp_player_dat$snap_counts_offense_pct <- NULL
  
  
  return(temp_player_dat)
}


# function for featurizing qb data
get_te_data <- function(temp_player_dat){
  
  # to start: days since last game
  temp_player_dat <- temp_player_dat %>% mutate(last_game=round(c(100,diff(date)), 1))
  
  # get starting streak and H/A streak
  temp_player_dat$starter_streak <- streak(temp_player_dat$starter, value = 'Y')
  temp_player_dat$venue_streak <- streak(temp_player_dat$venue, value = 'Home')
  # FUMBLES
  # fumbles, fumbles_fl
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles <- cumsum(temp_player_dat$fumbles)
  temp_player_dat$cum_sum_fumbles <- get_lag_data(temp_player_dat$cum_sum_fumbles)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles <- movavg(temp_player_dat$fumbles, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles <- get_lag_data(temp_player_dat$mov_avg_fumbles)
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles_fl <- cumsum(temp_player_dat$fumbles_fl)
  temp_player_dat$cum_sum_fumbles_fl <- get_lag_data(temp_player_dat$cum_sum_fumbles_fl)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles_fl <- movavg(temp_player_dat$fumbles_fl, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles_fl <- get_lag_data(temp_player_dat$mov_avg_fumbles_fl)
  
  # RECEPTIONS
  # rec_target, rec_reception, rec_yds, rec_td, rec_lg,
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_target <- cumsum(temp_player_dat$rec_target)
  temp_player_dat$cum_sum_rec_target <- get_lag_data(temp_player_dat$cum_sum_rec_target)
  
  # moving average
  temp_player_dat$mov_avg_rec_target <- movavg(temp_player_dat$rec_target, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_target <- get_lag_data(temp_player_dat$mov_avg_rec_target)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_reception <- cumsum(temp_player_dat$rec_reception)
  temp_player_dat$cum_sum_rec_reception <- get_lag_data(temp_player_dat$cum_sum_rec_reception)
  
  # moving average
  temp_player_dat$mov_avg_rec_reception <- movavg(temp_player_dat$rec_reception, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_reception <- get_lag_data(temp_player_dat$mov_avg_rec_reception)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_yds <- cumsum(temp_player_dat$rec_yds)
  temp_player_dat$cum_sum_rec_yds <- get_lag_data(temp_player_dat$cum_sum_rec_yds)
  
  # moving average
  temp_player_dat$mov_avg_rec_yds <- movavg(temp_player_dat$rec_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_yds <- get_lag_data(temp_player_dat$mov_avg_rec_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_lg <- cumsum(temp_player_dat$rec_lg)
  temp_player_dat$cum_sum_rec_lg <- get_lag_data(temp_player_dat$cum_sum_rec_lg)
  
  # moving average
  temp_player_dat$mov_avg_rec_lg <- movavg(temp_player_dat$rec_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_lg <- get_lag_data( temp_player_dat$mov_avg_rec_lg)
  
 # SNAP COUNTS
  # snap_counts_offense, snap_counts_offense_pct,
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense <- cumsum(temp_player_dat$snap_counts_offense)
  temp_player_dat$cum_sum_snap_counts_offense <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense_pct <- cumsum(temp_player_dat$snap_counts_offense_pct)
  temp_player_dat$cum_sum_snap_counts_offense_pct <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense_pct)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  
  # remove static variables 
  temp_player_dat$starter <- temp_player_dat$venue <- temp_player_dat$fumbles <- temp_player_dat$fumbles_fl <-
    temp_player_dat$rec_target <- temp_player_dat$rec_reception <- temp_player_dat$rec_yds <- temp_player_dat$rec_td <- 
    temp_player_dat$rec_lg <- temp_player_dat$snap_counts_offense <- temp_player_dat$snap_counts_offense_pct <- 
    NULL
  
  return(temp_player_dat)
}


# function for featurizing k data
get_k_data <- function(temp_player_dat){
  
  # 0_19 is a character, change to numeric
  temp_player_dat$fga_0_19 <- as.numeric(temp_player_dat$fga_0_19)
  temp_player_dat$fgm_0_19 <- as.numeric(temp_player_dat$fgm_0_19)
  
  # to start: days since last game
  temp_player_dat <- temp_player_dat %>% mutate(last_game=round(c(100,diff(date)), 1))
  
  # get starting streak and H/A streak
  temp_player_dat$starter_streak <- streak(temp_player_dat$starter, value = 'Y')
  temp_player_dat$venue_streak <- streak(temp_player_dat$venue, value = 'Home')
  
  # cumulative sum
  temp_player_dat$cum_sum_scoring_extra_points_made <- cumsum(temp_player_dat$scoring_extra_points_made)
  temp_player_dat$cum_sum_scoring_extra_points_made <- get_lag_data(temp_player_dat$cum_sum_scoring_extra_points_made)
  
  # moving avg
  temp_player_dat$mov_avg_scoring_extra_points_made <- movavg(temp_player_dat$scoring_extra_points_made, n = 3, type = 's')
  temp_player_dat$mov_avg_scoring_extra_points_made <- get_lag_data(temp_player_dat$mov_avg_scoring_extra_points_made)
  
  # cumulative sum
  temp_player_dat$cum_sum_scoring_extra_points_att <- cumsum(temp_player_dat$scoring_extra_points_att)
  temp_player_dat$cum_sum_scoring_extra_points_att <- get_lag_data(temp_player_dat$cum_sum_scoring_extra_points_att)
  
  # moving avg
  temp_player_dat$mov_avg_scoring_extra_points_att <- movavg(temp_player_dat$scoring_extra_points_att, n = 3, type = 's')
  temp_player_dat$mov_avg_scoring_extra_points_att <- get_lag_data(temp_player_dat$mov_avg_scoring_extra_points_att)
  
  # cumulative sum
  temp_player_dat$cum_sum_fga <- cumsum(temp_player_dat$fga)
  temp_player_dat$cum_sum_fga <- get_lag_data(temp_player_dat$cum_sum_fga)
  
  # moving avg
  temp_player_dat$mov_avg_fga <- movavg(temp_player_dat$fga, n = 3, type = 's')
  temp_player_dat$mov_avg_fga <- get_lag_data(temp_player_dat$mov_avg_fga)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm <- cumsum(temp_player_dat$fgm)
  temp_player_dat$cum_sum_fgm <- get_lag_data(temp_player_dat$cum_sum_fgm)
  
  # moving avg
  temp_player_dat$mov_avg_fgm <- movavg(temp_player_dat$fgm, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm <- get_lag_data(temp_player_dat$mov_avg_fgm)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm_0_19 <- cumsum(temp_player_dat$fgm_0_19)
  temp_player_dat$cum_sum_fgm_0_19 <- get_lag_data(temp_player_dat$cum_sum_fgm_0_19)
  
  # moving avg
  temp_player_dat$mov_avg_fgm_0_19 <- movavg(temp_player_dat$fgm_0_19, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm_0_19 <- get_lag_data(temp_player_dat$mov_avg_fgm_0_19)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm_20_29 <- cumsum(temp_player_dat$fgm_20_29)
  temp_player_dat$cum_sum_fgm_20_29 <- get_lag_data(temp_player_dat$cum_sum_fgm_20_29)
  
  # moving avg
  temp_player_dat$mov_avg_fgm_20_29 <- movavg(temp_player_dat$fgm_20_29, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm_20_29 <- get_lag_data(temp_player_dat$mov_avg_fgm_20_29)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm_30_39 <- cumsum(temp_player_dat$fgm_30_39)
  temp_player_dat$cum_sum_fgm_30_39 <- get_lag_data(temp_player_dat$cum_sum_fgm_30_39)
  
  # moving avg
  temp_player_dat$mov_avg_fgm_30_39 <- movavg(temp_player_dat$fgm_30_39, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm_30_39 <- get_lag_data(temp_player_dat$mov_avg_fgm_30_39)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm_40_49 <- cumsum(temp_player_dat$fgm_40_49)
  temp_player_dat$cum_sum_fgm_40_49 <- get_lag_data(temp_player_dat$cum_sum_fgm_40_49)
  
  # moving avg
  temp_player_dat$mov_avg_fgm_40_49 <- movavg(temp_player_dat$fgm_40_49, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm_40_49 <- get_lag_data(temp_player_dat$mov_avg_fgm_40_49)
  
  # cumulative sum
  temp_player_dat$cum_sum_fgm_50_plus <- cumsum(temp_player_dat$fgm_50_plus)
  temp_player_dat$cum_sum_fgm_50_plus <- get_lag_data(temp_player_dat$cum_sum_fgm_50_plus)
  
  # moving avg
  temp_player_dat$mov_avg_fgm_50_plus <- movavg(temp_player_dat$fgm_50_plus, n = 3, type = 's')
  temp_player_dat$mov_avg_fgm_50_plus <- get_lag_data(temp_player_dat$mov_avg_fgm_50_plus)
  
  # remove variables
  temp_player_dat$starter <- temp_player_dat$venue <- temp_player_dat$fga <- temp_player_dat$fgm <- 
    temp_player_dat$fga_0_19 <- temp_player_dat$fgm_0_19 <- temp_player_dat$fga_20_29 <- temp_player_dat$fgm_20_29 <- 
    temp_player_dat$fga_30_39 <- temp_player_dat$fgm_30_39 <- temp_player_dat$fga_40_49<- temp_player_dat$fgm_40_49 <- 
    temp_player_dat$fga_50_plus <- temp_player_dat$fgm_50_plus <- temp_player_dat$scoring_extra_points_att <- 
    temp_player_dat$scoring_extra_points_att <- temp_player_dat$fumbles <- temp_player_dat$fumbles_fl <- NULL
  
  return(temp_player_dat)
}

# function for featurizing qb data
get_rb_data <- function(temp_player_dat){
  
  # get starting streak and H/A streak
  temp_player_dat$starter_streak <- streak(temp_player_dat$starter, value = 'Y')
  temp_player_dat$venue_streak <- streak(temp_player_dat$venue, value = 'Home')
  # FUMBLES
  # fumbles, fumbles_fl
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles <- cumsum(temp_player_dat$fumbles)
  temp_player_dat$cum_sum_fumbles <- get_lag_data(temp_player_dat$cum_sum_fumbles)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles <- movavg(temp_player_dat$fumbles, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles <- get_lag_data(temp_player_dat$mov_avg_fumbles)
  
  # cumulative sum
  temp_player_dat$cum_sum_fumbles_fl <- cumsum(temp_player_dat$fumbles_fl)
  temp_player_dat$cum_sum_fumbles_fl <- get_lag_data(temp_player_dat$cum_sum_fumbles_fl)
  
  # moving avg
  temp_player_dat$mov_avg_fumbles_fl <- movavg(temp_player_dat$fumbles_fl, n = 3, type = 's')
  temp_player_dat$mov_avg_fumbles_fl <- get_lag_data(temp_player_dat$mov_avg_fumbles_fl)
  
  # RUSHING 
  # rush_yds, rush_td, rush_lg, rush_target, rec_target, rec_reception, rec_yds, rec_td, rec_lg
  # cumulative sum
  temp_player_dat$cum_sum_rush_att <- cumsum(temp_player_dat$rush_att)
  temp_player_dat$cum_sum_rush_att <- get_lag_data(temp_player_dat$cum_sum_rush_att)
  
  # moving avg
  temp_player_dat$mov_avg_rush_att <- movavg(temp_player_dat$rush_att, n = 3, type = 's')
  temp_player_dat$mov_avg_rush_att <- get_lag_data(temp_player_dat$mov_avg_rush_att)
  
  # cumulative sum
  temp_player_dat$cum_sum_rush_yds <- cumsum(temp_player_dat$rush_yds)
  temp_player_dat$cum_sum_rush_yds <- get_lag_data(temp_player_dat$cum_sum_rush_yds)
  
  # moving avg
  temp_player_dat$mov_avg_rush_yds <- movavg(temp_player_dat$rush_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_rush_yds <- get_lag_data(temp_player_dat$mov_avg_rush_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_rush_td <- cumsum(temp_player_dat$rush_td)
  temp_player_dat$cum_sum_rush_td <- get_lag_data(temp_player_dat$cum_sum_rush_td)
  
  # moving avg
  temp_player_dat$mov_avg_rush_td <- movavg(temp_player_dat$rush_td, n = 3, type = 's')
  temp_player_dat$mov_avg_rush_td <- get_lag_data(temp_player_dat$mov_avg_rush_td)
  
  # cumulative sum
  temp_player_dat$cum_sum_rush_lg <- cumsum(temp_player_dat$rush_lg)
  temp_player_dat$cum_sum_rush_lg <- get_lag_data(temp_player_dat$cum_sum_rush_lg)
  
  # moving avg
  temp_player_dat$mov_avg_rush_lg <- movavg(temp_player_dat$rush_lg, n = 3, type = 's')
  temp_player_dat$mov_avg_rush_lg <- get_lag_data(temp_player_dat$mov_avg_rush_lg)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_target <- cumsum(temp_player_dat$rec_target)
  temp_player_dat$cum_sum_rec_target <- get_lag_data(temp_player_dat$cum_sum_rec_target)
  
  # moving avg
  temp_player_dat$mov_avg_rec_target <- movavg(temp_player_dat$rec_target, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_target <- get_lag_data(temp_player_dat$mov_avg_rec_target)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_reception <- cumsum(temp_player_dat$rec_reception)
  temp_player_dat$cum_sum_rec_reception <- get_lag_data(temp_player_dat$cum_sum_rec_reception)
  
  # moving avg
  temp_player_dat$mov_avg_rec_reception <- movavg(temp_player_dat$rec_reception, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_reception <- get_lag_data(temp_player_dat$mov_avg_rec_reception)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_yds <- cumsum(temp_player_dat$rec_yds)
  temp_player_dat$cum_sum_rec_yds <- get_lag_data(temp_player_dat$cum_sum_rec_yds)
  
  # moving avg
  temp_player_dat$mov_avg_rec_yds <- movavg(temp_player_dat$rec_yds, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_yds <- get_lag_data(temp_player_dat$mov_avg_rec_yds)
  
  # cumulative sum
  temp_player_dat$cum_sum_rec_td <- cumsum(temp_player_dat$rec_td)
  temp_player_dat$cum_sum_rec_td <- get_lag_data(temp_player_dat$cum_sum_rec_td)
  
  # moving avg
  temp_player_dat$mov_avg_rec_td <- movavg(temp_player_dat$rec_td, n = 3, type = 's')
  temp_player_dat$mov_avg_rec_td <- get_lag_data(temp_player_dat$mov_avg_rec_td)
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense <- cumsum(temp_player_dat$snap_counts_offense)
  temp_player_dat$cum_sum_snap_counts_offense <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense <- movavg(temp_player_dat$snap_counts_offense, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense)
  
  # cumulative sum
  temp_player_dat$cum_sum_snap_counts_offense_pct <- cumsum(temp_player_dat$snap_counts_offense_pct)
  temp_player_dat$cum_sum_snap_counts_offense_pct <- get_lag_data(temp_player_dat$cum_sum_snap_counts_offense_pct)
  
  # moving avg
  temp_player_dat$mov_avg_snap_counts_offense_pct <- movavg(temp_player_dat$snap_counts_offense_pct, n = 3, type = 's')
  temp_player_dat$mov_avg_snap_counts_offense_pct <- get_lag_data(temp_player_dat$mov_avg_snap_counts_offense_pct)
  
  # remove variables
  temp_player_dat$starter <- temp_player_dat$venue <- temp_player_dat$fumbles <- temp_player_dat$fumbles_fl <- 
    temp_player_dat$rush_att <-  temp_player_dat$rush_yds  <-  temp_player_dat$rush_td <- 
    temp_player_dat$rush_lg <- temp_player_dat$rec_target <- temp_player_dat$rec_reception <- 
    temp_player_dat$rec_yds <- temp_player_dat$rec_td <- temp_player_dat$rec_lg <- 
    temp_player_dat$snap_counts_offense <- temp_player_dat$snap_counts_offense_pct <- NULL
  
  return(temp_player_dat)
}

# create a function that loops through the position list and featurizes data
# this is the main function that featurizes player data.
# temp_dat <- qb_16
# position_name <- 'QB'
# j = 1
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
    if(position_name == 'RB'){
      sub_player <- get_rb_data(sub_player)
    }
    
    player_result_list[[j]] <- sub_player
  }
  player_data <- do.call('rbind', player_result_list)
  return(player_data)
}


# need to recode player name since that is what we will merge on
match_player_names <- function(temp_dat){
  player_names <- unique(temp_dat$name)
  first_names <- unlist(lapply(strsplit(player_names, ', '), function(x) x[2]))
  last_names <- unlist(lapply(strsplit(player_names, ', '), function(x) x[1]))
  full_name <- paste0(first_names, ' ', last_names)
  
  fuzzy_names <- stringdistmatrix(a = all_player_names,
                                  b = full_name)
  
  # get old names
  old_names <- apply(fuzzy_names, 1, function(x){
    # get the index of the best match(es)
    #best_match <- which.min(x)
    the_min <- min(x)
    best_match <- which(x  == the_min)
    # extract the best match from geo_unique_2011
    player_names <- player_names[best_match]
    # paste together the best names
    player_names <- paste0(player_names, collapse = ';')
    # remove rows that have ";" becuase they are not correctly matched
    # best_names <- best_names[!grepl(';', best_names, fixed = TRUE)]
  })
  
  # get best matches
  matched_names <- apply(fuzzy_names, 1, function(x){
    # get the index of the best match(es)
    #best_match <- which.min(x)
    the_min <- min(x)
    best_match <- which(x  == the_min)
    # extract the best match from geo_unique_2011
    best_names <- full_name[best_match]
    # paste together the best names
    best_names <- paste0(best_names, collapse = ';')
    # remove rows that have ";" becuase they are not correctly matched
    # best_names <- best_names[!grepl(';', best_names, fixed = TRUE)]
  })
  
  # remove duplicates 
  old_names <- old_names[!duplicated(old_names)]
  matched_names <- matched_names[!duplicated(matched_names)]
  
  # combine old_names and matched names into a dataset
  all_names <- as.data.frame(cbind(old_names, matched_names))
  
  # remove rows with semicolon as they are not matched correctly
  all_names <- all_names[!grepl(';', all_names$matched_names, fixed = TRUE),]
  all_names$old_names <- as.character(all_names$old_names)
  
  # join with temp_dat 
  final_data <- inner_join(temp_dat, all_names, by = c('name' = 'old_names'))
  final_data$name <- NULL
  final_data <- final_data[, c('year', 'week', 'matched_names', 'gid', 'pos', 'team', 'h/a',
                               'oppt', 'dk points', 'dk salary')]
  names(final_data)[3] <- 'player'
  
  return(final_data)
  
}


# # create function that splits data by game id and then joins on game id.
# combine_by_game_id <- function(temp_dat){
#   temp_dat$game_id_dup_ind <- duplicated(temp_dat$game_id)
#   temp_dup_1 <- temp_dat[temp_dat$game_id_dup_ind == TRUE,]
#   temp_dup_2 <- temp_dat[temp_dat$game_id_dup_ind == FALSE,]
#   temp_final <- inner_join(temp_dup_1, temp_dup_2, by = 'game_id')
#   names(temp_final) <- gsub('.x', 'team_1', names(temp_final), fixed = TRUE)
#   names(temp_final) <- gsub('.y', 'team_2', names(temp_final), fixed = TRUE)
# 
#   return(temp_final)
# }
# 


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

