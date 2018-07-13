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
get_lag_data <- function(temp_dat, variable){
  temp_dat[, variable] <- lag(temp_dat[, variable])
  temp_dat[, variable] <- ifelse(is.na(temp_dat[, variable]), 0, temp_dat[, variable])
  return(temp_dat[, variable])
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
    sub_team$win_ind <- get_lag_data(sub_team, 'win_ind')
    
    # create a numeric venue column and use the lag function 
    sub_team$venue_ind <- ifelse(sub_team$venue == 'Home', 1, 0 )
    sub_team$venue_ind <- get_lag_data(sub_team, 'venue_ind')
    
    # get cumulative sum of lagged wins and winning percentage 
    sub_team$cum_wins_lag <- cumsum(sub_team$win_ind)
    sub_team$cum_wins_per_lag <- cumsum(sub_team$win_ind)/get_lag_data(sub_team, 'game_num')
    sub_team$cum_wins_per_lag <- ifelse(sub_team$cum_wins_per_lag == 'NaN', 0, sub_team$cum_wins_per_lag)
    
    # get cumulative sum of points scored and points allowed 
    sub_team$cum_sum_points <- cumsum(sub_team$final)
    sub_team$cum_sum_points <- get_lag_data(sub_team, 'cum_sum_points')
    sub_team$cum_sum_points_opp <- cumsum(sub_team$points_allowed_by_def)
    sub_team$cum_sum_points_opp <- get_lag_data(sub_team, 'cum_sum_points_opp')
    
    # get cumulative sum of yds 
    sub_team$cum_sum_total_yds <- cumsum(sub_team$total_yds)
    sub_team$cum_sum_total_yds <- get_lag_data(sub_team, 'cum_sum_total_yds')
    
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
    sub_team$mov_avg_first_downs <- get_lag_data(sub_team,'mov_avg_first_downs')
    
    # rush_yds
    sub_team$mov_avg_rush_yds <- movavg(sub_team$rush_yds, n = 5, type = 's')
    sub_team$mov_avg_rush_yds <- get_lag_data(sub_team,'mov_avg_rush_yds')
    
    # rush_tds
    sub_team$mov_avg_rush_tds <- movavg(sub_team$rush_tds, n = 5, type = 's')
    sub_team$mov_avg_rush_tds <- get_lag_data(sub_team,'mov_avg_rush_tds')
    
    # pass_comp
    sub_team$mov_avg_pass_comp <- movavg(sub_team$pass_comp, n = 5, type = 's')
    sub_team$mov_avg_pass_comp <- get_lag_data(sub_team,'mov_avg_pass_comp')
    
    # pass_yds
    sub_team$mov_avg_pass_yds <- movavg(sub_team$pass_yds, n = 5, type = 's')
    sub_team$mov_avg_pass_yds <- get_lag_data(sub_team,'mov_avg_pass_yds')
    
    # pass_tds
    sub_team$mov_avg_pass_tds <- movavg(sub_team$pass_tds, n = 5, type = 's')
    sub_team$mov_avg_pass_tds <- get_lag_data(sub_team,'mov_avg_pass_tds')
    
    # qb_interceptions
    sub_team$mov_avg_qb_interceptions <- movavg(sub_team$qb_interceptions, n = 5, type = 's')
    sub_team$mov_avg_qb_interceptions <- get_lag_data(sub_team,'mov_avg_qb_interceptions')
    
    # qb_sacked
    sub_team$mov_avg_qb_sacked <- movavg(sub_team$qb_sacked, n = 5, type = 's')
    sub_team$mov_avg_qb_sacked <- get_lag_data(sub_team,'mov_avg_qb_sacked')
    
    # fumbles
    sub_team$mov_avg_fumbles <- movavg(sub_team$fumbles, n = 5, type = 's')
    sub_team$mov_avg_fumbles <- get_lag_data(sub_team,'mov_avg_fumbles')
    
    # turnovers
    sub_team$mov_avg_turnovers <- movavg(sub_team$turnovers, n = 5, type = 's')
    sub_team$mov_avg_turnovers <- get_lag_data(sub_team,'mov_avg_turnovers')
    
    # penalties
    sub_team$mov_avg_penalties <- movavg(sub_team$penalties , n = 5, type = 's')
    sub_team$mov_avg_penalties <- get_lag_data(sub_team,'mov_avg_penalties')
    
    # def_interception
    sub_team$mov_avg_def_interception <- movavg(sub_team$def_interception , n = 5, type = 's')
    sub_team$mov_avg_def_interception <- get_lag_data(sub_team,'mov_avg_def_interception')
    
    # def_sack
    sub_team$mov_avg_def_sack <- movavg(sub_team$def_sack , n = 5, type = 's')
    sub_team$mov_avg_def_sack <- get_lag_data(sub_team,'mov_avg_def_sack')
    
    # first
    sub_team$mov_avg_first <- movavg(sub_team$first , n = 5, type = 's')
    sub_team$mov_avg_first <- get_lag_data(sub_team,'mov_avg_first')
    
    # second
    sub_team$mov_avg_second <- movavg(sub_team$second , n = 5, type = 's')
    sub_team$mov_avg_second <- get_lag_data(sub_team,'mov_avg_second')
    
    # third
    sub_team$mov_avg_third <- movavg(sub_team$third , n = 5, type = 's')
    sub_team$mov_avg_third <- get_lag_data(sub_team,'mov_avg_third')
    
    # fourth
    sub_team$mov_avg_fourth <- movavg(sub_team$fourth , n = 5, type = 's')
    sub_team$mov_avg_fourth <- get_lag_data(sub_team,'mov_avg_fourth')
    
    # final
    sub_team$mov_avg_final <- movavg(sub_team$final , n = 5, type = 's')
    sub_team$mov_avg_final <- get_lag_data(sub_team,'mov_avg_final')
    
    # points_allowed_by_def
    sub_team$mov_avg_points_allowed_by_def <- movavg(sub_team$points_allowed_by_def , n = 5, type = 's')
    sub_team$mov_avg_points_allowed_by_def <- get_lag_data(sub_team,'mov_avg_points_allowed_by_def')
    
    # third_downs_made, att, and efficiency
    sub_team$mov_avg_third_down_made <- movavg(sub_team$third_down_made , n = 5, type = 's')
    sub_team$mov_avg_third_down_made <- get_lag_data(sub_team,'mov_avg_third_down_made')
    sub_team$mov_avg_third_down_att <- movavg(sub_team$third_down_att , n = 5, type = 's')
    sub_team$mov_avg_third_down_att <- get_lag_data(sub_team,'mov_avg_third_down_att')
    sub_team$third_down_per <- round((sub_team$third_down_made/sub_team$third_down_att)*100,2)
    sub_team$mov_avg_third_down_per <- movavg(sub_team$third_down_per , n = 5, type = 's')
    sub_team$mov_avg_third_down_per <- get_lag_data(sub_team,'mov_avg_third_down_per')
    
    # fourth_downs_made, att, and efficiency
    sub_team$mov_avg_fourth_down_made <- movavg(sub_team$fourth_down_made , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_made <- get_lag_data(sub_team,'mov_avg_fourth_down_made')
    sub_team$mov_avg_fourth_down_att <- movavg(sub_team$fourth_down_att , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_att <- get_lag_data(sub_team,'mov_avg_fourth_down_att')
    sub_team$fourth_down_per <- round((sub_team$fourth_down_made/sub_team$fourth_down_att)*100,2)
    sub_team$mov_avg_fourth_down_per <- movavg(sub_team$fourth_down_per , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_per <- get_lag_data(sub_team,'mov_avg_fourth_down_per')
    sub_team$fourth_down_per <- sub_team$third_down_per <- NULL
    
    # only keep the variables that are created with the correct format = each row is previous weeks data
    # either in the form of cumulative sums or moving avgerages
    column_string <- c('mov_avg|^cum|win_streak|lose_streak|win_loss|game_num|last_game|momentumdate|week|team|^venue$')
    sub_team <- sub_team[, grepl(column_string, names(sub_team))]
    
    # store data in data_list
    data_list[[i]] <- sub_team
    
  }
  
  final_data <- do.call('rbind', data_list)
  return(final_data)
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

