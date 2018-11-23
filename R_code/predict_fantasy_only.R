
# this script will be used to predict fantasy points ONLY using fantasy data (more years.)

# source funtions
source('model_functions.R')

# read in data
dat_off <- readRDS('../data/cleaned_data/fantasy_offense.rda')
dat_def <- readRDS('../data/cleaned_data/fantasy_defense.rda')

# -----------------------------------------------------------------
# offense data

# recode fan_duel_position so K and PK are the same 
dat_off$fan_duel_position <- gsub('PK', 'K', dat_off$fan_duel_position)

# remove unneeded columns
dat_off$draft_kings_position <- NULL

# change year to numeric for correct order
dat_off$year <- as.numeric(dat_off$year)
dat_off$week <- as.numeric(dat_off$week)

# recode position varibale
dat_off$fan_duel_position <- gsub('#N/A', 'Other', dat_off$fan_duel_position, fixed = TRUE)

# subset data by players that only have more than 30 games played
dat_off <- dat_off %>% group_by(player) %>% mutate(tot_games = n())
dat_off <- dat_off[dat_off$tot_games > 39,]

# break up into position level data)
dat_qb <- dat_off %>% filter(fan_duel_position == 'QB')
dat_rb <- dat_off %>% filter(fan_duel_position == 'RB')
dat_wr <- dat_off %>% filter(fan_duel_position == 'WR')
dat_te <- dat_off %>% filter(fan_duel_position == 'TE')
dat_k <- dat_off %>% filter(fan_duel_position == 'K')


# get folds for model
# dat_qb <- get_fantasy_off_folds(dat_qb)
# dat_rb <- get_fantasy_off_folds(dat_rb)
# dat_wr <- get_fantasy_off_folds(dat_wr)
# dat_te <- get_fantasy_off_folds(dat_te)
# dat_k <- get_fantasy_off_folds(dat_k)




#### ---------------------------------------------------------------------------------------

# temp_dat <- dat_qb
# mod_type = 'random_forest'
# create function that loops through position data and gives predictions for each player
pred_by_position <- function(temp_dat, mod_type){
  
  # create lists to store results
  pred_results <- list()
  preds <- list()
  important_vars <- list()
  # predict points for all positions using all players, use posistion as a feature though
  mod_mat <- temp_dat[, c('fan_duel_points','player', 'year','week','team','opponent', 'venue', 'fan_duel_position',
                          'fan_duel_salary', 'cum_sum_fan_duel_salary', 'mov_avg_fan_duel_salary', 'last_week_fan_duel_salary',
                          'cum_sum_fan_duel_points', 'mov_avg_fan_duel_points', 'last_week_fan_duel_points')]
  
  # loop through text set players and run model
  test_pos <- as.character(unique(mod_mat$player[mod_mat$year == '2018']))
  mod_mat <- mod_mat %>% filter(player %in% test_pos)
  
  for(i in 1:length(test_pos)){
    this_player <- test_pos[i]
    sub_player <- mod_mat[mod_mat$player == this_player,]
    sub_player$player <- NULL  
    sub_player$team <- NULL
    # get fold 
    # get folds - where each fold is the first week in my data and the final fold is the last week, regardless of year.
    sub_player <- sub_player[with(sub_player, order(year, week)),]
    
    # remove first row
    sub_player <- sub_player[-c(1,2),]
    sub_player$fold <-  1:nrow(sub_player)
    test_games <- max(sub_player$week[sub_player$year == '2018'])
    train_games <- (nrow(sub_player) - test_games)
    test_window <- (max(train_games) + 1):(train_games + test_games)
    train_window<- (1:train_games)
    
    # configue initial window and horizon window
    initial_window <- floor((nrow(sub_player)/3))
    horizon_window <- (initial_window/5)
    
    message('starting ', i,'th', ' player')
    sub_player$opponent <- as.factor(sub_player$opponent)
    sub_player$fan_duel_position <- NULL
    sub_player$venue <- as.factor(sub_player$venue)
    pred_results[[i]] <- pred_fantasy(model_matrix = sub_player,
                                      player_name = this_player,
                                      train_window = train_window, 
                                      test_window = test_window, 
                                      fantasy_type = 'fan_duel', 
                                      include_opp = FALSE,
                                      param_folds = NULL, 
                                      param_repeats = NULL,
                                      model_type = 'random_forest', 
                                      initial_window = initial_window, 
                                      fixed_window = TRUE, 
                                      horizon_window = horizon_window,
                                      lm_aic = TRUE)
    preds[[i]] <- as.data.frame(pred_results[[i]][1])
    important_vars[[i]] <- as.data.frame(pred_results[[i]][2])
    
  }
  final_preds <- do.call('rbind', preds)
  final_importance <- do.call('rbind', important_vars)
  
  return(list(final_preds, final_importance))
}

temp <- pred_by_position(temp_dat = dat_wr[dat_wr$fantasy_points > 5,], mod_type = 'random_forest')

  preds <- temp[[1]]
preds$predicted_points <- as.numeric(as.character(preds$predicted_points))
preds$real_points <- as.numeric(as.character(preds$real_points))
preds$diff <- preds$real_points - preds$predicted_points
preds$abs_diff <- abs(preds$diff)

ggplot(preds, aes(predicted_points, real_points)) +
  geom_point(size = 2,
             color = 'black',
             alpha = 0.7) +
   geom_smooth(method = 'loess',
               alpha = 0.7,
               se = FALSE) +
  geom_smooth(method = 'lm',
              color = 'black',
              alpha = 0.7,
              se = FALSE) +
  labs(title = paste0(round(cor(preds$predicted_points, preds$real_points), 2), ' correlation for black line (linear regression)'))


# need to model differnece bettwen salary and real points and maybe use y hat (?) as a covariate in your model

# could model points directly on salary and use that as adjustments

# or model the difference on other covariates and use that as the adjustment

# could use salary as an instrument for predicted points 
# assumptions (i) Z causes variation in X and (ii) It not does not have a direct effect on the outcome
# only directly thought X. BUT we dont have a treatment variable like vaccination.
list.files()
