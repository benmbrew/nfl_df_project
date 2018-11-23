# this script will predict on qb data
# USE FANTASY POINTS INSTEAD OF DRAFT KINGS OR FAN DUEL
# source functions scrip to get functions and libraries
source('model_functions.R')

# read in top qb data frame
qb_group <- readRDS( '../data/model_data/best_qbs.rda')

# fantasy and playerr data 
qb_data <- readRDS('../data/model_data/qb_data.rda')

# fantasy, player,  and team data
qb_data_team <- readRDS('../data/model_data/qb_data_team.rda')

# subset data by qb_group
best_qbs <- as.character(qb_group$player)
qb_data <- qb_data %>% filter(player %in% best_qbs)
qb_data_team <- qb_data_team %>% filter(player %in% best_qbs)

# recode qb_data_team
names(qb_data_team) <- gsub('.x', '', names(qb_data_team), fixed = TRUE)
qb_data_team$date.y <- qb_data_team$last_game.y <- NULL

# remove unnecessary columns
qb_data$position <- qb_data$counts <- qb_data$draft_kings_position <- qb_data$fan_duel_position <- 
  qb_data$date <- qb_data$fan_duel_points <- NULL

# from team as well
qb_data_team$position <- qb_data_team$counts <- qb_data_team$draft_kings_position <- 
  qb_data_team$fan_duel_position <- qb_data_team$date <- qb_data_team$fan_duel_points <-
  qb_data_team$game_id <- qb_data_team$game_num <- qb_data_team$win_loss <- 
  qb_data_team$win_ind_team <- qb_data_team$team_opp <- NULL

# select columns to keep
qb_data_team <- qb_data_team[, c('player','year','week', 'venue', 'draft_kings_points', "draft_kings_salary",
                                 'fan_duel_salary', 'cum_sum_draft_kings_salary', 'mov_avg_draft_kings_salary',
                                 'last_week_draft_kings_salary', 'cum_sum_draft_kings_points', 'mov_avg_draft_kings_points',
                                 'last_week_draft_kings_points', 'mov_avg_pass_comp', 'mov_avg_pass_att', 'mov_avg_pass_yds',
                                 'mov_avg_pass_td', 'mov_avg_pass_int', 'mov_avg_pass_sack', "mov_avg_points_allowed_by_def_opp",
                                 'rank_tot_points', 'rank_tot_yds', 'rank_tot_yds_opp', 'rank_tot_int_opp', 'rank_tot_def_int_opp',
                                 'rank_tot_rush_yds_opp', 'rank_tot_pass_yds_opp', 'rank_tot_pass_tds_opp')]

# remove first row week 1 from 2016 
qb_data <- qb_data[which(qb_data$week != 1 | qb_data$year != '2016'),]
qb_data_team <- qb_data_team[which(qb_data_team$week != '1' | qb_data_team$year != '2016'),]

# # get folds
# qb_data <- get_data_folds(qb_data, season_length = 17, current_season_length = 6)
# qb_data_team <- get_data_folds(qb_data_team, season_length = 17, current_season_length = 6)
# qb_data$fold <- qb_data$fold - 1
# qb_data_team$fold <- qb_data_team$fold - 1

# get complete cases (in this case remove the row where fan duel salary is NA)
qb_data <- qb_data[complete.cases(qb_data),]
qb_data_team <- qb_data_team[complete.cases(qb_data_team),]

# ceiling ranks and make character 
qb_data_team$rank_tot_points <- as.factor(ifelse(qb_data_team$rank_tot_points > 0 & qb_data_team$rank_tot_points <= 6,
                                                 'top_quin', 
                                                 ifelse(qb_data_team$rank_tot_points > 6 & qb_data_team$rank_tot_points <= 12,
                                                        '2nd_quin', 
                                                        ifelse(qb_data_team$rank_tot_points > 12 & qb_data_team$rank_tot_points <= 19,
                                                               '3rd_quin', 
                                                               ifelse(qb_data_team$rank_tot_points > 19 & qb_data_team$rank_tot_points <= 26,
                                                                      '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_yds <- as.factor(ifelse(qb_data_team$rank_tot_yds > 0 & qb_data_team$rank_tot_yds <= 6,
                                              'top_quin', 
                                              ifelse(qb_data_team$rank_tot_yds > 6 & qb_data_team$rank_tot_yds <= 12,
                                                     '2nd_quin', 
                                                     ifelse(qb_data_team$rank_tot_yds > 12 & qb_data_team$rank_tot_yds <= 19,
                                                            '3rd_quin', 
                                                            ifelse(qb_data_team$rank_tot_yds > 19 & qb_data_team$rank_tot_yds <= 26,
                                                                   '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_yds_opp <- as.factor(ifelse(qb_data_team$rank_tot_yds_opp > 0 & qb_data_team$rank_tot_yds_opp <= 6,
                                                  'top_quin', 
                                                  ifelse(qb_data_team$rank_tot_yds_opp > 6 & qb_data_team$rank_tot_yds_opp <= 12,
                                                         '2nd_quin', 
                                                         ifelse(qb_data_team$rank_tot_yds_opp > 12 & qb_data_team$rank_tot_yds_opp <= 19,
                                                                '3rd_quin', 
                                                                ifelse(qb_data_team$rank_tot_yds_opp > 19 & qb_data_team$rank_tot_yds_opp <= 26,
                                                                       '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_int_opp <- ifelse(qb_data_team$rank_tot_int_opp > 0 & qb_data_team$rank_tot_int_opp <= 6,
                                        'top_quin', 
                                        ifelse(qb_data_team$rank_tot_int_opp > 6 & qb_data_team$rank_tot_int_opp <= 12,
                                               '2nd_quin', 
                                               ifelse(qb_data_team$rank_tot_int_opp > 12 & qb_data_team$rank_tot_int_opp <= 19,
                                                      '3rd_quin', 
                                                      ifelse(qb_data_team$rank_tot_int_opp > 19 & qb_data_team$rank_tot_int_opp <= 26,
                                                             '4th_quin', '5th_quin'))))

qb_data_team$rank_tot_def_int_opp <- as.factor(ifelse(qb_data_team$rank_tot_def_int_opp > 0 & qb_data_team$rank_tot_def_int_opp <= 6,
                                                      'top_quin', 
                                                      ifelse(qb_data_team$rank_tot_def_int_opp > 6 & qb_data_team$rank_tot_def_int_opp <= 12,
                                                             '2nd_quin', 
                                                             ifelse(qb_data_team$rank_tot_def_int_opp > 12 & qb_data_team$rank_tot_def_int_opp <= 19,
                                                                    '3rd_quin', 
                                                                    ifelse(qb_data_team$rank_tot_def_int_opp > 19 & qb_data_team$rank_tot_def_int_opp <= 26,
                                                                           '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_rush_yds_opp <- as.factor(ifelse(qb_data_team$rank_tot_rush_yds_opp > 0 & qb_data_team$rank_tot_rush_yds_opp <= 6,
                                                       'top_quin', 
                                                       ifelse(qb_data_team$rank_tot_rush_yds_opp > 6 & qb_data_team$rank_tot_rush_yds_opp <= 12,
                                                              '2nd_quin', 
                                                              ifelse(qb_data_team$rank_tot_rush_yds_opp > 12 & qb_data_team$rank_tot_rush_yds_opp <= 19,
                                                                     '3rd_quin', 
                                                                     ifelse(qb_data_team$rank_tot_rush_yds_opp > 19 & qb_data_team$rank_tot_rush_yds_opp <= 26,
                                                                            '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_pass_yds_opp <- as.factor(ifelse(qb_data_team$rank_tot_pass_yds_opp > 0 & qb_data_team$rank_tot_pass_yds_opp <= 6,
                                                       'top_quin', 
                                                       ifelse(qb_data_team$rank_tot_pass_yds_opp > 6 & qb_data_team$rank_tot_pass_yds_opp <= 12,
                                                              '2nd_quin', 
                                                              ifelse(qb_data_team$rank_tot_pass_yds_opp > 12 & qb_data_team$rank_tot_pass_yds_opp <= 19,
                                                                     '3rd_quin', 
                                                                     ifelse(qb_data_team$rank_tot_pass_yds_opp > 19 & qb_data_team$rank_tot_pass_yds_opp <= 26,
                                                                            '4th_quin', '5th_quin')))))

qb_data_team$rank_tot_pass_tds_opp <- as.factor(ifelse(qb_data_team$rank_tot_pass_tds_opp > 0 & qb_data_team$rank_tot_pass_tds_opp <= 6,
                                                       'top_quin', 
                                                       ifelse(qb_data_team$rank_tot_pass_tds_opp > 6 & qb_data_team$rank_tot_pass_tds_opp <= 12,
                                                              '2nd_quin', 
                                                              ifelse(qb_data_team$rank_tot_pass_tds_opp > 12 & qb_data_team$rank_tot_pass_tds_opp <= 19,
                                                                     '3rd_quin', 
                                                                     ifelse(qb_data_team$rank_tot_pass_tds_opp > 19 & qb_data_team$rank_tot_pass_tds_opp <= 26,
                                                                            '4th_quin', '5th_quin')))))



# recode ranks
# first model, for each player (for loop), predict fantasy points with a subset of features and 
# ranked features (whole numbers 1-32), with no team or opponent team names

# function to loop through each qb and get predictions
temp_dat <- qb_data_team
i = 1
pred_qb_loop <- function(temp_dat){
  unique_qbs <- unique(temp_dat$player)
  for(i in 1:length(unique_qbs)){
    qb_name <- unique_qbs[i]
    temp_dat <- temp_dat[temp_dat$player == qb_name,]
    
    # order by date
    temp_dat$year <- as.numeric(temp_dat$year)
    temp_dat$week <- as.numeric(temp_dat$week)
    
    temp_dat <- temp_dat[with(temp_dat, order(year, week)), ]    
    
    temp_dat$fold <- seq(1, nrow(temp_dat), 1)
    # order data by date
  }
}


model_matrix = temp_dat
train_window = 1:(nrow(temp_dat) - 1)
test_window = nrow(temp_dat)
model_type = 'elastic_net'
importance_fac = TRUE
fixed_window = TRUE
initial_window = 20
horizon_window = 1
lm_aic = TRUE

# make sure characters are factors
pred_qb_all <- function(model_matrix,
                        train_window,
                        test_window,
                        model_type,
                        fixed_window,
                        initial_window,
                        horizon_window) {
  
  
  # store player name 
  qb_names <- unique(model_matrix$player)
  model_matrix$player <- NULL
  
  # change characters to factors in triain_x - player, team, opponent, venue, year
  model_matrix$venue <- as.factor(model_matrix$venue)
  model_matrix$year <- as.factor(model_matrix$year)
  
  # get complete cases
  model_matrix <- model_matrix[complete.cases(model_matrix),]
  
  # get training and test data
  train_x <- model_matrix %>% filter(fold %in% train_window)
  test_x <- model_matrix %>% filter(fold %in% test_window)
  
  # get outcome variable (draft kings fantasy points)
  train_y <- as.numeric(unlist(train_x$draft_kings_points))
  train_x$draft_kings_points <- NULL
  
  test_y <- as.numeric(unlist(test_x$draft_kings_points))
  test_x$draft_kings_points <- NULL
  
  # remove folds
  train_x$fold <- NULL
  test_x$fold <- NULL
  
  if(model_type == 'random_forest') {
    # determines how you train the model.
    fitControl <- trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE
    )
    
    
    # mtry: Number of variables randomly sampled as candidates at each split.
    # ntree: Number of trees to grow.
    mtry <- sqrt(ncol(train_x[,colnames(train_x)]))
    tunegrid <- expand.grid(.mtry=mtry)
    
    model <- train(x = train_x
                   , y = train_y
                   , metric = 'RMSE'
                   , method = "rf"
                   , trControl = fitControl
                   , tuneGrid = tunegrid
                   , importance = T
                   , verbose = FALSE)
    
    
    temp <- varImp(model)[[1]]
    importance <- cbind(variable = rownames(temp), score = temp$Overall)
    importance <- as.data.frame(importance)
    importance$score <- round(as.numeric(as.character(importance$score)), 2)
    importance <- importance %>% arrange(-score)
    
    # Predictions on test data
    
    # This returns 100 prediction with 1-100 lambdas
    test.predictions <- predict(model,
                                newdata = test_x)
    
    
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(player_name = qb_names,
                                    predicted = as.character(test.predictions), 
                                    real = as.character(test_y)))
    
    temp_dat$player_name <- as.character(temp_dat$player_name)
    temp_dat$predicted <- as.numeric(as.character(temp_dat$predicted))
    temp_dat$real <- as.numeric(as.character(temp_dat$real))
    
    return(list(temp_dat, importance))
  }
  
  if(model_type == 'lm') {
    # recombine train_x and train_y
    lm_data <- as.data.frame(cbind(train_y, train_x))
    
    # run model
    lm_mod <- lm(train_y~., data = lm_data)
    
    lm_preds <- predict(lm_mod, newdata = test_x)
    
    temp_dat <- as.data.frame(cbind(player_name = qb_names, predicted_points = lm_preds, real_points = test_y))
    
    return(list(temp_dat, lm_mod))
    
    
  }
  
  if(model_type == 'elastic_net'){
    lambda.grid <- seq(0, 100)
    alpha.grid <- seq(0, 0.9, length = 10)
    
    trnCtrl = trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE)
    
    srchGrd = expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
    
    trained_glmnet <- train(x = model.matrix(~., train_x),
                            y = train_y,
                            method = "glmnet",
                            tuneGrid = srchGrd,
                            trControl = trnCtrl,
                            standardize = FALSE,
                            maxit = 1000000)
    
    
    final_model <- trained_glmnet$finalModel
    lambda_min_index <- which(final_model$lambda == min(final_model$lambda))
    
    # This returns 100 prediction with 1-100 lambdas
    temp_test.predictions <- predict(final_model,
                                     model.matrix(~.,test_x),
                                     type = 'response')
    
    
    # get predictions with corresponding lambda.
    test.predictions <- temp_test.predictions[, lambda_min_index]
    
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(predicted_points = test.predictions, real_points = test_y))
    
    return(list(temp_dat, trained_glmnet))
    
    
  }
  
}




# make sure characters are factors
pred_qb_individual <- function(model_matrix,
                               train_window,
                               test_window,
                               include_team,
                               include_opp,
                               model_type,
                               fixed_window,
                               initial_window,
                               horizon_window,
                               lm_aic) {
  
  # get traning data
  
  # get complete cases
  model_matrix <- model_matrix[complete.cases(model_matrix),]
  
  # get training and test data
  train_x <- model_matrix %>% filter(fold %in% train_window)
  test_x <- model_matrix %>% filter(fold %in% test_window)
  
  if(include_team){
    # Assure they have the same levels for team and opponent
    # get intersecting teams and opponents so train and test data have same variables
    shared_teams <- intersect(train_x$team, test_x$team)
    
    # subset by shared teams
    train_x <- train_x %>% filter(team %in% shared_teams)
    test_x <- test_x %>% filter(team %in% shared_teams)
    
    # condition to stop model if not met
    stopifnot(all(sort(unique(train_x$team)) == sort(unique(test_x$team))))
    
  }
  if(include_opp){
    # opponents
    shared_opponents <- intersect(train_x$team_opp, test_x$team_opp)
    
    # subset by shared opponents
    train_x <- train_x %>% filter(team_opp %in% shared_opponents)
    test_x <- test_x %>% filter(team_opp %in% shared_opponents)
    
    # stop the model if these conditions are not met
    stopifnot(all(sort(unique(train_x$opponent)) == sort(unique(test_x$opponent))))
    
  }
  
  # get outcome variable (draft kings fantasy points)
  train_y <- train_x$draft_kings_points
  train_x$draft_kings_points <- NULL
  
  train_y <- train_y$draft_kings_points
  train_y$draft_kings_points <- NULL
  
  # remove folds
  train_x$fold <- NULL
  test_x$fold <- NULL
  
  
  if(model_type == 'random_forest') {
    # determines how you train the model.
    fitControl <- trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE
    )
    
    
    # mtry: Number of variables randomly sampled as candidates at each split.
    # ntree: Number of trees to grow.
    mtry <- sqrt(ncol(train_x[,colnames(train_x)]))
    tunegrid <- expand.grid(.mtry=mtry)
    
    model <- train(x = train_x
                   , y = train_y
                   , metric = 'RMSE'
                   , method = "rf"
                   , trControl = fitControl
                   , tuneGrid = tunegrid
                   , importance = T
                   , verbose = FALSE)
    
    
    
    temp <- varImp(model)[[1]]
    importance <- cbind(variable = rownames(temp), score = temp$Overall)
    importance <- as.data.frame(importance)
    importance$score <- round(as.numeric(as.character(importance$score)), 2)
    importance <- importance %>% arrange(-score)
    
    # Predictions on test data
    
    # This returns 100 prediction with 1-100 lambdas
    test.predictions <- predict(model,
                                newdata = test_x)
    
    
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(predicted = as.character(test.predictions), 
                                    real = as.character(test_y)))
    
    return(list(temp_dat, importance))
  }
  
  if(model_type == 'lm') {
    # recombine train_x and train_y
    lm_data <- as.data.frame(cbind(train_y, train_x))
    
    # run model
    lm_mod <- lm(train_y~., data = lm_data[1:num_obs,])
    
    if(lm_aic){
      lm_mod <- stepAIC(lm_mod, 
                        direction = "both",
                        trace = FALSE)
    }
    
    lm_preds <- predict(lm_mod, newdata = test_x)
    
    temp_dat <- as.data.frame(cbind(predicted_points = lm_preds, real_points = test_y))
    
    return(list(temp_dat, lm_mod))
    
    
  }
  
  if(model_type == 'elastic_net'){
    lambda.grid <- seq(0, 100)
    alpha.grid <- seq(0, 0.9, length = 10)
    
    trnCtrl = trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE)
    
    srchGrd = expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
    
    trained_glmnet <- train(x = model.matrix(~., train_x[1:num_obs,]),
                            y = train_y[1:num_obs],
                            method = "glmnet",
                            tuneGrid = srchGrd,
                            trControl = trnCtrl,
                            standardize = FALSE,
                            maxit = 1000000)
    
    
    final_model <- trained_glmnet$finalModel
    lambda_min_index <- which(final_model$lambda == min(final_model$lambda))
    
    # This returns 100 prediction with 1-100 lambdas
    temp_test.predictions <- predict(final_model,
                                     model.matrix(~.,test_x),
                                     type = 'response')
    
    
    # get predictions with corresponding lambda.
    test.predictions <- temp_test.predictions[, lambda_min_index]
    
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(predicted_points = test.predictions, real_points = test_y))
    
    return(list(temp_dat, trained_glmnet))
    
    
  }
  
}

