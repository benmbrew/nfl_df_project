# This script will store all functions that are used for modelling.

# load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(tidyverse)
library(preprocessCore)
library(pROC)
library(doParallel) 
library(e1071)
library(nnet)
library(glmnet)
library(PRROC)
library(broom)
library(MASS)
library(ggthemes)
library(pracma)

registerDoParallel(2)

# This function should be used only when predicting game outcomes, not df outcomes. it 
# restructures data so it is in matrix form where each row is a game and can now be predicted.
get_matrix_structure <- function(temp_dat){
  game_ids <- unique(dat_team$game_id)
  result_list <- list()
  #loop
  for(i in 1:length(game_ids)) {
    message('working on', i)
    this_id <- game_ids[i]
    # subset by id
    sub_game <- dat_team[dat_team$game_id == this_id,]
    # get away team name 
    # away_team <- sub_game$team[sub_game$venue == 'Road']
    # remove away team row
    sub_game <- sub_game[!grepl('Road', sub_game$venue),]
    # create variable in sub_game for the road team name, the data is already represented. 
    # sub_game$away_team <- away_team
    # get model features and original columns organized
    feats <- names(sub_game)[grepl('mov_avg|cum|rank_|momentum|streak|opp', names(sub_game))]
    orig <- c(names(sub_game)[1:10],'year')
    # reorder columns 
    sub_game <- sub_game[, c(orig, feats)]
    # store in list 
    result_list[[i]] <- sub_game
  }
  # collapse list to data frame
  final_data <- do.call('rbind', result_list)
  return(final_data)
  
}


# create an indicator for each week played from the beginning of the data, to use as folds in the model
# season_length = 17
get_fantasy_off_folds <- function(temp_dat){
  
  temp_dat$unique_counter <- paste0(temp_dat$year, '_', temp_dat$week)
  
  groups <-temp_dat$unique_counter %>% unique
  i = 0
  temp_dat$fold <- NA
  for(g in groups) {
    i = i + 1
    temp_dat$fold[temp_dat$unique_counter == g] <- i
  }
  
  temp_dat$unique_counter <- NULL
  
  return(temp_dat)
}


# function to get complete weeks, through years
get_data_folds <- function(temp_dat, season_length, current_season_length){
  
  unique_years <- sort(unique(temp_dat$year))
  year_list <- list()
  
  temp_dat$week <- as.numeric(temp_dat$week)
  for(i in 1:length(unique_years)){
    this_year <- unique_years[i]
    sub_year <- temp_dat[temp_dat$year == this_year,]
    sub_year <- sub_year %>% arrange(week)
    
    if(this_year == '2016'){
      sub_year$fold <- sub_year$week
    } 
    
    if(this_year == '2017'){
      sub_year$fold <- sub_year$week + 16
    } 
    
    if(this_year == '2018'){
      sub_year$fold <- sub_year$week + 33
    }
    
    
    year_list[[i]] <- sub_year
  }
  
  final_data <- do.call('rbind', year_list)
  return(final_data)
}



# create a functions that takes a matrix of features, an outcome, and fold window, and then
# returns predictions and ground truth from model.
# model_matrix = sub_qb
# player_name = this_qb
# train_window = train_window
# test_window = test_window
# fantasy_type = 'fan_duel'
# include_opp = FALSE
# param_folds = NULL
# param_repeats = NULL
# model_type = 'elastic_net'
# initial_window = 50
# fixed_window = TRUE
# horizon_window = 10
# lm_aic = TRUE
# make sure characters are factors
pred_fantasy <- function(model_matrix,
                         player_name,
                         train_window,
                         test_window,
                         fantasy_type,
                         include_opp,
                         param_folds,
                         param_repeats,
                         model_type,
                         initial_window,
                         fixed_window,
                         horizon_window,
                         lm_aic) {
  
  
  
  
  # get traning data
  if(fantasy_type == 'fan_duel') {
    
    # get complete cases
    model_matrix <- model_matrix[complete.cases(model_matrix),]
    
    # get training and test data
    train_x <- model_matrix %>% filter(fold %in% train_window)
    test_x <- model_matrix %>% filter(fold %in% test_window)
    
    if(include_opp){
      # opponents
      shared_opponents <- intersect(train_x$opponent, test_x$opponent)
      
      # subset by shared opponents
      train_x <- train_x %>% filter(opponent %in% shared_opponents)
      test_x <- test_x %>% filter(opponent %in% shared_opponents)
      
      # stop the model if these conditions are not met
      stopifnot(all(sort(unique(train_x$opponent)) == sort(unique(test_x$opponent))))
      
    }
    
    
    # get training and test outcome
    train_y <- as.numeric(unlist(train_x$fan_duel_points))
    train_x$fan_duel_points <- NULL
    
    test_y <- as.numeric(unlist(test_x$fan_duel_points))
    test_x$fan_duel_points <- NULL
  }
  
  # get traning data
  if(fantasy_type == 'draft_kings') {
    if(include_opp){
      # opponents
      shared_opponents <- intersect(train_x$opponent, test_x$opponent)
      
      # subset by shared opponents
      train_x <- train_x %>% filter(opponent %in% shared_opponents)
      test_x <- test_x %>% filter(opponent %in% shared_opponents)
      
      # stop the model if these conditions are not met
      stopifnot(all(sort(unique(train_x$opponent)) == sort(unique(test_x$opponent))))
      
    }
    # get training and test outcome
    train_y <- as.numeric(unlist(train_x$draft_kings_points))
    train_x$draft_kings_points <- NULL
    
    test_y <- as.numeric(unlist(test_x$draft_kings_points))
    test_x$draft_kings_points <- NULL
  }
  
  train_x$fold <- NULL
  test_x$fold <- NULL
  
  salary <- test_x$fan_duel_salary
  
  
  # check 
  # stopifnot(unique(train_x$fan_duel_position) == unique(test_x$fan_duel_position))
  
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
    temp_dat <- as.data.frame(cbind(predicted_points = test.predictions, real_points = test_y, player_name = player_name, salary = salary))
    
    return(list(temp_dat, importance))
  }
  
  if(model_type == 'lm') {
    # recombine train_x and train_y
    lm_data <- as.data.frame(cbind(train_y, train_x))
    
    # run model
    lm_mod <- lm(train_y~., data = lm_data)
    
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
    temp_dat <- as.data.frame(cbind(predicted_points = test.predictions, real_points = test_y, player_name = player_name, salary = salary))
    
    return(list(temp_dat, trained_glmnet))
    
    
  }
  
}


# create a functions that takes a matrix of features, an outcome, and fold window, and then
# returns predictions and ground truth from model.
# 
# model_matrix = mod_dat
# train_window = c(1:39)
# test_window = c(40:44)
# include_team = TRUE
# include_opp = TRUE
# model_type = 'elastic_net'
# importance_fac = TRUE
# fixed_window = TRUE
# initial_window = 100
# horizon_window = 50
# lm_aic = TRUE
# # 

# make sure characters are factors
pred_team <- function(model_matrix,
                     train_window,
                     test_window,
                     response_variable,
                     include_team,
                     include_opp,
                     model_type,
                     importance_fac,
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
    stopifnot(all(sort(unique(train_x$team_opp)) == sort(unique(test_x$team_opp))))
    
  }
  
 
  # get training and test outcome
  train_y <- train_x$home_diff
  train_x$home_diff <- NULL
  train_x$win_ind_team <- train_x$win_loss <- NULL
  
  test_y <- test_x$home_diff
  test_x$home_diff <- NULL
  test_x$win_ind_team <- test_x$win_loss <- NULL
  
  # get closing spread as well
  test_closing_spread <- test_x$closing_spread_team


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
    
    if(importance_fac){
      
      temp <- varImp(model)[[1]]
      importance <- cbind(variable = rownames(temp), W = temp$W, L = temp$L)
      importance <- as.data.frame(importance)
      importance$W <- round(as.numeric(as.character(importance$W)), 2)
      importance$L <- round(as.numeric(as.character(importance$W)), 2)
      
      importance <- importance %>% arrange(-W)
    } else {
      temp <- varImp(model)[[1]]
      importance <- cbind(variable = rownames(temp), score = temp$Overall)
      importance <- as.data.frame(importance)
      importance$score <- round(as.numeric(as.character(importance$score)), 2)
      importance <- importance %>% arrange(-score)
      
    }
    
    # Predictions on test data
    
    # This returns 100 prediction with 1-100 lambdas
    test.predictions <- predict(model,
                                newdata = test_x)
    
    
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(predicted = as.character(test.predictions), 
                                    real = as.character(test_y)))
    temp_dat$predicted <- round(as.numeric(as.character(temp_dat$predicted)), 2)
    temp_dat$real <- round(as.numeric(as.character(temp_dat$real)), 2)
    
    
    return(list(temp_dat, importance))
  }
  
  if(model_type == 'lm') {
    # recombine train_x and train_y
    lm_data <- as.data.frame(cbind(train_y, train_x))
    
    # run model
    lm_mod <- lm(train_y~., data = lm_data)
    
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
    temp_dat <- as.data.frame(cbind(predicted_points = test.predictions, real_points = test_y, 
                                    closing_spread = test_closing_spread))
    
    return(list(temp_dat, trained_glmnet))
    
    
  }
  
}

