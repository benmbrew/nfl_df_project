# this script will predict on qb data

# source functions scrip to get functions and libraries
source('model_functions.R')

# fantasy and playerr data 
qb_data <- readRDS('../data/model_data/qb_data.rda')

# fantasy, player,  and team data
qb_data_team <- readRDS('../data/model_data/qb_data_team.rda')

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

# get folds
qb_data <- get_data_folds(qb_data, season_length = 17, current_season_length = 4)
qb_data_team <- get_data_folds(qb_data_team, season_length = 17, current_season_length = 4)

# get complete cases (in this case remove the row where fan duel salary is NA)
qb_data <- qb_data[complete.cases(qb_data),]
qb_data_team <- qb_data_team[complete.cases(qb_data_team),]

# in the first model, pred_qb_all, we make predictions for each row in the data (~780 or ~790) with 
# player name as a feature.

# in second model, qb_pred_individual,we build a model for each individual player. 

# in potential third model, we could hide the name of the qb and use only the ranking to identify. in other words,
# we esteimate qb performance based on their ranking at time t. 



model_matrix = qb_data_team
train_window = c(1:34)
test_window = c(35:38)
include_palyer = TRUE
include_team = TRUE
include_opp = TRUE
model_type = 'elastic_net'
importance_fac = TRUE
fixed_window = TRUE
initial_window = 200
horizon_window = 50
lm_aic = TRUE

# make sure characters are factors
pred_qb_all <- function(model_matrix,
                        train_window,
                        test_window,
                        include_team,
                        include_opp,# only applicable if using team data
                        model_type,
                        fixed_window,
                        initial_window,
                        horizon_window,
                        lm_aic) {
  
  # change characters to factors in triain_x - player, team, opponent, venue, year
  model_matrix$opponent <- as.factor(model_matrix$opponent)
  model_matrix$team <- as.factor(model_matrix$team)
  model_matrix$player <- as.factor(model_matrix$player)
  model_matrix$venue <- as.factor(model_matrix$venue)
  model_matrix$year <- as.factor(model_matrix$year)

  
  
  
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
    shared_opponents <- intersect(train_x$opponent, test_x$opponent)
    
    # subset by shared opponents
    train_x <- train_x %>% filter(opponent %in% shared_opponents)
    test_x <- test_x %>% filter(opponent %in% shared_opponents)
    
    # stop the model if these conditions are not met
    stopifnot(all(sort(unique(train_x$opponent)) == sort(unique(test_x$opponent))))
    
  }
  
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

