
# this script will be used to predict fantasy points ONLY using fantasy data (more years.)



# source funtions
source('functions.R')

# read in data
dat_off <- readRDS('../data/model_data/dat_fan_off.rda')
dat_def <- readRDS('../data/model_data/dat_fan_def.rda')

# -----------------------------------------------------------------
# offense data

# recode fan_duel_position so K and PK are the same 
dat_off$fan_duel_position <- gsub('PK', 'K', dat_off$fan_duel_position)

# remove unneeded columns
dat_off$draft_kings_position <- NULL

# change year to numeric for correct order
dat_off$year <- as.numeric(dat_off$year)
dat_off$week <- as.numeric(dat_off$week)

# get folds - where each fold is the first week in my data and the final fold is the last week, regardless of year.
dat_off <- get_fantasy_off_folds(dat_off, season_length = 17)

# recode position varibale
dat_off$fan_duel_position <- gsub('#N/A', 'Other', dat_off$fan_duel_position, fixed = TRUE)

# change to factors
dat_off$week <- as.factor(dat_off$week)
dat_off$player <- as.factor(dat_off$player)
dat_off$team <- as.factor(dat_off$team)
dat_off$opponent <- as.factor(dat_off$opponent)
dat_off$venue <- as.factor(dat_off$venue)
dat_off$fan_duel_position <- as.factor(dat_off$fan_duel_position)

# break up into position level data
dat_qb <- dat_off %>% filter(fan_duel_position == 'QB')
dat_rb <- dat_off %>% filter(fan_duel_position == 'RB')
dat_wr <- dat_off %>% filter(fan_duel_position == 'WR')
dat_te <- dat_off %>% filter(fan_duel_position == 'TE')
dat_k <- dat_off %>% filter(fan_duel_position == 'K')

# assure that 

#### ---------------------------------------------------------------------------------------
# predict points for all positions using all players, use posistion as a feature though
mod_mat <- dat_off[, c('fan_duel_points','fold', 'team','opponent', 'venue', 'fan_duel_position',
                      'fan_duel_salary', 'cum_sum_fan_duel_salary', 'mov_avg_fan_duel_salary', 'last_week_fan_duel_salary',
                      'cum_sum_fan_duel_points', 'mov_avg_fan_duel_points', 'last_week_fan_duel_points')]


temp_pred <- pred_fantasy(model_matrix = mod_mat,
                         train_window = c(1:80), 
                         test_window = c(81:119), 
                         fantasy_type = 'fan_duel', 
                         include_team = TRUE,
                         include_opp = TRUE,
                         param_folds = NULL, 
                         param_repeats = NULL,
                         model_type = 'elastic_net', 
                         initial_window = 1000, 
                         fixed_window = TRUE, 
                         horizon_window = 200,
                         lm_aic = TRUE,
                         num_obs = 5000)



importance <- temp_pred[[2]]
ggplot(data = as.data.frame(temp_pred[[1]]), 
       aes(predicted_points, 
           real_points)) +
  geom_point(size = 1,
             color = 'white',
             pch = 1,
             stroke = 1,
             alpha = 0.7) +
  xlab('Predicted points') +
  ylab('Real points') +
  xlim(c(0, 50)) +
  ylim(c(0, 50)) +
  geom_abline(intercept = 0, 
              slope = 1) + 
  theme_dark()

#### ---------------------------------------------------------------------------------------
# predict points for each position using all players, but not as a feature

mod_mat <- dat_qb[, c('fan_duel_points','fold', 'team','opponent', 'venue', 'fan_duel_position',
                       'fan_duel_salary', 'cum_sum_fan_duel_salary', 'mov_avg_fan_duel_salary', 'last_week_fan_duel_salary',
                       'cum_sum_fan_duel_points', 'mov_avg_fan_duel_points', 'last_week_fan_duel_points')]

temp_pred <- pred_fantasy(model_matrix = mod_mat,
                          train_window = c(1:80), 
                          test_window = c(81:119), 
                          fantasy_type = 'fan_duel', 
                          include_team = TRUE,
                          include_opp = TRUE,
                          param_folds = NULL, 
                          param_repeats = NULL,
                          model_type = 'random_forest', 
                          initial_window = 60, 
                          fixed_window = TRUE, 
                          horizon_window = 40,
                          lm_aic = TRUE)

model <- temp_pred[[2]]
ggplot(temp_pred[[1]], 
       aes(predicted_points, 
           real_points)) +
  geom_point(size = 1,
             pch = 1,
             stroke = 1,
             alpha = 0.7) +
  xlab('Predicted points') +
  ylab('Real points') +
  xlim(c(0, 50)) +
  ylim(c(0, 50)) +
  geom_abline(intercept = 0, 
              slope = 1) + theme_bw()


#### ---------------------------------------------------------------------------------------
# predict points for each position using inidividual player models for each position
