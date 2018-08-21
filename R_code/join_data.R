# this script will read in all data and join where necessary
# source functions sczcript to get custom functions 
source('functions.R')

# read in all data

# read team data
dat_team <- readRDS('../data/cleaned_data/team_data.csv')

# read individual data
qb_all <- as.data.frame(readRDS('../data/cleaned_data/player_data_qb.csv'))
rb_all <- as.data.frame(readRDS('../data/cleaned_data/player_data_rb.csv'))
wr_all <- as.data.frame(readRDS('../data/cleaned_data/player_data_wr.csv'))
te_all <- as.data.frame(readRDS('../data/cleaned_data/player_data_te.csv'))
k_all <-  as.data.frame(readRDS('../data/cleaned_data/player_data_k.csv'))

# read fantasy data
dat_fan_off <- readRDS('../data/cleaned_data/fantasy_offense.csv')
dat_fan_def <- readRDS('../data/cleaned_data/fantasy_defense.csv')

# temporarily remove game_id
# dat_fan_off$game_id <- dat_team$game_id <- NULL

# make week a character 
dat_team$week <- as.character(dat_team$week)
dat_fan_def$week <- as.character(dat_fan_def$week)
dat_fan_off$week <- as.character(dat_fan_off$week)
qb_all$week <- as.character(qb_all$week)
wr_all$week <- as.character(wr_all$week)
rb_all$week <- as.character(rb_all$week)
te_all$week <- as.character(te_all$week)
k_all$week <- as.character(k_all$week)

# join each individual player level data (k_all, qb_all, rb_all, etc) with 
# dat_fan_off

# homogenize player names across data types 
# qb
dat_fan_off$player <- gsub(' III', '', dat_fan_off$player)
dat_fan_off$player <- gsub('E.J.', 'EJ', dat_fan_off$player)

# join by player, year, week, team
qb_data <- inner_join(dat_fan_off, qb_all, by = c('player', 'year', 'week', 'team'))
rb_data <- inner_join(dat_fan_off, rb_all, by = c('player', 'year', 'week', 'team'))
wr_data <- inner_join(dat_fan_off, wr_all, by = c('player', 'year', 'week', 'team'))
te_data <- inner_join(dat_fan_off, te_all, by = c('player', 'year', 'week', 'team'))
k_data <-  inner_join(dat_fan_off, k_all, by = c('player', 'year', 'week', 'team'))

# remove dup names
names(qb_data) <- gsub('.x', '', names(qb_data), fixed = TRUE)
qb_data$opponent.y <- NULL
names(rb_data) <- gsub('.x', '', names(rb_data), fixed = TRUE)
rb_data$opponent.y <- NULL
names(wr_data) <- gsub('.x', '', names(wr_data), fixed = TRUE)
wr_data$opponent.y <- NULL
names(te_data) <- gsub('.x', '', names(te_data), fixed = TRUE)
te_data$opponent.y <- NULL
names(k_data) <- gsub('.x', '', names(k_data), fixed = TRUE)
k_data$opponent.y <- NULL

# join team data to each position level data with week, team, venue, year
qb_data_team <- inner_join(qb_data, dat_team, by = c('team', 'year', 'week', 'venue'))
rb_data_team <- inner_join(rb_data, dat_team, by = c('team', 'year', 'week', 'venue'))
wr_data_team <- inner_join(wr_data, dat_team, by = c('team', 'year', 'week', 'venue'))
te_data_team <- inner_join(te_data, dat_team, by = c('team', 'year', 'week', 'venue'))
k_data_team <- inner_join(k_data, dat_team, by = c('team', 'year', 'week', 'venue'))

rm(dat_team)

# remove dups
names(qb_data_team) <- gsub('.x', '', names(qb_data_team), fixed = TRUE)
qb_data_team$game_id.y <- qb_data_team$last_game.y <- qb_data_team$date.y <- NULL

names(rb_data_team) <- gsub('.x', '', names(rb_data_team), fixed = TRUE)
rb_data_team$game_id.y <- rb_data_team$date.y <- NULL

names(wr_data_team) <- gsub('.x', '', names(wr_data_team), fixed = TRUE)
wr_data_team$game_id.y <- wr_data_team$last_game.y <- wr_data_team$date.y <- NULL

names(te_data_team) <- gsub('.x', '', names(te_data_team), fixed = TRUE)
te_data_team$game_id.y <- te_data_team$last_game.y <- te_data_team$date.y <- NULL

names(k_data_team) <- gsub('.x', '', names(k_data_team), fixed = TRUE)
k_data_team$game_id.y <- k_data_team$last_game.y <- k_data_team$date.y <- NULL

# save data for modelling

# just fantasy data
saveRDS(dat_fan_off, '../data/model_data/dat_fan_off.rda')
saveRDS(dat_fan_def, '../data/model_data/dat_fan_def.rda')

# fantasy and palyer data data
saveRDS(qb_data, '../data/model_data/qb_data.rda')
saveRDS(rb_data, '../data/model_data/rb_data.rda')
saveRDS(wr_data, '../data/model_data/wr_data.rda')
saveRDS(te_data, '../data/model_data/te_data.rda')
saveRDS(k_data, '../data/model_data/k_data.rda')


# fantasy and team data
saveRDS(qb_data_team, '../data/model_data/qb_data_team.rda')
saveRDS(rb_data_team, '../data/model_data/rb_data_team.rda')
saveRDS(wr_data_team, '../data/model_data/wr_data_team.rda')
saveRDS(te_data_team, '../data/model_data/te_data_team.rda')
saveRDS(k_data_team, '../data/model_data/k_data_team.rda')



