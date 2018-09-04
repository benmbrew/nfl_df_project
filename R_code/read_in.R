# this script will read in the nfl data we have and clean, combine, and explore
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
dat_2016 <- dat_2016[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2016$position),]
dat_2017 <- dat_2017[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2017$position),]

# remove defensive columns as well
dat_2016 <- dat_2016[,!grepl('def_', names(dat_2016))]
dat_2017 <- dat_2017[,!grepl('def_', names(dat_2017))]

# take care of rams issue being in both LA and st louits
dat_2016$team <- ifelse(grepl('Rams', dat_2016$team),'Rams (St.Louis, LA)', dat_2016$team)
dat_2017$team <- ifelse(grepl('Rams', dat_2017$team),'Rams (St.Louis, LA)', dat_2017$team)

# opponent
dat_2016$opponent <- ifelse(grepl('Rams', dat_2016$opponent),'Rams (St.Louis, LA)', dat_2016$opponent)
dat_2017$opponent <- ifelse(grepl('Rams', dat_2017$opponent),'Rams (St.Louis, LA)', dat_2017$opponent)

# first recode position PR-WR
dat_2016$position <- gsub('PR-WR', 'WR', dat_2016$position)

# get a vector of player names for both year (so we can use them to join on the fantasy data)
names_2016 <- unique(dat_2016$player)
names_2017 <- unique(dat_2017$player)
all_player_names <- union(names_2016, names_2017)

rm(names_2016, names_2017)
# get position statistics
# create list for results
position_data_list_2016 <- list()
position_data_list_2017 <- list()

# get all positions
all_positions <- unique(dat_2016$position)

# loop through each position and get corresponding statitics 
for(position in 1:length(all_positions)){
  position_name <- all_positions[position]
  position_data_list_2016[[position]] <- get_position_stats(dat_2016, pos_type = position_name)
  position_data_list_2017[[position]] <- get_position_stats(dat_2017, pos_type = position_name)
}

rm(dat_2016, dat_2017)

# get data from list for 2016
wr_16 <- position_data_list_2016[[1]]
qb_16 <- position_data_list_2016[[2]]
te_16 <- position_data_list_2016[[3]]
k_16 <- position_data_list_2016[[4]]
rb_16 <- position_data_list_2016[[5]]

# get data from list 2017
wr_17 <- position_data_list_2017[[1]]
qb_17 <- position_data_list_2017[[2]]
te_17 <- position_data_list_2017[[3]]
k_17 <- position_data_list_2017[[4]]
rb_17 <- position_data_list_2017[[5]]

rm(position_data_list_2016, position_data_list_2017)
# apply the featurize_player_data function to get past cumulative 
# and moving avg statistic for each player position
# for 2016
wr_16 <- featurize_player_data(wr_16, position_name = 'WR')
qb_16 <- featurize_player_data(qb_16, position_name = 'QB')
te_16 <- featurize_player_data(te_16, position_name = 'TE')
k_16 <- featurize_player_data(k_16, position_name = 'K')
rb_16 <- featurize_player_data(rb_16, position_name = 'RB')

# for 2017
wr_17 <- featurize_player_data(wr_17, position_name = 'WR')
qb_17 <- featurize_player_data(qb_17, position_name = 'QB')
te_17 <- featurize_player_data(te_17, position_name = 'TE')
k_17 <- featurize_player_data(k_17, position_name = 'K')
rb_17 <- featurize_player_data(rb_17, position_name = 'RB')

# # get opponent data for each players week
# temp_dat <- qb_16
# i = 1
# 
# get_opponent_data <- function(temp_dat){
#   
#   # get a vector of unique opponents
#   unique_opponents <- unique(temp_dat$opponent)
#   
#   # loop through each opponent and get past statistics (and maybe rankings?)
#   for(i in 1:length(unique_opponents)){
#     this_opp <- unique_opponents[i]
#     sub_opp <- temp_dat[temp_dat$team == this_opp,]
#     # sort by week
#     sub_opp <- sub_opp %>% arrange(week)
#     
#   }
#   
# }

# combine data for each year

# qb
qb_all <- rbind(qb_16,
                qb_17)
rm(qb_16, 
   qb_17)

# rb
rb_all <- rbind(rb_16,
                rb_17)
rm(rb_16, 
   rb_17)

# te
te_all <- rbind(te_16,
                te_17)
rm(te_16, 
   te_17)

# wr
wr_all <- rbind(wr_16,
                wr_17)
rm(wr_16, 
   wr_17)

# k
k_all <- rbind(k_16,
                k_17)
rm(k_16, 
   k_17)

#----------------------------------
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

# take care of rams situation
dat_team_2016$team <- ifelse(grepl('Rams', dat_team_2016$team),'Rams (St.Louis, LA)', dat_team_2016$team)
dat_team_2017$team <- ifelse(grepl('Rams', dat_team_2017$team),'Rams (St.Louis, LA)', dat_team_2017$team)

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

# get opposing team statistics for each game
dat_team_2016 <- get_opposing_team_stats(dat_team_2016)
dat_team_2017 <- get_opposing_team_stats(dat_team_2017)

# add year indicator
dat_team_2016$year <- as.character('2016')
dat_team_2017$year <- as.character('2017')

# combine data sets and save for joining and modelling
dat_team <- rbind(dat_team_2016,
                  dat_team_2017)

# get a vector of team names 
team_names <- unique(dat_team$team)

rm(dat_team_2016,
   dat_team_2017)

# sort by year and week
dat_team$year <- as.numeric(dat_team$year)
dat_team$week <- as.numeric(dat_team$week)

dat_team <- dat_team %>% arrange(year, week)

# get a weekly ranking for each team for total off yds,  def yds, etc
dat_team <- get_team_ranks(temp_dat = dat_team)

# ------------------------------------------------------------
# read in fantasy data
dat_fan_off <- read_csv('../data/player_fan_offense.csv')
dat_fan_def <- read_csv('../data/player_fan_defense.csv')

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

# -------------------------------------------------------------
# read in draft kings scraped data from 2014-2017
dat_dk <- read_csv('../data/draft_kings_scrape.csv')

# read in fanduel scraped data from 2011-2016
dat_fd <- read_csv('../data/fan_duel_scrape.csv')

# read in fanduel 2016 
dat_fd_2016 <- read_csv('../data/fan_duel_2016_full.csv')

# make column names lower case
names(dat_dk) <- tolower(names(dat_dk))
names(dat_fd) <- tolower(names(dat_fd))
names(dat_fd_2016) <- tolower(names(dat_fd_2016))

# recode column names
names(dat_dk) <- c('week', 'year', 'game_id', 'name', 'draft_kings_position', 'team', 
                   'venue', 'opponent', 'draft_kings_points', 'draft_kings_salary')
names(dat_fd) <- c('week', 'year', 'game_id', 'name', 'fan_duel_position', 'team', 
                   'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')
names(dat_fd_2016) <- c('week', 'year', 'game_id', 'last_name', 'first_name' ,'fan_duel_position', 'team', 
                   'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')

# subset to offense and defense 
dk_offense <- dat_dk[!grepl('Def', dat_dk$draft_kings_position),]
dk_defense <- dat_dk[grepl('Def', dat_dk$draft_kings_position),]

fd_offense <- dat_fd[!grepl('Def', dat_fd$fan_duel_position),]
fd_defense <- dat_fd[grepl('Def', dat_fd$fan_duel_position),]

fd_offense_2016 <- dat_fd_2016[!grepl('Def', dat_fd_2016$fan_duel_position),]
fd_defense_2016 <- dat_fd_2016[grepl('Def', dat_fd_2016$fan_duel_position),]

rm(dat_dk, dat_fd, dat_fd_2016)

# put df_offense_@016 into the format of the others so the match player names funciton works
fd_offense_2016$name <- paste0(fd_offense_2016$last_name, ', ', fd_offense_2016$first_name)

# apply the function to homogenize names of players across data sets
dk_offense <- match_player_names(dk_offense, df_type = 'dk')
fd_offense <- match_player_names(fd_offense, df_type = 'fd')
fd_offense_2016 <- match_player_names(fd_offense_2016, df_type = 'fd')

# Team dictionary for offense
# ------------------------------------------------------------
# write csv to creat team dictaionary by hand
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$name)), real_names = sort(team_names))), '../data/team_dict_off.csv')

# read in team dictionary
team_dict_off <- read_csv('../data/team_dict_off.csv')

# join team dictionary and dk_offense
dk_offense <- inner_join(dk_offense, team_dict_off, by = c('team' = 'old_team_name'))
fd_offense <- inner_join(fd_offense, team_dict_off, by = c('team' = 'old_team_name'))
fd_offense_2016 <- inner_join(fd_offense_2016, team_dict_off, by = c('team' = 'old_team_name'))

dk_offense$team <- fd_offense$team <- fd_offense_2016$team <- NULL
# rearrange columns and rename

dk_offense <- dk_offense[, c('year', 'week', 'player', 'new_team_name','game_id', 'draft_kings_position',
                             'venue', 'opponent', 'draft_kings_points', 'draft_kings_salary')]
fd_offense <- fd_offense[, c('year', 'week', 'player', 'new_team_name', 'game_id', 'fan_duel_position', 
                             'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')]
fd_offense_2016 <- fd_offense_2016[, c('year', 'week', 'player', 'new_team_name', 'game_id', 'fan_duel_position', 
                                       'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')]

# Team dictionary for defense
# ------------------------------------------------------------
# write csv to creat team dictaionary by hand
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$name)), real_names = sort(team_names))), '../data/team_dict_def.csv')

# read in team dictionary
team_dict_def <- read_csv('../data/team_dict_def.csv')

# join team dictionary and dk_defense
dk_defense <- inner_join(dk_defense, team_dict_def, by = c('name' = 'old_names'))
fd_defense <- inner_join(fd_defense, team_dict_def, by = c('name' = 'old_names'))
fd_defense_2016 <- inner_join(fd_defense_2016, team_dict_def, by = c('last_name' = 'old_names'))

dk_defense$name <- fd_defense$name <- fd_defense_2016$last_name <- 
  fd_defense_2016$first_name <- NULL

# rearrange columns and rename

dk_defense <- dk_defense[, c('year', 'week', 'real_names', 'game_id', 'draft_kings_position', 
                             'venue', 'opponent', 'draft_kings_points', 'draft_kings_salary')]
fd_defense <- fd_defense[, c('year', 'week', 'real_names', 'game_id', 'fan_duel_position', 
                             'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')]
fd_defense_2016 <- fd_defense_2016[, c('year', 'week', 'real_names', 'game_id', 'fan_duel_position', 
                                       'venue', 'opponent', 'fan_duel_points', 'fan_duel_salary')]

# Opponoent dictionary for offense
# ------------------------------------------------------------------
# write dictionary again for opponenets 
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$oppt)), real_names = sort(team_names))), '../data/opp_dict_off.csv')

# read in opponent dictionary
opp_dict_off <- read_csv('../data/opp_dict_off.csv')

# join oppt dictionary and dk_offense and fd_offense
dk_offense <- inner_join(dk_offense, opp_dict_off, by = c('opponent' = 'old_oppt_name'))
fd_offense <- inner_join(fd_offense, opp_dict_off, by = c('opponent' = 'old_oppt_name'))
fd_offense_2016 <- inner_join(fd_offense_2016, opp_dict_off, by = c('opponent' = 'old_oppt_name'))

# Opponoent dictionary for defense
# ------------------------------------------------------------------
# write dictionary again for opponenets 
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$oppt)), real_names = sort(team_names))), '../data/opp_dict_def.csv')

# read in opponent dictionary
opp_dict_def <- read_csv('../data/opp_dict_def.csv')

# join oppt dictionary and dk_defense and fd_defense
dk_defense <- inner_join(dk_defense, opp_dict_def, by = c('opponent' = 'old_oppt_name'))
fd_defense <- inner_join(fd_defense, opp_dict_def, by = c('opponent' = 'old_oppt_name'))
fd_defense_2016 <- inner_join(fd_defense_2016, opp_dict_def, by = c('opponent' = 'old_oppt_name'))

dk_defense$opponent <- fd_defense$opponent <- fd_defense_2016$opponent <- NULL

# combine fd_offense and fd_offense_2016
fd_offense <- rbind(fd_offense,
                    fd_offense_2016)
rm(fd_offense_2016)

# combine fd_defense and fd_defense_2016
fd_defense <- rbind(fd_defense,
                    fd_defense_2016)
rm(fd_defense_2016)

# join fd_offense with dk_offense
fd_dk_offense <- left_join(fd_offense, dk_offense, by = c('player', 'week', 'year'))

# remove columns with .y and remove .x from exisiting colyumns
fd_dk_offense <- fd_dk_offense[, !grepl('.y', names(fd_dk_offense), fixed = TRUE)]
names(fd_dk_offense) <- gsub('.x', '', names(fd_dk_offense), fixed = TRUE)

rm(fd_offense, dk_offense)
# join fd_defense with dk_defense
fd_dk_defense <- left_join(fd_defense, dk_defense, by = c('year', 'week', 'real_names'))

# remove columns with .y and remove .x from exisiting colyumns
fd_dk_defense <- fd_dk_defense[, !grepl('.y', names(fd_dk_defense), fixed = TRUE)]
names(fd_dk_defense) <- gsub('.x', '', names(fd_dk_defense), fixed = TRUE)

rm(fd_defense, dk_defense)

# now combine fd_dk_offense with dat_fan_off
fd_dk_offense$year <- as.character(fd_dk_offense$year)
sort(names(fd_dk_offense)); sort(names(dat_fan_off))
names(dat_fan_off) <- gsub('_fan', '', names(dat_fan_off))
names(dat_fan_off) <- gsub('_pts', '_points', names(dat_fan_off))

# use function to get game id for dat_fan_off
# dat_fan_off <- get_fan_game_id(dat_fan_off)

# remove date from dat_fan_off
dat_fan_off$date <- NULL
dat_fan_off$first_team <- dat_fan_off$second_team <- NULL

fd_dk_offense$opponent <- NULL
names(fd_dk_offense)[4] <- 'team'
names(fd_dk_offense)[10] <- 'opponent'
fd_dk_offense$game_id <- NULL

# recode venue for both fantasy datasets
dat_fan_off$venue <- ifelse(dat_fan_off$venue == 'a', 'Road',
                            ifelse(dat_fan_off$venue == 'h', 'Home', dat_fan_off$venue))
dat_fan_def$venue <- ifelse(dat_fan_def$venue == 'a', 'Road',
                            ifelse(dat_fan_def$venue == 'h', 'Home', dat_fan_def$venue))
fd_dk_offense$venue <- ifelse(fd_dk_offense$venue == 'a', 'Road',
                          ifelse(fd_dk_offense$venue == 'h', 'Home', fd_dk_offense$venue))
fd_dk_defense$venue <- ifelse(fd_dk_defense$venue == 'a', 'Road',
                          ifelse(fd_dk_defense$venue == 'h', 'Home', fd_dk_defense$venue))

# combine into one data set using row bind
dat_fan_off <- rbind(dat_fan_off,
                     fd_dk_offense)

rm(fd_dk_offense)

# now combine fd_dk_defensee with dat_fan_def
fd_dk_defense$year <- as.character(fd_dk_defense$year)
fd_dk_defense$week <- as.numeric(fd_dk_defense$week)
dat_fan_def$week <- as.numeric(dat_fan_def$week)

# homognenize names
sort(names(dat_fan_def));sort(names(fd_dk_defense))
dat_fan_def$date <- NULL
names(dat_fan_def) <- gsub('_fan', '', names(dat_fan_def))
names(dat_fan_def) <- gsub('_pts', '_points', names(dat_fan_def))
# dat_fan_def <- get_fan_game_id(dat_fan_def)
dat_fan_def$first_team <- dat_fan_def$second_team <- 
  dat_fan_def$date <- NULL
fd_dk_defense$fan_duel_position <- fd_dk_defense$draft_kings_position <- NULL
fd_dk_defense$team <- NULL
names(fd_dk_defense)[8] <- 'opponent'
names(fd_dk_defense)[3] <- 'team'
fd_dk_defense$game_id <- NULL
# combine def data with row binding
dat_fan_def <- rbind(dat_fan_def,
                     fd_dk_defense)

# take care of rams issue being in both LA and st louits
dat_fan_off$team <- ifelse(grepl('Rams', dat_fan_off$team),'Rams (St.Louis, LA)', dat_fan_off$team)
dat_fan_def$team <- ifelse(grepl('Rams', dat_fan_def$team),'Rams (St.Louis, LA)', dat_fan_def$team)

# opponenet 
dat_fan_off$opponent <- ifelse(grepl('Rams', dat_fan_off$opponent),'Rams (St.Louis, LA)', dat_fan_off$opponent)
dat_fan_def$opponent <- ifelse(grepl('Rams', dat_fan_def$opponent),'Rams (St.Louis, LA)', dat_fan_def$opponent)

# remove extra objects
rm(team_dict_def, team_dict_off)
rm(opp_dict_def, opp_dict_off)
rm(fd_dk_defense)

# moving forward: featrurize fantasy data, get weekly ranks for each team (and player?),
# get data for last week alone. 

# offense 
dat_fan_off <- featurize_fantasy_data(dat_fan_off, offense = TRUE)
dat_fan_def <- featurize_fantasy_data(dat_fan_def, offense = FALSE)


# # save all data
# 
# save team data
saveRDS(dat_team, '../data/cleaned_data/team_data.csv')

# save individual data
saveRDS(qb_all, '../data/cleaned_data/player_data_qb.csv')
saveRDS(rb_all, '../data/cleaned_data/player_data_rb.csv')
saveRDS(wr_all, '../data/cleaned_data/player_data_wr.csv')
saveRDS(te_all, '../data/cleaned_data/player_data_te.csv')
saveRDS(k_all, '../data/cleaned_data/player_data_k.csv')

# save fantasy data
saveRDS(dat_fan_off, '../data/cleaned_data/fantasy_offense.csv')
saveRDS(dat_fan_def, '../data/cleaned_data/fantasy_defense.csv')

