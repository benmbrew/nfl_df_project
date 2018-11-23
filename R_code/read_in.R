# this script will read in the nfl data we have and clean, combine, and explore
# source functions script to get custom functions 
source('functions.R')

# -----------------------------------------------------------
# read in player level data
dat_2016 <- read_csv('../data/player_2016.csv')
dat_2017 <- read_csv('../data/player_2017.csv')

# curate player data
dat_in_season_player <- read_csv('../data/in_season_player_new.csv')

# read in dictionary
in_season_player_dict <- read.csv('../data/in_season_player_dict.csv')

# overwire colnames
names(in_season_player_dict) <- c('original', 'in_season', 'new_in_season')

# remove NAs in new_in_season
in_season_player_dict <- in_season_player_dict %>% filter(!is.na(new_in_season))
in_season_player_dict$new_in_season <- as.character(in_season_player_dict$new_in_season)

# get a vector of column names to keep
keep_cols <- as.character(in_season_player_dict$in_season)

# subet data by keep cols
dat_in_season_player <- dat_in_season_player[, keep_cols]
dat_in_season_player$`GAME-ID` <- dat_in_season_player$`PLAYER-ID` <- NULL

# now overlay the new_in_season vector over the names of data
names(dat_in_season_player) <- in_season_player_dict$new_in_season
dat_2018 <- dat_in_season_player 

rm(dat_in_season_player, in_season_player_dict)

# add year column
dat_2016$year <- '2016'
dat_2017$year <- '2017'
dat_2018$year <- '2018'

# make lower case 
names(dat_2016) <- tolower(names(dat_2016))
names(dat_2017) <- tolower(names(dat_2017))
names(dat_2018) <- tolower(names(dat_2018))


# remove playoffs from dataset
dat_2016 <- dat_2016[!grepl('Playoffs|Postseason', dat_2016$dataset),]
dat_2016$dataset <- NULL
dat_2017 <- dat_2017[!grepl('Playoffs|Postseason', dat_2017$dataset),]
dat_2017$dataset <- NULL
dat_2018 <- dat_2018[!grepl("(Y/N)", dat_2018$dataset, fixed = TRUE),]
dat_2018$dataset <- NULL


# convert date to date object
dat_2016$date <- as.Date(dat_2016$date, format = '%m/%d/%Y')
dat_2017$date <- as.Date(dat_2017$date, format = '%m/%d/%Y')
dat_2018$date <- as.Date(dat_2018$date, format = '%d-%m-%Y')

# remove all defensive players (for now, might want them)
dat_2016 <- dat_2016[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2016$position),]
dat_2017 <- dat_2017[grepl('QB|WR|^TE$|^K$|RB|PR-WR', dat_2017$position),]
dat_2018 <- dat_2018[grepl('QB|WR|^TE$|^K$|RB|WR', dat_2018$position),]


# remove defensive columns as well
dat_2016 <- dat_2016[,!grepl('def_', names(dat_2016))]
dat_2017 <- dat_2017[,!grepl('def_', names(dat_2017))]
dat_2018 <- dat_2018[,!grepl('def_', names(dat_2018))]


# take care of rams issue being in both LA and st louits
dat_2016$team <- ifelse(grepl('Rams', dat_2016$team),'Rams (St.Louis, LA)', dat_2016$team)
dat_2017$team <- ifelse(grepl('Rams', dat_2017$team),'Rams (St.Louis, LA)', dat_2017$team)
dat_2018$team <- ifelse(grepl('Rams', dat_2018$team),'Rams (St.Louis, LA)', dat_2018$team)

# opponent
dat_2016$opponent <- ifelse(grepl('Rams', dat_2016$opponent),'Rams (St.Louis, LA)', dat_2016$opponent)
dat_2017$opponent <- ifelse(grepl('Rams', dat_2017$opponent),'Rams (St.Louis, LA)', dat_2017$opponent)
dat_2018$opponent <- ifelse(grepl('Rams', dat_2018$opponent),'Rams (St.Louis, LA)', dat_2018$opponent)

# take care of Chargers issue being in both LA and st louits
dat_2016$team <- ifelse(grepl('Chargers', dat_2016$team),'Chargers (SD, LA)', dat_2016$team)
dat_2017$team <- ifelse(grepl('Chargers', dat_2017$team),'Chargers (SD, LA)', dat_2017$team)
dat_2018$team <- ifelse(grepl('Chargers', dat_2018$team),'Chargers (SD, LA)', dat_2018$team)

# opponent
dat_2016$opponent <- ifelse(grepl('Chargers', dat_2016$opponent),'Chargers (SD, LA)', dat_2016$opponent)
dat_2017$opponent <- ifelse(grepl('Chargers', dat_2017$opponent),'Chargers (SD, LA)', dat_2017$opponent)
dat_2018$opponent <- ifelse(grepl('Chargers', dat_2018$opponent),'Chargers (SD, LA)', dat_2018$opponent)

# first recode position PR-WR
dat_2016$position <- gsub('PR-WR', 'WR', dat_2016$position)

# get a vector of player names for both year (so we can use them to join on the fantasy data)
names_2016 <- unique(dat_2016$player)
names_2017 <- unique(dat_2017$player)
names_2018 <- unique(dat_2018$player)
all_player_names <- Reduce(union, list(names_2016, names_2017, names_2018))

rm(names_2016, names_2017, names_2018)
# get position statistics
# create list for results
position_data_list_2016 <- list()
position_data_list_2017 <- list()
position_data_list_2018 <- list()


# get all positions
all_positions <- unique(dat_2016$position)

# loop through each position and get corresponding statitics 
for(position in 1:length(all_positions)){
  position_name <- all_positions[position]
  position_data_list_2016[[position]] <- get_position_stats(dat_2016, pos_type = position_name)
  position_data_list_2017[[position]] <- get_position_stats(dat_2017, pos_type = position_name)
  position_data_list_2018[[position]] <- get_position_stats(dat_2018, pos_type = position_name)
  
}

rm(dat_2016, dat_2017, dat_2018)

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

# get data from list 2018
wr_18 <- position_data_list_2018[[1]]
qb_18 <- position_data_list_2018[[2]]
te_18 <- position_data_list_2018[[3]]
k_18 <- position_data_list_2018[[4]]
rb_18 <- position_data_list_2018[[5]]

rm(position_data_list_2016, position_data_list_2017, position_data_list_2018)

# combine data for each year

# # qb
# qb_all <- rbind(qb_16,
#                 qb_17)
# rm(qb_16, 
#    qb_17)
# 
# # rb
# rb_all <- rbind(rb_16,
#                 rb_17)
# rm(rb_16, 
#    rb_17)
# 
# # te
# te_all <- rbind(te_16,
#                 te_17)
# rm(te_16, 
#    te_17)
# 
# # wr
# wr_all <- rbind(wr_16,
#                 wr_17)
# rm(wr_16, 
#    wr_17)
# 
# # k
# k_all <- rbind(k_16,
#                k_17)
# rm(k_16, 
#    k_17)

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

# for 2018
wr_18 <- featurize_player_data(wr_18, position_name = 'WR')
qb_18 <- featurize_player_data(qb_18, position_name = 'QB')
te_18 <- featurize_player_data(te_18, position_name = 'TE')
k_18 <- featurize_player_data(k_18, position_name = 'K')
rb_18 <- featurize_player_data(rb_18, position_name = 'RB')

# qb
qb_all <- rbind(qb_16,
                qb_17,
                qb_18)
rm(qb_16, 
   qb_17,
   qb_18)

# rb
rb_all <- rbind(rb_16,
                rb_17,
                rb_18)
rm(rb_16, 
   rb_17,
   rb_18)

# te
te_all <- rbind(te_16,
                te_17,
                te_18)
rm(te_16, 
   te_17,
   te_18)

# wr
wr_all <- rbind(wr_16,
                wr_17,
                wr_18)
rm(wr_16, 
   wr_17,
   wr_18)

# k
k_all <- rbind(k_16,
               k_17,
               k_18)
rm(k_16, 
   k_17,
   k_18)

#----------------------------------
# read in team data
source('functions.R')

# read in season data for 2016 and 2017
dat_team_2016 <- read_csv('../data/team_2016.csv')
dat_team_2017 <- read_csv('../data/team_2017.csv')

# get latest data
dat_in_season_team <- read.csv('../data/in_season_team_newer.csv', stringsAsFactors = FALSE)

# # create dictionary to map names
# temp <- as.data.frame(cbind(names(dat_team_2016), names(dat_in_season_team)))
# write_csv(temp, '~/Desktop/temp.csv')

# read in dictionary
in_season_team_dict <- read_csv('../data/in_season_team_dict.csv')

# remove NAs in new_in_season
in_season_team_dict <- in_season_team_dict %>% filter(!is.na(new_in_season))

# get a vector of column names to keep
keep_cols <- as.character(in_season_team_dict$in_season)

# subet data by keep cols
dat_in_season_team <- dat_in_season_team[, keep_cols]

# temp <- as.data.frame(cbind(names(dat_in_season_team), in_season_team_dict$new_in_season))
# now overlay the new_in_season vector over the names of data
names(dat_in_season_team) <- in_season_team_dict$new_in_season
dat_team_2018 <- dat_in_season_team
rm(dat_in_season_team)

# remove vars from 2016 and 2017 data that arent in 2018 data
dat_team_2017$spread_odds_movts <- dat_team_2016$spread_odds_movts <-
  dat_team_2017$fan_duel_dst_pts <- dat_team_2016$fan_duel_dst_pts <-
  dat_team_2017$draft_kings_dst_pts <- dat_team_2016$draft_kings_dst_pts <- 
  NULL

# take care of rams situation
dat_team_2016$team <- ifelse(grepl('Rams', dat_team_2016$team),'Rams (St.Louis, LA)', dat_team_2016$team)
dat_team_2017$team <- ifelse(grepl('Rams', dat_team_2017$team),'Rams (St.Louis, LA)', dat_team_2017$team)
dat_team_2018$team <- ifelse(grepl('Rams', dat_team_2018$team),'Rams (St.Louis, LA)', dat_team_2018$team)

# chargers 
dat_team_2016$team <- ifelse(grepl('Chargers', dat_team_2016$team),'Chargers (SD, LA)', dat_team_2016$team)
dat_team_2017$team <- ifelse(grepl('Chargers', dat_team_2017$team),'Chargers (SD, LA)', dat_team_2017$team)
dat_team_2018$team <- ifelse(grepl('Chargers', dat_team_2018$team),'Chargers (SD, LA)', dat_team_2018$team)


# remove playoffs from dataset
dat_team_2016 <- dat_team_2016[!grepl('Wild|Division|Conference|Super', dat_team_2016$week),]
dat_team_2017 <- dat_team_2017[!grepl('Wild|Division|Conference|Super', dat_team_2017$week),]

# create a game id for each game present int he data
dat_team_2016$game_id <- rep(1:(nrow(dat_team_2016)/2), each=2)
dat_team_2017$game_id <- rep(1:(nrow(dat_team_2017)/2), each=2)
dat_team_2018$game_id <- rep(1:(nrow(dat_team_2018)/2), each=2)


# remove opening_odds, closing odds, spread_odds_movts,
dat_team_2016$opening_odds <- dat_team_2016$closing_odds <- dat_team_2016$spread_odds_movts <- 
  dat_team_2016$halftime <- NULL
dat_team_2017$opening_odds <- dat_team_2017$closing_odds <- dat_team_2017$spread_odds_movts <- 
  dat_team_2017$halftime <- NULL
dat_team_2018$opening_odds <- dat_team_2018$closing_odds <- dat_team_2018$spread_odds_movts <- 
  dat_team_2018$halftime <- NULL

# convert date to date object
dat_team_2016$date <- as.Date(dat_team_2016$date, format = '%m/%d/%Y')
dat_team_2017$date <- as.Date(dat_team_2017$date, format = '%m/%d/%Y')
dat_team_2018$date <- as.Date(dat_team_2018$date, format = '%Y-%m-%d')

# use win_lost function from functions.R
dat_team_2016 <- get_win_loss(dat_team_2016)
dat_team_2017 <- get_win_loss(dat_team_2017)
dat_team_2018 <- get_win_loss(dat_team_2018)


# apply function to get game number for each team
# combine years
dat_team_combined <- rbind(dat_team_2016, 
                           dat_team_2017,
                           dat_team_2018)

# When not combined, we featurize at the beginning of each season - for now only take the combined 
# data that featurize with t=0 at the beginning of 2016 season
# dat_team_2016 <- get_game_num(dat_team_2016)
# dat_team_2017 <- get_game_num(dat_team_2017)
# dat_team_2018 <- get_game_num(dat_team_2018)

dat_team_combined <- get_game_num(dat_team_combined)

# get game id for dat_team_combined and remove the current one, because that is a reflection of 2016 and 2017
# being separated.

#  order by numeric rownames
dat_team_combined <- dat_team_combined[order(as.numeric(rownames(dat_team_combined))),]

# get game number through all years
dat_team_combined$game_id <- rep(1:(nrow(dat_team_combined)/2), each=2)

# Use custom function 'featurize_team_data' sourced from functions.R
# this function loops through each team and creates features for each row (game) with 
# previous weeks statistics 

# dat_team_2016 <- featurize_team_data(dat_team_2016)
# dat_team_2017 <- featurize_team_data(dat_team_2017)
# dat_team_2018 <- featurize_team_data(dat_team_2018)

dat_team_combined$opening_total <- as.numeric(dat_team_combined$opening_total)
dat_team_combined <- featurize_team_data(dat_team_combined)
names(dat_team_combined)[5] <- 'final_team'
names(dat_team_combined)[6] <- 'closing_spread_team'

# get opposing team statistics for each game
# dat_team_2016 <- get_opposing_team_stats(dat_team_2016)
# dat_team_2017 <- get_opposing_team_stats(dat_team_2017)
# dat_team_2018 <- get_opposing_team_stats(dat_team_2018)

# get opposing team stats
dat_team_combined <- get_opposing_team_stats(dat_team_combined)

# add year indicator
# dat_team_2016$year <- format(as.Date(dat_team_2016$date, format="%d/%m/%Y"),"%Y")
# dat_team_2017$year <- format(as.Date(dat_team_2017$date, format="%d/%m/%Y"),"%Y")
# dat_team_2018$year <- format(as.Date(dat_team_2017$date, format = '%Y-%m-%d'),"%Y")

dat_team_combined$year <- format(as.Date(dat_team_combined$date, format="%d/%m/%Y"),"%Y")

# combine data sets and save for joining and modelling
# dat_team <- rbind(dat_team_2016,
#                   dat_team_2017,
#                   dat_team_2018)

# get a vector of team names 
team_names <- unique(dat_team_combined$team)

rm(dat_team_2016,
   dat_team_2017,
   dat_team_2018)

# sort by year and week
# separated by year
# dat_team$year <- as.numeric(dat_team$year)
# dat_team$week <- as.numeric(dat_team$week)
# dat_team <- dat_team %>% arrange(year, week)
# dat_team <- get_team_ranks(temp_dat = dat_team)

# combined data
dat_team_combined$year <- as.numeric(dat_team_combined$year)
dat_team_combined$week <- as.numeric(dat_team_combined$week)
dat_team_combined <- dat_team_combined %>% arrange(year, week)
dat_team_combined <- get_team_ranks(temp_dat = dat_team_combined)

# ------------------------------------------------------------
source('functions.R')

# read in fantasy data
dat_fan_off <- read_csv('../data/player_fan_offense.csv')
dat_fan_def <- read_csv('../data/player_fan_defense.csv')

# curate fantasy data
dat_in_season_fan <- read.csv('../data/in_season_df_new.csv', stringsAsFactors = FALSE)
names(dat_in_season_fan) <- tolower(names(dat_in_season_fan))

# get defense and offense 
dat_in_season_fan_def <- dat_in_season_fan[dat_in_season_fan$draftkings == 'DST',]
dat_in_season_fan_off <- dat_in_season_fan[dat_in_season_fan$draftkings != 'DST',]
# 
# temp_def <- as.data.frame(cbind(names(dat_fan_def), names(dat_in_season_fan_def)))
# temp_off <- as.data.frame(cbind(names(dat_fan_off), names(dat_in_season_fan_off)))
# 
# write_csv(temp_def, '~/Desktop/temp_def.csv')
# write_csv(temp_off, '~/Desktop/temp_off.csv')

# read in dictionary
in_season_fan_off_dict <- read.csv('../data/in_season_fan_off_dict.csv')
in_season_fan_def_dict <- read.csv('../data/in_season_fan_def_dict.csv')

# Remove any NA in in_season_new
in_season_fan_off_dict <- in_season_fan_off_dict %>% filter(!is.na(in_season_new))
in_season_fan_def_dict <- in_season_fan_def_dict %>% filter(!is.na(in_season_new))

# get a vector of column names to keep
keep_cols_off <- as.character(in_season_fan_off_dict$in_season_new)
keep_cols_def <- as.character(in_season_fan_def_dict$in_season_new)

# remove gameid player_team_id and player_team
dat_in_season_fan_off$player_team_id <- dat_in_season_fan_off$game.id <- NULL
dat_in_season_fan_def$player_team_id <- dat_in_season_fan_def$game.id <- 
  dat_in_season_fan_def$player_team <- dat_in_season_fan_def$draftkings <- 
  dat_in_season_fan_def$fanduel <- NULL


# subet data by keep cols
names(dat_in_season_fan_off) <- keep_cols_off
names(dat_in_season_fan_def) <- keep_cols_def

rm(keep_cols_off, keep_cols_def, in_season_fan_def_dict, in_season_fan_off_dict, dat_in_season_fan)

# add year to in season data
dat_in_season_fan_off$year <- '2018'
dat_in_season_fan_def$year <- '2018'

# add year
dat_fan_off$year <- '2017'
dat_fan_def$year <- '2017'

# remove playoffs from dataset
dat_fan_off <- dat_fan_off[!grepl('Playoffs', dat_fan_off$dataset),]
dat_fan_def <- dat_fan_def[!grepl('Wild|Divis|Conf|Super', dat_fan_def$week),]

dat_fan_def$dataset <- NULL
dat_fan_off$dataset <- NULL
dat_in_season_fan_off$dataset <- NULL
dat_in_season_fan_def$dataset <- NULL

# convert to date type
dat_fan_off$date <- as.Date(dat_fan_off$date, format = '%m/%d/%Y')
dat_fan_def$date <- as.Date(dat_fan_def$date, format = '%m/%d/%Y')
dat_in_season_fan_off$date <- as.Date(dat_in_season_fan_off$date, format = '%d-%m-%Y')
dat_in_season_fan_def$date <- as.Date(dat_in_season_fan_def$date, format = '%d-%m-%Y')

# combine new data with old data
dat_fan_off <- rbind(dat_fan_off,
                     dat_in_season_fan_off)
rm(dat_in_season_fan_off)

dat_fan_def <- rbind(dat_fan_def,
                     dat_in_season_fan_def)
rm(dat_in_season_fan_def)
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
dk_offense <- match_player_names(dk_offense, 
                                 df_type = 'dk', 
                                 all_player_names = all_player_names)
fd_offense <- match_player_names(fd_offense, 
                                 df_type = 'fd', 
                                 all_player_names = all_player_names)
fd_offense_2016 <- match_player_names(fd_offense_2016, 
                                      df_type = 'fd', 
                                      all_player_names = all_player_names)

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

# take care of chargers as well
dat_fan_off$team <- ifelse(grepl('Chargers', dat_fan_off$team),'Chargers (SD, LA)', dat_fan_off$team)
dat_fan_def$team <- ifelse(grepl('Chargers', dat_fan_def$team),'Chargers (SD, LA)', dat_fan_def$team)

# opponenet 
dat_fan_off$opponent <- ifelse(grepl('Chargers', dat_fan_off$opponent),'Chargers (SD, LA)', dat_fan_off$opponent)
dat_fan_def$opponent <- ifelse(grepl('Chargers', dat_fan_def$opponent),'Chargers (SD, LA)', dat_fan_def$opponent)

# remove extra objects
rm(team_dict_def, team_dict_off)
rm(opp_dict_def, opp_dict_off)
rm(fd_dk_defense)

# moving forward: featrurize fantasy data, get weekly ranks for each team (and player?),
# get data for last week alone. 

# offense 
dat_fan_off <- featurize_fantasy_data(dat_fan_off, offense = TRUE)
dat_fan_def <- featurize_fantasy_data(dat_fan_def, offense = FALSE)

# remove draft_kings_position
dat_fan_off$draft_kings_position <- NA

# if draft kings points is zero, impute fan duel
dat_fan_off$fantasy_points <- ifelse(dat_fan_off$draft_kings_points == 0, 
                                     dat_fan_off$fan_duel_points,
                                     dat_fan_off$draft_kings_points)

dat_fan_def$fantasy_points <- ifelse(dat_fan_def$draft_kings_points == 0, 
                                     dat_fan_def$fan_duel_points,
                                     dat_fan_def$draft_kings_points)


##### -----------------------------------------

# # save all data
# 
# save team data
saveRDS(dat_team, '../data/cleaned_data/team_data.rda')
saveRDS(dat_team_combined, '../data/cleaned_data/team_data_combined.rda')


# save individual data
saveRDS(qb_all, '../data/cleaned_data/player_data_qb.rda')
saveRDS(rb_all, '../data/cleaned_data/player_data_rb.rda')
saveRDS(wr_all, '../data/cleaned_data/player_data_wr.rda')
saveRDS(te_all, '../data/cleaned_data/player_data_te.rda')
saveRDS(k_all, '../data/cleaned_data/player_data_k.rda')

# save fantasy data
saveRDS(dat_fan_off, '../data/cleaned_data/fantasy_offense.rda')
saveRDS(dat_fan_def, '../data/cleaned_data/fantasy_defense.rda')



