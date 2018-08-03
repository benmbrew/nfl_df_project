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

# save data
saveRDS(qb_all, '../data/qb_all.rda')
saveRDS(rb_all, '../data/rb_all.rda')
saveRDS(wr_all, '../data/wr_all.rda')
saveRDS(te_all, '../data/te_all.rda')
saveRDS(k_all, '../data/k_all.rda')

# ------------------------------------------------------------
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

dat_team_2016 <- get_opposing_team_stats(dat_team_2016)
dat_team_2017 <- get_opposing_team_stats(dat_team_2017)

# combine data sets and save for joining and modelling
dat_team <- rbind(dat_team_2016,
                  dat_team_2017)

# get a vector of team names 
team_names <- unique(dat_team$team)

rm(dat_team_2016,
   dat_team_2017)

saveRDS(dat_team, '../data/mod_dat_team.rda')

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

# make column names lower case
names(dat_dk) <- tolower(names(dat_dk))
names(dat_fd) <- tolower(names(dat_fd))

# subset to offense and defense 
dk_offense <- dat_dk[!grepl('Def', dat_dk$pos),]
dk_defense <- dat_dk[grepl('Def', dat_dk$pos),]

fd_offense <- dat_dk[!grepl('Def', dat_dk$pos),]
fd_defense <- dat_fd[grepl('Def', dat_fd$pos),]

rm(dat_dk, dat_fd)

# apply the function to homogenize names of players across data sets
dk_offense <- match_player_names(dk_offense)
fd_offense <- match_player_names(fd_offense)

# write csv to creat team dictaionary by hand
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$name)), real_names = sort(team_names))), '../data/team.csv')

# read in team dictionary
team_dict <- read_csv('../data/team.csv')

# join team dictionary and dk_defense
dk_defense <- inner_join(dk_defense, team_dict, by = c('name' = 'old_names'))
fd_defense <- inner_join(fd_defense, team_dict, by = c('name' = 'old_names'))

# rearrange columns and rename
dk_defense <- dk_defense[, c('year', 'week', 'real_names', 'gid', 'pos', 'h/a',
                             'oppt', 'dk points', 'dk salary')]
fd_defense <- fd_defense[, c('year', 'week', 'real_names', 'gid', 'pos', 'h/a',
                             'oppt', 'fd points', 'fd salary')]

# write dictionary again for opponenets 
# write_csv(as.data.frame(cbind(old_names = sort(unique(dk_defense$oppt)), real_names = sort(team_names))), '../data/oppt.csv')

# read in opponent dictionary
oppt_dict <- read_csv('../data/oppt.csv')

# join oppt dictionary and dk_defense
dk_defense <- inner_join(dk_defense, oppt_dict, by = c('oppt' = 'old_oppt_name'))
fd_defense <- inner_join(fd_defense, oppt_dict, by = c('oppt' = 'old_oppt_name'))

# rearrange columns
dk_defense <- dk_defense[, c('year', 'week', 'real_names', 'gid', 'pos', 'h/a',
                             'real_oppt_name', 'dk points', 'dk salary')]
fd_defense <- fd_defense[, c('year', 'week', 'real_names', 'gid', 'pos', 'h/a',
                             'real_oppt_name', 'fd points', 'fd salary')]

# rename columns
names(dk_defense) <- c('year', 'week', 'team_name', 'game_id', 'position', ' venue', 'opp_name', 
                       'dk_points', 'dk_salary')
names(fd_defense) <- c('year', 'week', 'team_name', 'game_id', 'position', ' venue', 'opp_name', 
                       'fd_points', 'fd_salary')

# save data 
saveRDS(dk_offense, '../data/dk_offense.rda')
saveRDS(fd_offense, '../data/fd_offense.rda')

saveRDS(dk_defense, '../data/dk_defense.rda')
saveRDS(fd_defense, '../data/fd_defense.rda')



