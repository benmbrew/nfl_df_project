# this script will read in all data from read_in.R and combine them for modeling and save each dataset
# combine individual data with team data and fantasy data


# source functions scrip to get functions and libraries
source('functions.R')

# read in running back data
dat_team <- readRDS('../data/cleaned_data/team_data.csv')

# read individual data
qb_all <- readRDS('../data/cleaned_data/player_data_qb.csv')
rb_all <- readRDS('../data/cleaned_data/player_data_rb.csv')
wr_all <- readRDS('../data/cleaned_data/player_data_wr.csv')
te_all <- readRDS('../data/cleaned_data/player_data_te.csv')
k_all <- readRDS('../data/cleaned_data/player_data_k.csv')

# read fantasy data
dat_fan_off <- readRDS('../data/cleaned_data/fantasy_offense.csv')

# homogenize names of players "Robert Griffin" "EJ Manuel"
dat_fan_off$player <- gsub('E.J. Manuel', 'EJ Manuel', dat_fan_off$player)
dat_fan_off$player <- gsub(' III', '', dat_fan_off$player)

# combine each position to fantasy datat
qb_fan <- combine_position_to_fantasy(temp_dat = qb_all)
rb_fan <- combine_position_to_fantasy(temp_dat = rb_all)
wr_fan <- combine_position_to_fantasy(temp_dat = wr_all)
te_fan <- combine_position_to_fantasy(temp_dat = te_all)
k_fan <- combine_position_to_fantasy(temp_dat = k_all)

# combine fan with team 
qb_fan_team <- combine_fan_to_team(qb_fan)
rb_fan_team <- combine_fan_to_team(rb_fan)
wr_fan_team <- combine_fan_to_team(wr_fan)
te_fan_team <- combine_fan_to_team(te_fan)
k_fan_team <- combine_fan_to_team(k_fan)

# save player fanatay data
saveRDS(qb_fan, '../data/model_data/qb_fan_final.rda')
saveRDS(rb_fan, '../data/model_data/rb_fan_final.rda')
saveRDS(wr_fan, '../data/model_data/wr_fan_final.rda')
saveRDS(te_fan, '../data/model_data/te_fan_final.rda')
saveRDS(k_fan, '../data/model_data/k_fan_final.rda')


# save player fantasy team data
saveRDS(qb_fan_team, '../data/model_data/qb_fan_team_final.rda')
saveRDS(rb_fan_team, '../data/model_data/rb_fan_team_final.rda')
saveRDS(wr_fan_team, '../data/model_data/wr_fan_team_final.rda')
saveRDS(te_fan_team, '../data/model_data/te_fan_team_final.rda')
saveRDS(k_fan_team, '../data/model_data/k_fan_team_final.rda')

