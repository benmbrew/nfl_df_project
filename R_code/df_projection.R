# this script will read in the nfl data we have and clean, combine, and explore
# source functions script to get custom functions 
source('functions.R')

# -----------------------------------------------------------
# read in player level data
dat_proj <- read_csv('../data/dfs_projections.csv')

# make lower case and replace space with '_'
names(dat_proj) <- tolower(names(dat_proj))
names(dat_proj) <- gsub(' ', '_', names(dat_proj), fixed = TRUE)
names(dat_proj) <- gsub('/', '_', names(dat_proj), fixed = TRUE)

