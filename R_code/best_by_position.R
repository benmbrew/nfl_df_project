# this scrip will read in all player data and take the top players from each position group to be used as 
# features in the model. 
library(tidyverse)
library(dplyr)
library(reshape2)

# qb data
qb_data <- readRDS('../data/model_data/qb_data.rda')

# group by player and get top draft_kings_points
qb_group <- qb_data %>%
  group_by(player) %>%
  filter(counts > 7) %>%
  summarise(mean_dk_points = mean(draft_kings_points, na.rm = TRUE))

saveRDS(qb_group, '../data/model_data/best_qbs.rda')

# rb data
rb_data <- readRDS('../data/model_data/rb_data.rda')

# group by player and get top draft_kings_points
rb_group <- rb_data %>%
  group_by(player) %>%
  filter(counts > 7) %>%
  summarise(mean_dk_points = mean(draft_kings_points, na.rm = TRUE))

saveRDS(rb_group, '../data/model_data/best_rbs.rda')

# wr data
wr_data <- readRDS('../data/model_data/wr_data.rda')

# te data
te_data <- readRDS('../data/model_data/te_data.rda')

# k data
k_data <- readRDS('../data/model_data/k_data.rda')



