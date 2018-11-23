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
  # filter(counts > 7) %>%
  summarise(num_years = length(unique(year)),
            mean_dk_points = mean(draft_kings_points, na.rm = TRUE),
            mean_fd_points = mean(fan_duel_points, na.rm = TRUE),
            mean_fan_points = mean(fantasy_points, na.rm = TRUE)) %>%
  arrange(-mean_fan_points)

# remove any rows with only num years = 1
qb_group <- qb_group[qb_group$num_years > 1,]
qb_group <- qb_group[qb_group$mean_fan_points > 15,]

saveRDS(qb_group, '../data/model_data/best_qbs.rda')

# rb data
rb_data <- readRDS('../data/model_data/rb_data.rda')

# group by player and get top draft_kings_points
rb_group <- rb_data %>%
  group_by(player) %>%
  # filter(counts > 7) %>%
  summarise(num_years = length(unique(year)),
            mean_dk_points = mean(draft_kings_points, na.rm = TRUE),
            mean_fd_points = mean(fan_duel_points, na.rm = TRUE),
            mean_fan_points = mean(fantasy_points, na.rm = TRUE)) %>%
  arrange(-mean_fan_points)

# remove any rows with only num years = 1
rb_group <- rb_group[rb_group$num_years > 1,]
rb_group <- rb_group[rb_group$mean_fan_points > 10,]

saveRDS(rb_group, '../data/model_data/best_rbs.rda')

# wr data
wr_data <- readRDS('../data/model_data/wr_data.rda')

# group by player and get top draft_kings_points
wr_group <- wr_data %>%
  group_by(player) %>%
  # filter(counts > 7) %>%
  summarise(num_years = length(unique(year)),
            mean_dk_points = mean(draft_kings_points, na.rm = TRUE),
            mean_fd_points = mean(fan_duel_points, na.rm = TRUE),
            mean_fan_points = mean(fantasy_points, na.rm = TRUE)) %>%
  arrange(-mean_fan_points)

# remove any rows with only num years = 1
wr_group <- wr_group[wr_group$num_years > 1,]
wr_group <- wr_group[wr_group$mean_fan_points > 8,]

saveRDS(wr_group, '../data/model_data/best_wrs.rda')

# te data
te_data <- readRDS('../data/model_data/te_data.rda')

# group by player and get top draft_kings_points
te_group <- te_data %>%
  group_by(player) %>%
  # filter(counts > 7) %>%
  summarise(num_years = length(unique(year)),
            mean_dk_points = mean(draft_kings_points, na.rm = TRUE),
            mean_fd_points = mean(fan_duel_points, na.rm = TRUE),
            mean_fan_points = mean(fantasy_points, na.rm = TRUE)) %>%
  arrange(-mean_fan_points)

# remove any rows with only num years = 1
te_group <- te_group[te_group$num_years > 1,]
te_group <- te_group[te_group$mean_fan_points > 5,]

saveRDS(te_group, '../data/model_data/best_tes.rda')

# k data
k_data <- readRDS('../data/model_data/k_data.rda')

# group by player and get top draft_kings_points
k_group <- k_data %>%
  group_by(player) %>%
  # filter(counts > 7) %>%
  summarise(num_years = length(unique(year)),
            mean_dk_points = mean(draft_kings_points, na.rm = TRUE),
            mean_fd_points = mean(fan_duel_points, na.rm = TRUE),
            mean_fan_points = mean(fantasy_points, na.rm = TRUE)) %>%
  arrange(-mean_fan_points)

# remove any rows with only num years = 1
k_group <- k_group[k_group$num_years > 1,]
k_group <- k_group[k_group$mean_fan_points > 8,]

saveRDS(k_group, '../data/model_data/best_ks.rda')




