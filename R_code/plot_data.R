# this script will be used to plot nfl data for blog

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

# load all data

# team level 
team <- readRDS('../data/team_plot_data.rda')

# player level
player <- readRDS('../data/player_plot_data.rda')

# fantasy level
fan_off <- readRDS('../data/fan_off_plot_data.rda')
fan_def <- readRDS('../data/fan_def_plot_data.rda')

fan_off$year <- as.numeric(fan_off$year)
fan_off$week <- as.numeric(fan_off$week)
fan_off$fan_duel_points <- as.numeric(fan_off$fan_duel_points)
fan_off$fan_duel_salary <- as.numeric(fan_off$fan_duel_salary)


# plot fantasy data for each top player over time
# first arrage by year and week
fan_off <- fan_off[with(fan_off, order(year, week)), ]

# create colum to represent time period of year and week (mimic date)
fan_off$num <- paste0(fan_off$year, fan_off$week)
fan_off$num <- as.numeric(fan_off$num)

# subset by points
fan_off <- fan_off[fan_off$fan_duel_points > 8,]




temp_dat = fan_off
this_position = 'QB'
this_name = 'Cam Newton'
plot_type = 'scatter'
roll_avg = TRUE
# function for plotting
scatter_bar_player <- function(temp_dat, 
                               this_name, 
                               plot_type,
                               fit_type,
                               y_lab,
                               roll_avg) {
  # curate data
  temp_dat <- temp_dat[temp_dat$player == this_name,]
  temp_dat <- temp_dat[with(temp_dat, order(year, week)),]
  temp_dat <- temp_dat[,!grepl('draft_kings', names(temp_dat))]
  temp_dat <- temp_dat[complete.cases(temp_dat),]
  
  
  # now make x axis 
  temp_dat$x_axis <- seq(1, nrow(temp_dat), 1)
  
  # get earliest year object
  min_year <-  min(unique(temp_dat$year))
  
  # get ylim and xlim
  y_lim <- c(min(temp_dat$fan_duel_points), max(temp_dat$fan_duel_points))

  
  
  
  if(roll_avg){
    temp_dat$fan_duel_points <- movavg(temp_dat$fan_duel_points, n = 10, type = 's')
  }
  
  if(plot_type == 'scatter'){
    plot <- ggplot(temp_dat, aes(x_axis, fan_duel_points)) +
      geom_point(size = 1,
                 alpha = 0.7,
                 color = 'black') + 
      # xlim(c(0, 100)) +
      ylim(c(y_lim)) +
      geom_smooth(method = fit_type,
                  color = 'black') +
      labs(title = this_name,
           x = paste0('Weeks since ', min_year),
           y = y_lab,
           subtitle = 'Line represents OLS line of best fit') +
      theme_bw()
    
    return(plot)
  }
  
  if(plot_type == 'bar')
    plot <- ggplot(temp_dat, aes(x_axis, fan_duel_points)) +
    geom_bar(stat = 'identity',
             alpha = 0.7,
             fill = 'black') + 
    geom_smooth(method = fit_type,
                color = 'black') +
    # xlim(c(0, 100)) +
    ylim(c(y_lim)) +
    labs(title = this_name,
         x = 'Weeks since 2011',
         y = y_lab,
         subtitle = 'Line represents OLS line of best fit') +
    theme_bw()
  
  return(plot)
  
  
}

scatter_bar_player(temp_dat = fan_off,
                   this_name = 'Camaron Newton', 
                   plot_type = 'scatter',
                   fit_type = 'loess',
                   y_lab = 'Moving average of fantasy points',
                   roll_avg = FALSE)

temp_off <- fan_off[fan_off$fan_duel_salary > 10,]
temp_off <- temp_off[complete.cases(temp_off),]
# plot salary against points
ggplot(temp_off, aes(fan_duel_salary, fan_duel_points)) +
  geom_point(size = 1, 
             alpha = 0.8,
             color = 'black') +
  geom_smooth(method= 'lm',
              se = FALSE) +
   geom_smooth(method = 'loess',
               se = FALSE) +
  labs(x = 'Salary',
       y = 'Points')
cor(temp_off$fan_duel_salary, temp_off$fan_duel_points)

# standarize salary and points and get difference 
temp_off$points_norm <- scale(temp_off$fan_duel_points, center = TRUE, scale = TRUE)
temp_off$salary_norm <- scale(temp_off$fan_duel_salary, center = TRUE, scale = TRUE)
temp_off$points_log <- log10(temp_off$fan_duel_points)
temp_off$salary_log <- log10(temp_off$fan_duel_salary)

# compare historgrams
hist(temp_off$points_norm)
hist(temp_off$salary_norm)
hist(temp_off$points_log)
hist(temp_off$salary_log)

# get differencs with these variables
temp_off$points_norm_diff <- temp_off$points_norm - temp_off$salary_norm
temp_off$points_log_diff <- temp_off$points_log - temp_off$salary_log

hist(temp_off$points_norm_diff)
hist(temp_off$points_log_diff)

# plot normalized difference against time
