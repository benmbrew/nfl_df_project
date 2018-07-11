# nfl_df_project
This repo will store code from our project to predict daily fantasy outcomes for nfl data.

Currently we have 2 seasons worth of player level and team level data and 7 total season of fan duel and draft kings
fantasy data. 

## Python code
.ipynb scripts to scrape fantasy data that I hadn't already purchased. Able to get Fan duel data from 2011-2016 and Draft Kings data from 2014-2017. In addition I purchased 2017 fanduel and draft kings data from bigdataball.com.

## R code

### read_in.R

#### Step 1: Read in all data, clean, restructure, and recode variables. Stack (rbind) similar datases, player level 2016 and 2017, team level 2016 and 2017, and fantasy data (2011-2017).

#### Step 2: Featurize team and player level data so that each row displays previous historical stats, either cumulative or not. Combine featurized data with fantasy level data so we can model an outcome.

#### Step 3: Build models to predict outcomes for each position (fantasy points, passing atts, rushing atts, etc)

