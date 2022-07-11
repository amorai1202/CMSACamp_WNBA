# PURPOSE: Compile final cumulative NBA dataset from BBallRef


# Load in data ------------------------------------------------------------

library(ballr)
library(rvest)
library(tidyverse)


nba_pbp <- read_csv("data/nba_pbp.csv")
nba_shooting <- read_csv("data/nba_shooting.csv")
nba_per100_stats <- read_csv("data/nba_per100_stats.csv")
nba_adv_stats <- read_csv("data/nba_adv_stats.csv")


# Joining all the datasets together

nba_all_stats <- nba_per100_stats %>%
  left_join(nba_adv_stats) %>%
  left_join(nba_shooting) %>%
  left_join(nba_pbp) %>% 
  arrange(-season) %>% 
  select(1:4, 32, 5:30, 33:90, 31) %>%
  select(-rk)

### DOCUMENTATION
# Columns 1-31 are per 100 stats (O/Drtg, 2pa/3pa, pts, reb, etc) + shooting percentages
# Columns 32-51 are advanced stats - TS%, PER, WS, vorp, BPM, etc
# Columns 52-72 are shootin tendencies - % of shots that are 2s, 3s, distance where they're taken, etc
# Columns 73-88 are play-by-play data: on off, turnover type, % of minutes at each estimated position

# Write to csv
# write_csv(nba_all_stats, "data/nba_all_stats.csv")
