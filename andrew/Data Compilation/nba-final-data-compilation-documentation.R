# PURPOSE: Compile final cumulative NBA dataset from BBallRef


# Load in data ------------------------------------------------------------

library(ballr)
library(rvest)
library(tidyverse)


nba_pbp <- read_csv("data/nba_pbp.csv")
nba_shooting <- read_csv("data/nba_shooting.csv")
nba_per100_stats <- read_csv("data/nba_per100_stats.csv")
nba_adv_stats <- read_csv("data/nba_adv_stats.csv")
nba_total_stats <- read_csv("data/nba_total_stats.csv")


nba_total_stats <- nba_total_stats %>%
  rename(fg_tot = "fg",
         fga_tot = "fga",
         x3p_tot = "x3p",
         x3pa_tot = "x3pa",
         x2p_tot = "x2p",
         x2pa_tot = "x2pa",
         ft_tot = "ft",
         fta_tot = "fta",
         orb_tot = "orb",
         trb_tot = "trb",
         ast_tot = "ast",
         stl_tot = "stl",
         blk_tot = "blk",
         tov_tot = "tov",
         pf_tot = "pf",
         pts_tot = "pts"
  )



# Joining all the datasets together

nba_all_stats <- nba_per100_stats %>%
  left_join(nba_adv_stats) %>%
  left_join(nba_shooting) %>%
  left_join(nba_pbp) %>%
  left_join(nba_total_stats) %>% 
  arrange(-season) %>%  
  select(1:4, 32, 5:30, 33:107, 31) %>%
  select(-rk)

### DOCUMENTATION
# Columns 1-31 are per 100 stats (O/Drtg, 2pa/3pa, pts, reb, etc) + shooting percentages
# Columns 32-51 are advanced stats - TS%, PER, WS, vorp, BPM, etc
# Columns 52-72 are shootin tendencies - % of shots that are 2s, 3s, distance where they're taken, etc
# Columns 73-88 are play-by-play data: on off, turnover type, % of minutes at each estimated position
# Columns 89-105 are raw totals (2pm, 2pa, etc)

# Write to csv
# write_csv(nba_all_stats, "data/nba_all_stats.csv")
