# PURPOSE: Compile final cumulative WNBA dataset from BBallRef


# Load in data ------------------------------------------------------------

library(ballr)
library(rvest)
library(tidyverse)

wnba_pbp <- read_csv("data/wnba_pbp.csv")
wnba_shooting <- read_csv("data/wnba_shooting.csv")
wnba_per100_stats <- read_csv("data/wnba_per100_stats.csv")
wnba_adv_stats <- read_csv("data/wnba_adv_stats.csv")

# Joining all the datasets together

wnba_all_stats <- wnba_per100_stats %>%
  left_join(wnba_adv_stats) %>%
  left_join(wnba_shooting) %>%
  left_join(wnba_pbp) %>%
  arrange(-season) %>% 
  select(-c(6, 8, 66)) %>%
  select(1, 2, 28, 3:26, 29:74, 27)


#### DOCUMENTATION

# Columns 1-27 are per 100 stats (3pa, 2pa, etc) and shooting percentages
# Columns 28-45 are advanced stats (ast%, per, ts%, etc)
# Columns 46-62 are shooting tendencies - % of shots that are 2s, 3s, distance where they're taken, etc
# Columns 63-73 are play-by-play stats - on off, turnover type, and 1s, fouls drawn/committed

# WRITE TO CSV
# write_csv(wnba_all_stats, "data/wnba_all_stats.csv")

         