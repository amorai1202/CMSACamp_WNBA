# PURPOSE: Compile final cumulative WNBA dataset from BBallRef


# Load in data ------------------------------------------------------------

library(ballr)
library(rvest)
library(tidyverse)

wnba_pbp <- read_csv("data/wnba_pbp.csv")
wnba_shooting <- read_csv("data/wnba_shooting.csv")
wnba_per100_stats <- read_csv("data/wnba_per100_stats.csv")
wnba_adv_stats <- read_csv("data/wnba_adv_stats.csv")
wnba_total_stats <- read_csv("data/wnba_total_stats.csv")

# Rename columns so that they don't get joined with per 100 columns
wnba_total_stats <- wnba_total_stats %>%
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


wnba_all_stats <- wnba_per100_stats %>%
  left_join(wnba_adv_stats) %>%
  left_join(wnba_shooting) %>%
  left_join(wnba_pbp) %>%
  left_join(wnba_total_stats) %>%
  arrange(-season) %>% 
  select(-c(6, 8, 66, 78)) %>% 
  select(1, 2, 28, 3:26, 29:90, 27)


#### DOCUMENTATION

# Columns 1-27 are per 100 stats (3pa, 2pa, etc) and shooting percentages
# Columns 28-45 are advanced stats (ast%, per, ts%, etc)
# Columns 46-62 are shooting tendencies - % of shots that are 2s, 3s, distance where they're taken, etc
# Columns 63-73 are play-by-play stats - on off, turnover type, and 1s, fouls drawn/committed
# Columns 74-89 are raw totals (useful for shooting tendencies as well)

# WRITE TO CSV
write_csv(wnba_all_stats, "data/wnba_all_stats.csv")

         