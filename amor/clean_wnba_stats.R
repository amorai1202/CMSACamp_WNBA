# PURPOSE: Clean WNBA players
## Error in TOT calculation - for players who played more than 1 team between 2018-2021, take row with most games played

library(tidyverse)
wnba_all_stats_raw <- read_csv("data/wnba_all_stats.csv") #1997-2022

# 2018-2021 seasons
wnba_seasons <- wnba_all_stats_raw %>% 
  select(-contains("link")) %>%  #remove link column
  mutate(mpg = mp/g) %>%  #add mpg column
  filter(grepl("2018|2019|2020|2021", season)) 

#ECDF plot of mpg:

wnba_ecdf <- wnba_seasons %>%  
  ggplot(aes(x = mpg)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Minutes per Game",
       y = "Proportion of WNBA players")

#Eliminating players that played less than 10 mpg and 5 games each season
## Justification: removing players who've played an insignificant amount of time during the season

wnba_all_stats_clean <- wnba_seasons %>% 
  filter(tm != "TOT") %>%  #remove TOT rows
  group_by(player, season) %>%
  filter(mp == max(mp)) %>% 
#if a player played with multiple teams over the course of a season, kept row with most mp 
  filter(mpg > 10, g > 5)

# WRITE CSV
write_csv(wnba_all_stats_clean, "data/wnba_all_stats_clean2.csv")



