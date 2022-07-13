# PURPOSE: Initial EDA on WNBA season


# Load the data -----------------------------------------------------------

library(tidyverse)
library(wehoop)
library(devtools)
install_github("rtelmore/ballr")
library(ballr)

nba_adv_stats <- read_csv("data/nba_adv_stats.csv")
wnba_adv_stats <- read_csv("data/wnba_adv_stats.csv")
nba_shooting <- read_csv("data/nba_shooting.csv")
wnba_shooting <- read_csv("data/wnba_shooting.csv") #only from 2018
nba_per100_stats <- read_csv("data/nba_per100_stats.csv")
wnba_per100_stats <- read_csv("data/wnba_per100_stats.csv")


nba_all_stats <- read_csv("data/nba_all_stats.csv")
nba_pbp <- read_csv("data/nba_pbp.csv")
  
wnba_all_stats <- read_csv("data/wnba_all_stats.csv") #1997-2022
wnba_pbp <- read_csv("data/wnba_pbp.csv") #2018-2022

# Explore WNBA datasets & identify outliers ---------------------------------------

head(wnba_adv_stats)
head(wnba_per100_stats)

OUTLIERS!!! #HOW DO WE DEAL WITH OUTLIERS
 - Emma Cannon
 - Shatori Walker-Kimbrough
 - Moriah Jefferson

# any(is.na(wnba_all_stats))
# any(is.na(wnba_pbp))

# Distribution of Total Minutes Played in a the 2021 Season

wnba_all_stats %>% 
  filter(season == "2021") %>% 
  ggplot(aes(x=mp)) + 
  geom_histogram(aes(y=..density..), position='identity', bins = 20) + 
  geom_density(alpha=0.2, fill="red") + # generate kernel density estimate
  labs(y = "Density", x = "Total Minutes Played", 
       title = "2021 Season - Minutes Played") + 
  theme_bw()


# Trends of per100 points across seasons ----------------------------------

## WNBA

wnba_avg_pts <- wnba_per100_stats %>% 
  filter(pts < 50) %>%  #what is a reasonable cutoff?
  group_by(season) %>% 
  summarize(mean = mean(pts)) %>% 
  ungroup() %>% 
  mutate(avg_pts = mean) 

wnba_avg_pts %>% 
  ggplot(aes(x = season, y = avg_pts)) +
  geom_point(alpha = 0.5) +
  theme_bw()


## NBA

nba_avg_pts <- nba_per100_stats %>% 
  filter(pts < 50) %>%  
  group_by(season) %>% 
  summarize(mean = mean(pts)) %>% 
  ungroup() %>% 
  mutate(avg_pts = mean) 

nba_avg_pts %>% 
  ggplot(aes(x = season, y = avg_pts)) +
  geom_point(alpha = 0.5) +
  theme_bw()

## Compare

ggplot() +
  geom_point(data = wnba_avg_pts, aes(x = season, y = avg_pts),
             col = "orange", alpha = 0.75, size = 3) +
  geom_point(data = nba_avg_pts, aes(x = season, y = avg_pts),
             col = "blue", alpha = 0.75, size = 3) +
  theme_bw()






