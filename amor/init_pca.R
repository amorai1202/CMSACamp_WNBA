# PURPOSE: Explore PCA - 2021 WNBA Season

# Load and filter data -----------------------------------------------------------

library(tidyverse)
library(wehoop)
library(devtools)

wnba_all_stats <- read_csv("data/wnba_all_stats.csv") #1997-2022
wnba_pbp <- read_csv("data/wnba_pbp.csv") #2018-2022

# Start by focusing on 2021 season
wnba_2021_stats <- wnba_all_stats %>% 
  mutate(mpg = mp/g) %>% 
  filter(season == "2021") 

wnba_ecdf <- wnba_2021_stats %>%
  ggplot(aes(x = mpg)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "mpg",
       y = "Proportion of WNBA players")
    
#Eliminating players that played less than 10 mpg and 5 games during the 2021 season
#Removing TOT

wnba_2021_stats <- wnba_2021_stats %>% 
  filter(mpg > 10, g > 5, !(tm == "TOT"))   #183 -> 136 -> 131 

#Which players played for more than 1 team during the season
tbl <- table(wnba_2021_stats$player)
tbl[tbl>1]

#CREATE TOTAL COLUMM


  

names(which(colSums(is.na(wnba_2021_stats)) > 0))
#HOW DO WE DEAL WITH NA'S





# PCA ---------------------------------------------------------------------

wnba_numeric <- wnba_2021_stats %>% 
  select_if(is.numeric)

model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -season, -g, -mp, -gs, -mpg, -off_fouls_drawn))

pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)

#is.infinite

summary(pca_wnba)

