# PURPOSE: Clustering WNBA players using GMM

# Load and filter data -----------------------------------------------------------

library(tidyverse)
wnba_all_stats_raw <- read_csv("data/wnba_all_stats.csv") #1997-2022

wnba_all_stats <- wnba_all_stats_raw %>% 
  select(-contains("link")) %>%  #remove link column
  mutate(mpg = mp/g) #add mpg column

#Eliminating players that played less than 10 mpg and 5 games 

wnba_ecdf <- wnba_all_stats %>% #justification
  ggplot(aes(x = mpg)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "mpg",
       y = "Proportion of WNBA players")

wnba_all_stats <- wnba_all_stats %>% 
  filter(mpg > 10, g > 5, !(tm == "TOT"))  #Removing TOT players (errors)


#### HOW TO DEAL WITH TOTALS

tot_wnba <- wnba_all_stats %>% 
  filter(tm == "TOT")


#Which players played for more than 1 team during the season
tbl <- table(wnba_all_stats$player)
tbl[tbl>1]

#Remove rows for the 2 players that played less games with their 2nd team 

wnba_2021_stats <- wnba_2021_stats %>% 
  slice(-c(122, 115)) #120 players left



# Try mclust on all variables ---------------------------------------------

library(mclust)
library(vip)

wnba_numeric <- wnba_all_stats %>% 
  select_if(is.numeric)

set.seed(2001)
init_wnba_mclust <- Mclust(dplyr::select(wnba_numeric,))

table("Clusters" = init_wnba_mclust$classification, "Positions" = wnba_numeric$pos)


summary(init_wnba_mclust)


wnba_player_probs <- init_nba_mclust$z








