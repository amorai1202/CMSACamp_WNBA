# PURPOSE: TRY NBA Mclust model on WNBA players


# Load libraries ----------------------------------------------------------

library(mclust)
library(tidyverse)


nba_mclust_2022 <- read_rds("models/nba_mclust_2022.rds")

wnba_clean <- read_csv("data/wnba_all_stats_clean.csv")

wnba_select_stats <- wnba_clean %>%
  select(1:7,  # Basic info
         9, 12, 18, 21, # Per 100 poss stats
         32, 34:37, # Advanced rate statistics (FT Rate, Block %, etc)
         46, 48:52, # Shooting tendency data
         )

wnba_clean_21 <- wnba_select_stats %>% 
  filter(season == 2021)


# Assigning probabilities and uncertainty to WNBA model results
pred_wnba_21 <- predict(nba_mclust_2022, newdata = select(wnba_clean_21, 8:22))

wnba_clean_21 %>%
  mutate(pred_class = pred_wnba_21$classification,
         prob1 = pred_wnba_21$z[,1],
         prob2 = pred_wnba_21$z[,2],
         prob3 = pred_wnba_21$z[,3],
         prob4 = pred_wnba_21$z[,4],
         prob5 = pred_wnba_21$z[,5]
         ) %>% 
  mutate(uncertainty = 1 - pmax(prob1, prob2, prob3, prob4, prob5))


# HOW TO DRAW NBA PLAYER COMPARSIONS
### Take Euclidean distance of the 5 cluster probs, determine closest NBA Player


# JUST USE 22/21 or train on more data?
### Could use all season because that can show career progression

