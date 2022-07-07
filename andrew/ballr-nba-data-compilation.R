# PURPOSE: Creating cumulative NBA datasets with ballr Package



# Load in libraries -------------------------------------------------------

library(ballr)
library(tidyverse)



# Create Advanced Stats Dataset 1997-2022 ---------------------------------

# ballr::NBAPerGameAdvStatistics(season = 2022) #This is way easier


nba_adv_stats <- tibble()
for (i in 1997:2022){
  temp <- ballr::NBAPerGameAdvStatistics(season = i) %>%
    mutate(season = i)
  nba_adv_stats <- bind_rows(nba_adv_stats, temp)
}

# Remove filler columns
nba_adv_stats <- nba_adv_stats %>%
  dplyr::select(-c(x, x_2, rk))


# Write adv_stats to csv
# write_csv(nba_adv_stats, "data/nba_adv_stats.csv")


# Create per 100 poss Dataset 1997-2022 -----------------------------------

ballr::NBAPerGameStatisticsPer100Poss(season = 2018)


nba_per100_stats <- tibble()
for (i in 1997:2022){
  temp <- ballr::NBAPerGameStatisticsPer100Poss(season = i) %>%
    mutate(season = i)
  nba_per100_stats <- bind_rows(nba_per100_stats, temp)
}

# Remove filler columns
nba_per100_stats <- nba_per100_stats %>%
  dplyr::select(-c(x, rk))

# Write csv
# write_csv(nba_per100_stats, "data/nba_per100_stats.csv")

