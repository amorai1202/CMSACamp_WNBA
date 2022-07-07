# PURPOSE: Initial Exploration of wehoop and ballr package




library(wehoop)
library(tidyverse)

# Trying functions that don't seem to exist
try(espn_wnba_player_stats(athlete_id = 2529130, year = 2022))
try(espn_wnba_team_stats(team_id = "0", year = 2020))



# Messing around with wehoop ----------------------------------------------

# All player IDs
all_players <- wehoop::wnba_commonallplayers()

# Hustle stats leaders
wehoop::wnba_leaguehustlestatsplayerleaders()

wnba_commonplayerinfo()

wnba_leaguedashplayerstats(season = c("2022"), stat)[[1]] 



wnba_playercareerstats()
wnba_playerdashboardbyyearoveryear()


wnba_homepagev2(stat_type = "Advanced", player_or_team = "Player")

wnba_homepageleaders(player_or_team = "Player")



# Trying ballr ------------------------------------------------------------

library(ballr)


# ballr::NBAPerGameAdvStatistics(season = 2022) #This is way easier


nba_adv_stats <- tibble()
for (i in 1997:2022){
  temp <- ballr::NBAPerGameAdvStatistics(season = i) %>%
    mutate(season = i)
  nba_adv_stats <- bind_rows(nba_adv_stats, temp)
}
