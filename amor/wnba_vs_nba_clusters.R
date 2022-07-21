# PURPOSE: Compare WNBA vs NBA clusters on variables selected by PCA

# Load both sets of data --------------------------------------------------

library(tidyverse)
wnba_all_stats <- read_csv("data/wnba_all_stats_clean.csv")

nba_all_stats_raw <- read_csv("data/nba_all_stats.csv")

tot_players <- nba_all_stats_raw %>% filter(tm == "TOT")

tot_players <- tot_players %>%
  unite("player_season", c(player, season), remove = FALSE)

nba_all_stats_raw <- nba_all_stats_raw %>% 
  unite("player_season", c(player, season), remove = FALSE)


nba_all_stats <- nba_all_stats_raw %>%
  filter(!(player_season %in% tot_players$player_season)) %>% 
  bind_rows(tot_players)

# Remove some weird outliers who didn't have a TOT column
nba_all_stats <- nba_all_stats %>%
  filter(!(player %in% c("Chris Johnson", "Marcus Williams", "Tony Mitchell")))

# Filter out by minutes played - 100 was around 10-15% of the data
nba_all_stats <- nba_all_stats %>%
  filter(mp >= 100)


# Variable selection & standardization ---------------------------------------

library(mclust)
library(vip)

set.seed(2001)

wnba_select_stats <- wnba_all_stats %>% 
  dplyr::select(shoot_fouls_drawn, x3p, 
                distance, trb, orb, ows, oncourt, ws_40, 
                ortg, ft, fg, pts, x3par, shoot_fouls_committed, 
                fga, ftr, pf, tov, drtg, stl, per, blk, ast, on_off) 

wnba_std <- wnba_select_stats %>%
  mutate(across(.cols = (1:24), .fns = scale)) %>%
  mutate(across(.cols = (1:24), .fns = as.numeric))

nba_select_stats <- nba_all_stats %>%
  dplyr::select(x3p, distance, trb, orb, ows, 
                ortg, ft, fg, pts, x3par,
                fga, ftr, pf, tov, drtg, stl, per, blk, ast)  

#no ws_40 variable, shoot_fouls_drawn, oncourt, shoot_fouls_committed, on_off are all NA's

nba_std <- nba_select_stats %>%
  mutate(across(.cols = (1:19), .fns = scale)) %>%
  mutate(across(.cols = (1:19), .fns = as.numeric)) %>% 
  view()


# Clustering --------------------------------------------------------------

init_wnba_mclust <- Mclust(wnba_std)
init_nba_mclust <- Mclust(nba_std)

write_rds(init_nba_mclust, "models/init_nba_mclust.rds")

wnba_cluster <- wnba_all_stats %>%
  mutate(cluster =
           init_wnba_mclust$classification,
         uncertainty =
           init_wnba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) 

nba_cluster <- wnba_all_stats %>%
  mutate(cluster =
           init_nba_mclust$classification,
         uncertainty =
           init_nba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty))


# Compare clusters --------------------------------------------------------

##Which stats to compare?


