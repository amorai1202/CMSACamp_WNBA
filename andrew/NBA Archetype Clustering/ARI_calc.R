# PURPOSE: Run Adjusted Rand Index 



# Load in libraries -------------------------------------------------------

library(mclust)
library(tidyverse)

wnba_results_clusters <- read_csv("data/wnba_archetypes.csv")
wnba_playstyle_clusters <- read_csv("data/wnba_playstyle_clusters.csv")

nba_results_clusters <- read_csv("data/nba_archetypes.csv")
nba_playstyle_clusters <- read_csv("data/nba_playstyle_clusters.csv")


# Try Adjusted Rand Index -------------------------------------------------

wnba_results_clusters <- wnba_results_clusters %>% 
  mutate(cluster_num = case_when(
    cluster == "Facilitors/Shooters" ~ 1,
    cluster == "Primary Scorers/Initiators" ~ 2,
    cluster == "Reserves" ~ 3,
    cluster == "Shooting Threats" ~ 4,
    cluster == "Traditional Bigs" ~ 5
  ))

wnba_playstyle_clusters$pred_class
wnba_results_clusters$cluster_num



adjustedRandIndex(wnba_playstyle_clusters$pred_class,
                  wnba_results_clusters$cluster_num)




# NBA Rand Index ----------------------------------------------------------

nba_results_clusters <- nba_results_clusters %>% 
  inner_join(nba_playstyle_clusters, by = c("player", "season", "tm")) %>% 
  select(player_season.x, cluster, uncertainty.x, player, season, # nba results based archetype
         pred_class, prob1:prob7, player_season.y, uncertainty.y, # playstyle
         ) %>%
  mutate(cluster_num = case_when(
    cluster == "Facilitors/Shooters" ~ 1,
    cluster == "Primary Scorers/Initiators" ~ 2,
    cluster == "Reserves" ~ 3,
    cluster == "Roleplayers" ~ 4,
    cluster == "Traditional Bigs" ~ 5
  ))

nba_results_clusters$cluster_num
nba_results_clusters$pred_class


adjustedRandIndex(nba_results_clusters$cluster_num,
                  nba_results_clusters$pred_class)


