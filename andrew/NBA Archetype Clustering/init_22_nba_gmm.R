# PURPOSE: 2022 NBA Clustering using GMM

# Load libraries & Data ---------------------------------------------------

library(mclust)
library(vip)
library(tidyverse)


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



# Further Data Trimming ---------------------------------------------------


# Filter out by minutes played - 
nba_all_stats <- nba_all_stats %>%
  mutate(mpg = mp / g) %>%
  filter(mpg >= 12, g >= 10)


# Now select trimmed variables before clustering
### NOTE: Variables are only statistics aimed at determining stylistic tendencies 

nba_select_stats <- nba_all_stats %>%
  select(1:9, # Basic info
         11, 14, 20, 24, # Per 100 poss statistics
         36, 39:42,  # Advanced rate statistics (FT rate, Block%, etc)
         53, 55:59 # Shot location info (distance, proportion of shots from each distance range)
  )

nba_2022 <- nba_select_stats %>%
  filter(season == 2022)



# GMM Modeling ------------------------------------------------------------

set.seed(2001)
nba_mclust_2022 <- Mclust(dplyr::select(nba_2022, -c(1:9)))
summary(nba_mclust_2022)
write_rds(nba_mclust_2022, "models/nba_mclust_2022.rds")



# Define functions to make dealing with models easier ---------------------


nba_cluster_prob <- function(x, mclust_model) {
  
  
  nba_player_probs <- mclust_model$z
  colnames(nba_player_probs) <- 
    paste0('Cluster ', 1:ncol(mclust_model$z))
  
  nba_player_probs <- nba_player_probs %>%
    as_tibble() %>%
    mutate(player = 
             x$player) %>%
    pivot_longer(contains("Cluster"),
                 names_to = "cluster",
                 values_to = "prob")
  
  return(nba_player_probs)
  
}

nba_cluster_assignment <- function(x, mclust_model){
  
  model_stats <- x %>%
    mutate(cluster =
             mclust_model$classification,
           uncertainty =
             mclust_model$uncertainty)
  
  return(model_stats)
}



# Post modeling examination of clusters -----------------------------------

nba_2022 <- nba_2022 %>%
  mutate(pos = case_when(
    pos == "C-PF" ~ "C",
    pos == "PF-SF" ~ "PF",
    pos == "PG-SG" ~ "PG",
    pos == "SF-SG" ~ "SF",
    pos == "SG-PG" ~ "SG",
    pos == "SG-SF" ~ "SG",
    TRUE ~ pos
  ))

table("Clusters" = nba_mclust_2022$classification, "Positions" = nba_2022$pos)

nba_player_probs <- nba_cluster_prob(nba_2022, nba_mclust_2022)


nba_player_probs %>%
  ggplot(aes(prob)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ cluster, nrow = 2)

nba_cluster_assignment(nba_2022, nba_mclust_2022) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) %>%
  slice(1:5) %>%
  ggplot(aes(y = uncertainty, 
             x = reorder(player,
                         uncertainty))) +
  geom_point() +
  coord_flip() + 
  theme_bw() +
  facet_wrap(~ cluster, 
             scales = 'free_y', nrow = 3)


nba_cluster_assignment(nba_2022, nba_mclust_2022) %>%
  count(pos, cluster) %>% 
  print(n = 33)


nba_cluster_assignment(nba_2022, nba_mclust_2022) %>%
  ggplot() + 
  geom_histogram(aes(x = x3pa)) + 
  theme_bw() + 
  facet_wrap(~ cluster,
             scales = 'free_y', nrow = 3)





