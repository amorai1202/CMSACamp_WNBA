# PURPOSE: Use GMMs to cluster NBA players on a stylistic basis, then predict with WNBA


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

# Filter out by minutes played - 100 was around 10-15% of the data
nba_all_stats <- nba_all_stats %>%
  filter(mp >= 100)

nba_all_stats %>%
  ggplot(aes(x = mp)) + 
  stat_ecdf()


# Now select trimmed variables before clustering
### NOTE: Variables are only statistics aimed at determining stylistic tendencies 

nba_select_stats <- nba_all_stats %>%
  select(1:9, # Basic info
         11, 14, 20, 24, # Per 100 poss statistics
         36, 39:42,  # Advanced rate statistics (FT rate, Block%, etc)
         53, 55:59 # Shot location info (distance, proportion of shots from each distance range)
  )

set.seed(2001)
init_nba_mclust <- Mclust(dplyr::select(nba_select_stats, -c(1:9)))

# write_rds(init_nba_mclust, "models/init_nba_mclust.rds")

table("Clusters" = init_nba_mclust$classification, "Positions" = nba_select_stats$pos)


summary(init_nba_mclust)


nba_player_probs <- init_nba_mclust$z

colnames(nba_player_probs) <- paste0("Cluster ", 1:9)

nba_player_probs <- nba_player_probs %>%
  as_tibble() %>%
  mutate(player = 
           nba_select_stats$player, 
         season = nba_select_stats$season) %>%
  pivot_longer(contains("Cluster"),
               names_to = "cluster",
               values_to = "prob")

  
nba_player_probs %>%
  ggplot(aes(prob)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ cluster, nrow = 5)

nba_select_stats %>%
  mutate(cluster =
           init_nba_mclust$classification,
         uncertainty =
           init_nba_mclust$uncertainty) %>%
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
