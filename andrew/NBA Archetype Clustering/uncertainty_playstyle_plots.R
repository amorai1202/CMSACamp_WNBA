# PURPOSE: Create uncertainty plots for NBA and WNBA clusters


library(tidyverse)

nba_playstyle_clusters <- read_csv("data/nba_playstyle_clusters.csv")
wnba_playstyle_clusters <- read_csv("data/wnba_playstyle_clusters.csv")




wnba_playstyle_clusters %>%
  rename(cluster = 'pred_class') %>%
  filter(season == 2021) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) %>%
  slice(1:3) %>%
  ggplot(aes(y = uncertainty,
             x = reorder(player,
                         uncertainty))) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ cluster,
             scales = 'free_y', nrow = 4) + 
  labs(y = "Uncertainty",
       x = "Player",
       title = "2021 WNBA players with highest uncertainty when clustering by playstyle variables")

nba_playstyle_clusters %>%
  rename(cluster = 'pred_class') %>%
  filter(season == 2022) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) %>%
  slice(1:3) %>%
  ggplot(aes(y = uncertainty,
             x = reorder(player,
                         uncertainty))) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ cluster,
             scales = 'free_y', nrow = 4) + 
  labs(y = "Uncertainty",
       x = "Player",
       title = "2022 NBA players with highest uncertainty when clustering by playstyle variables")

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

nba_all_stats %>%
  mutate(mpg = mp/g) %>%
  filter(mp > 100,
         season >= 2018, season < 2022)



