# PURPOSE: Create uncertainty plots for NBA and WNBA clusters


library(tidyverse)

#########--------------- PERFORMANCE ARCHETYPES

wnba_cluster <- read_csv("data/wnba_archetypes.csv")
nba_cluster <- read_csv("data/nba_archetypes.csv")

wnba_cluster %>%
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
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  facet_wrap(~ cluster,
             scales = 'free_y', nrow = 4) + 
  labs(y = "Uncertainty",
       x = "Player",
       title = "2021 WNBA players with highest uncertainty when clustering by performance variables")

nba_cluster %>%
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
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  facet_wrap(~ cluster,
             scales = 'free_y', nrow = 4) + 
  labs(y = "Uncertainty",
       x = "Player",
       title = "2021 NBA players with highest uncertainty when clustering by perforamance variables")





#########--------------- PLAYSTYLE

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
