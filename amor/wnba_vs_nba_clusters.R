# PURPOSE: Compare WNBA vs NBA clusters on variables selected by PCA

# Load both sets of data --------------------------------------------------

library(tidyverse)
wnba_all_stats <- read_csv("data/wnba_all_stats_clean.csv")

wnba_all_stats <- wnba_all_stats %>% 
  mutate(player_season = paste(player, season, sep = "_")) %>% 
  select(player_season, everything())

wnba_all_stats <- wnba_all_stats %>%
  mutate(pos = case_when(
    pos == "C-F" ~ "C",
    pos == "F-C" ~ "F",
    pos == "F-G" ~ "F",
    pos == "G-F" ~ "G",
    TRUE ~ pos
  ))

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
  filter(mp >= 100) %>% 
  mutate(pos = case_when(
    pos == "C-PF" ~ "C",
    pos == "PF-C" ~ "PF",
    pos == "SF-C" ~ "SF",
    pos == "SG-PF" ~ "SG",
    pos == "PF-SF" ~ "PF",
    pos == "PG-SG" ~ "PG",
    pos == "SF-SG" ~ "SF",
    pos == "SG-PG" ~ "SG",
    pos == "SG-SF" ~ "SG",
    pos == "SF-PF" ~ "SF",
    pos == "PG-SF" ~ "PG",
    TRUE ~ pos
  ))

# Gather data from 2018-2021 seasons

nba_all_stats <- nba_all_stats %>% 
  filter(grepl("2018|2019|2020|2021", season)) 


# Variable selection & standardization ---------------------------------------

library(mclust)
library(vip)

set.seed(2001)

wnba_select_stats <- wnba_all_stats %>% 
  dplyr::select(x3p, distance, trb, orb, ows, 
                ortg, ft, fg, pts, fga, pf, tov, drtg, stl, per, blk, ast) 


wnba_std <- wnba_select_stats %>%
  mutate(across(.cols = (1:24), .fns = scale)) %>%
  mutate(across(.cols = (1:24), .fns = as.numeric))

nba_select_stats <- nba_all_stats %>%
  dplyr::select(x3p, distance, trb, orb, ows, 
                ortg, ft, fg, pts,
                fga, pf, tov, drtg, stl, per, blk, ast)  


#no ws_40 variable, shoot_fouls_drawn, oncourt, shoot_fouls_committed, on_off are all NA's

nba_std <- nba_select_stats %>%
  mutate(across(.cols = (1:19), .fns = scale)) %>%
  mutate(across(.cols = (1:19), .fns = as.numeric)) 


# Clustering --------------------------------------------------------------

init_wnba_mclust <- Mclust(wnba_std)
init_nba_mclust <- Mclust(nba_std)
write_rds(init_wnba_mclust, "models/init_wnba_mclust.rds")
write_rds(init_nba_mclust, "models/init_nba_mclust.rds")

init_wnba_mclust <- readRDS("models/init_wnba_mclust.rds")
init_nba_mclust <- readRDS("models/init_nba_mclust.rds")


wnba_cluster <- wnba_all_stats %>%
  mutate(cluster =
           init_wnba_mclust$classification,
         uncertainty =
           init_wnba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(uncertainty)

nba_cluster <- nba_all_stats %>%
  mutate(cluster =
           init_nba_mclust$classification,
         uncertainty =
           init_nba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(uncertainty)

wnba_cluster <- wnba_cluster %>% 
  select(player_season, cluster, uncertainty, everything())

nba_cluster <- nba_cluster %>% 
  select(player_season, cluster, uncertainty, everything())

#view(wnba_cluster)
#view(nba_cluster)

#Position-Cluster tables
table("Clusters" = init_wnba_mclust$classification, "Positions" = wnba_all_stats$pos)
wnba_player_probs <- init_wnba_mclust$z
colnames(wnba_player_probs) <- paste0("Cluster ", 1:5)

table("Clusters" = init_nba_mclust$classification, "Positions" = nba_all_stats$pos)
nba_player_probs <- init_nba_mclust$z
colnames(nba_player_probs) <- paste0("Cluster ", 1:9)


#Cluster probabilities
wnba_player_probs <- wnba_player_probs %>%
  as_tibble() %>%
  mutate(player = 
           wnba_all_stats$player, 
         season = wnba_all_stats$season) %>%
  pivot_longer(contains("Cluster"),
               names_to = "cluster",
               values_to = "prob")

nba_player_probs <- nba_player_probs %>%
  as_tibble() %>%
  mutate(player = 
           nba_all_stats$player, 
         season = nba_all_stats$season) %>%
  pivot_longer(contains("Cluster"),
               names_to = "cluster",
               values_to = "prob")

#Uncertainty plots
wnba_uncertainty <- wnba_all_stats %>%
  mutate(cluster =
           init_wnba_mclust$classification,
         uncertainty =
           init_wnba_mclust$uncertainty) %>%
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


nba_uncertainty <- nba_all_stats %>%
  mutate(cluster =
           init_nba_mclust$classification,
         uncertainty =
           init_nba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) %>%
  slice(1:9) %>%
  ggplot(aes(y = uncertainty, 
             x = reorder(player,
                         uncertainty))) +
  geom_point() +
  coord_flip() + 
  theme_bw() +
  facet_wrap(~ cluster, 
             scales = 'free_y', nrow = 3)


# Compare clusters --------------------------------------------------------

## Position

wnba_pos <- wnba_cluster %>%
  ggplot(aes(x = pos)) + 
  geom_histogram(stat = "count") +
  theme_bw() +
  labs(x = "Position", title = "Clusters by Position") +
  facet_grid(cluster ~.)

nba_pos <- nba_cluster %>%
  ggplot(aes(x = pos)) + 
  geom_histogram(stat = "count") +
  theme_bw() +
  labs(x = "Position", title = "Clusters by Position") +
  facet_grid(cluster ~.)

library(patchwork)
wnba_pos + nba_pos

## Distance

wnba_dist <- wnba_cluster %>%
  ggplot(aes(x = distance, group = cluster, fill = cluster)) + 
  geom_density(adjust=1.5, alpha=.4) +
  labs(x = "Average Shot Distance", title = "WNBA Clusters by Distance") +
  theme_bw() +
  facet_wrap(~cluster) 

nba_dist <- nba_cluster %>%
  ggplot(aes(x = distance, group = cluster, fill = cluster)) + 
  geom_density(adjust=1.5, alpha=.4) +
  labs(x = "Average Shot Distance", title = "NBA Clusters by Distance") +
  theme_bw() +
  facet_wrap(~cluster) 

wnba_dist + nba_dist

## Rebounds

wnba_rbs <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = trb, fill = "trb"), 
           stat = "summary", fun = "mean", alpha = 0.5) +
  geom_bar(aes(x = factor(cluster), y = orb, fill = "orb"), 
           stat = "summary", fun = "mean", alpha = 0.5) +
  labs(x = "Clusters", y = "Avg Rebounds", title = "WNBA Rebounds") +
  theme_bw() 


nba_rbs <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = trb, fill = "trb"), 
           stat = "summary", fun = "mean", alpha = 0.5) +
  geom_bar(aes(x = factor(cluster), y = orb, fill = "orb"), 
           stat = "summary", fun = "mean", alpha = 0.5) +
  labs(x = "Clusters", y = "Avg Rebounds", title = "NBA Rebounds") +
  theme_bw() 

wnba_rbs + nba_rbs

## Free throw attempts

wnba_fta <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fta), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FTA", title = "WNBA Free Throw Attempts") +
  theme_bw() 


nba_fta <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fta), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FTA", title = "NBA Free Throw Attempts") +
  theme_bw() 

wnba_fta + nba_fta

## Assists

wnba_ast <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ast), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg assists", title = "WNBA Assists") +
  theme_bw() 

nba_ast <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ast), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg assists", title = "NBA Assists") +
  theme_bw() 

wnba_ast + nba_ast


## Steals

wnba_stl <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = stl), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg steals", title = "WNBA Steals") +
  theme_bw() 

nba_stl <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = stl), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg steals", title = "NBA Steals") +
  theme_bw() 

wnba_stl + nba_stl


## Blocks

wnba_blk <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = blk), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg blocks", title = "WNBA Blocks") +
  theme_bw() 

nba_blk <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = blk), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg blocks", title = "NBA Blocks") +
  theme_bw() 

wnba_blk + nba_blk


## Floater (3-10)

wnba_float <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_3_10), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg floaters", title = "WNBA FGA between 3 - 10 feet") +
  theme_bw() 

nba_float <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_3_10), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg floaters", title = "NBA FGA between 3 - 10 feet") +
  theme_bw() 

wnba_float + nba_float


## Midrange (10-16)

wnba_mid <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_10_16), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg midrange", title = "WNBA FGA between 10 - 16 feet") +
  theme_bw() 

nba_mid <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_10_16), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg midrange", title = "NBA FGA between 10 - 16 feet") +
  theme_bw() 

wnba_mid + nba_mid

## Midrange2 (16 - 3)

wnba_mid2 <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_16_3p), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg farther midrange", title = "WNBA FGA between 16 feet - 3's") +
  theme_bw() 

nba_mid2 <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga_16_3p), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg farther midrange", title = "NBA FGA between 16 feet - 3's") +
  theme_bw() 

wnba_mid2 + nba_mid2


## Threes (x3pa)

wnba_threes <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = x3pa), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg 3's", title = "WNBA 3's attempted") +
  theme_bw() 

nba_threes <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = x3pa), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg 3's", title = "NBA 3's attempted") +
  theme_bw() 

wnba_threes + nba_threes


## DRTG - NO SIGNIFICANT DIFFERENCE

wnba_drtg <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = drtg), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg DRTG", title = "WNBA Defensive Rating") +
  theme_bw() 

nba_drtg <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = drtg), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg DRTG", title = "NBA Defensive Rating") +
  theme_bw() 

wnba_drtg + nba_drtg

## ORTG 

wnba_ortg <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ortg), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg ORTG", title = "WNBA Offensive Rating") +
  theme_bw() 

nba_ortg <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ortg), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg ORTG", title = "NBA Offensive Rating") +
  theme_bw() 

wnba_ortg + nba_ortg

## TOV

wnba_tov <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = tov), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg DRTG", title = "WNBA Turnovers") +
  theme_bw() 

nba_tov <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = tov), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg TOV", title = "NBA Turnovers") +
  theme_bw() 

wnba_tov + nba_tov

## Usage

wnba_use <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = usgpercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg Usage", title = "WNBA Usage %") +
  theme_bw() 

nba_use <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = usgpercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg Usage", title = "NBA Usage %") +
  theme_bw() 

wnba_use + nba_use


## Points

wnba_points <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = pts), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg Pts", title = "WNBA Points") +
  theme_bw() 

nba_points <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = pts), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg Usage", title = "NBA Points") +
  theme_bw() 

wnba_points + nba_points

## DWS

wnba_dws <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = dws), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg DWS", title = "WNBA Defensive Win Share") +
  theme_bw() 

nba_dws <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = dws), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg DWS", title = "NBA Defensive Win Share") +
  theme_bw() 

wnba_dws + nba_dws

## OWS

wnba_ows <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ows), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg OWS", title = "WNBA Offensive Win Share") +
  theme_bw() 

nba_ows <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = ows), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg OWS", title = "NBA Offensive Win Share") +
  theme_bw() 

wnba_ows + nba_ows


## PER

wnba_per <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = per), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg PER", title = "WNBA Player Efficiency Rating") +
  theme_bw() 

nba_per <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = per), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg PER", title = "NBA Player Efficiency Rating") +
  theme_bw() 

wnba_per + nba_per

## FGA

wnba_fga <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FGA", title = "WNBA Field Goal Attempts") +
  theme_bw() 

nba_fga <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fga), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FGA", title = "NBA Field Goal Attempts") +
  theme_bw() 

wnba_fga + nba_fga

## FG%

wnba_fgpercent <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fgpercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FG%", title = "WNBA Field Goal %") +
  theme_bw() 

nba_fgpercent <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = fgpercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FG%", title = "NBA Field Goal %") +
  theme_bw() 

wnba_fgpercent + nba_fgpercent

## x3p%

wnba_threepercent <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = x3ppercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg FG%", title = "WNBA 3_point %") +
  theme_bw() 

nba_threepercent <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = x3ppercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg 3's%", title = "NBA 3-point %") +
  theme_bw() 

wnba_threepercent + nba_threepercent

## tspercent - NOT SIGNIFICANT

wnba_tspercent <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = tspercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg TS%", title = "WNBA True Shooting %") +
  theme_bw() 

nba_tspercent <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = tspercent), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg TS%", title = "NBA True Shooting %") +
  theme_bw() 

wnba_tspercent + nba_tspercent


## pf

wnba_pf <- wnba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = pf), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg PF", title = "WNBA Personal Fouls") +
  theme_bw() 

nba_pf <- nba_cluster %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = pf), 
           stat = "summary", fun = "mean") +
  labs(x = "Clusters", y = "Avg PF", title = "NBA Personal Fouls") +
  theme_bw() 

wnba_pf + nba_pf










