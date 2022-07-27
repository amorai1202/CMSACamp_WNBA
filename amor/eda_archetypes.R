# PURPOSE: EDA type visuals for distribution stats separated by archetypes


# Load the data -----------------------------------------------------------

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


final_wnba_mclust <- readRDS("models/final_wnba_mclust.rds")
final_nba_mclust <- readRDS("models/final_nba_mclust.rds")


wnba_cluster <- wnba_all_stats %>%
  mutate(cluster =
           final_wnba_mclust$classification,
         uncertainty =
           final_wnba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(uncertainty)

nba_cluster <- nba_all_stats %>%
  mutate(cluster =
           final_nba_mclust$classification,
         uncertainty =
           final_nba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(uncertainty)

wnba_cluster <- wnba_cluster %>% 
  select(player_season, cluster, uncertainty, everything())

nba_cluster <- nba_cluster %>% 
  select(player_season, cluster, uncertainty, everything())

# Rename clusters ----------------------------------------------------------

# wnba_cluster <- wnba_cluster %>%
# mutate(cluster = case_when(cluster == 1 ~ "Reserves",
#                            cluster == 2 ~ "Traditional Bigs",
#                            cluster == 3 ~ "Facilitators/Shooters",
#                            cluster == 4 ~ "Primary Scorers/Initiators",
#                            cluster == 5 ~ "Shooting Threats"))
# nba_cluster <- nba_cluster %>%
#   mutate(cluster = case_when(cluster == 1 ~ "Traditional Bigs",
#                              cluster == 2 ~ "Facilitators/Shooters",
#                              cluster == 3 ~ "Roleplayers",
#                              cluster == 4 ~ "Primary Scorers/Initiators",
#                              cluster == 5 ~ "Reserves"))
# 
# write_csv(wnba_cluster, "data/wnba_archetypes.csv")
# write_csv(nba_cluster, "data/nba_archetypes.csv")

wnba_cluster <- read_csv("data/wnba_archetypes.csv")
nba_cluster <- read_csv("data/nba_archetypes.csv")



# Try clustvarsel ---------------------------------------------------------

# library(clustvarsel)
# set.seed(2001)
# 
# ##STANDARDIZE!!
# 
# wnba_select_stats <- wnba_all_stats %>% 
#   dplyr::select(shoot_fouls_drawn, x3p, 
#                 distance, trb, orb, ows, oncourt, ws_40, 
#                 ortg, ft, fg, pts, x3par, shoot_fouls_committed, 
#                 fga, ftr, pf, tov, drtg, stl, per, blk, ast, on_off) 
# 
# wnba_std <- wnba_select_stats %>%
#   mutate(across(.cols = (1:24), .fns = scale)) %>%
#   mutate(across(.cols = (1:24), .fns = as.numeric))
# 
# init_wnba_clust <- clustvarsel(wnba_std) #to see which variables were picked first

# Cluster EDA -------------------------------------------------------------

#Pts & x3pa
wnba_pts_x3pa <- wnba_cluster %>%
  ggplot(aes(x = pts, y = x3p,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Points per 100 possessions",
       y = "3 point attempts",
       fill = "cluster",
       colour = "WNBA Archetypes") 

nba_pts_x3pa <- nba_cluster %>%
  ggplot(aes(x = pts, y = x3p,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Points per 100 possessions",
       y = "3 point attempts",
       fill = "cluster",
       colour = "NBA Archetypes")

library(patchwork)

wnba_pts_x3pa + nba_pts_x3pa + plot_layout(nrow = 2)

#Rebounds/assists

wnba_trb_ast <- wnba_cluster %>%
  ggplot(aes(x = trb, y = ast,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Total rebounds",
       y = "Assists",
       fill = "cluster",
       colour = "WNBA Archetypes") 

nba_trb_ast <- nba_cluster %>%
  ggplot(aes(x = trb, y = ast,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Total rebounds",
       y = "Assists",
       fill = "cluster",
       colour = "NBA Archetypes")

wnba_trb_ast + nba_trb_ast + plot_layout(nrow = 2)

#Usage/PER

wnba_usage <- wnba_cluster %>%
  ggplot(aes(x = usgpercent, y = per,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Usage Percentage",
       y = "Player Efficiency Rating",
       fill = "cluster",
       colour = "WNBA Archetypes") 

nba_usage <- nba_cluster %>%
  ggplot(aes(x = usgpercent, y = per,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Usage Percentag",
       y = "Player Efficiency Rating",
       fill = "cluster",
       colour = "NBA Archetypes")

wnba_usage + nba_usage + plot_layout(nrow = 2)

#OWS/DWS

wnba_ws <- wnba_cluster %>%
  ggplot(aes(x = ows, y = dws,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Offensive Win Share",
       y = "Defensive Win Share",
       fill = "cluster",
       colour = "WNBA Archetypes") 

nba_ws <- nba_cluster %>%
  ggplot(aes(x = ows, y = dws,
             color = as.factor(cluster))) +
  geom_point(alpha = 0.4) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "right") +
  labs(x = "Offensive Win Share",
       y = "Defensive Win Share",
       fill = "cluster",
       colour = "NBA Archetypes")

wnba_ws + nba_ws + plot_layout(nrow = 2)


#TRIED THE FOLLOWING BUT WOULD NOT USE:
#Points/distance
#Steals/blocks



