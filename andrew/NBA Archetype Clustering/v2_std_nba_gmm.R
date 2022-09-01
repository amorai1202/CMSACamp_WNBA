# PURPOSE: Remodel GMM using standardized values


# Load Libraries and Data -------------------------------------------------

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
nba_all_stats_filtered <- nba_all_stats %>%
  filter(season >= 2018) %>%
  mutate(mpg = mp / g) %>%
  filter(mpg >= 12, g >= 10)


# Now select trimmed variables before clustering
### NOTE: Variables are only statistics aimed at determining stylistic tendencies 

nba_select_stats <- nba_all_stats_filtered %>%
  select(player, tm, season, pos, g, mp, gs, # Basic info
         fga, x3pa, fta, trb, # Per 100 poss statistics
         astpercent, stlpercent, blkpercent,  # Advanced rate statistics
         distance # Shot location info (distance, proportion of shots from each distance range)
  )

# Adjust positions to just be 5
nba_select_stats <- nba_select_stats %>%
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
    TRUE ~ pos
  ))

# Scaling the Data --------------------------------------------------------

nba_standard_stats <- nba_select_stats %>%
  mutate(across(.cols = -(1:7), .fns = scale)) %>%
  mutate(across(.cols = -(1:7), .fns = as.numeric))

# NBA GMM Modeling --------------------------------------------------------

set.seed(2001)
nba_mclust_std2 <- read_rds("models/nba_mclust_std2.rds")
# nba_mclust_std2 <- Mclust(dplyr::select(nba_standard_stats, -c(1:7)))
# write_rds(nba_mclust_std2, "models/nba_mclust_std2.rds")

summary(nba_mclust_std2)

table("Cluster" = nba_mclust_std2$classification,
      "Position" = nba_standard_stats$pos)




# Loading WNBA Data -------------------------------------------------------

wnba_clean <- read_csv("data/wnba_all_stats_clean.csv")

wnba_select_stats <- wnba_clean %>%
  select(player, tm, season, pos, g, mp, gs, # Basic info
         fga, x3pa, fta, trb, # Per 100 poss statistics
         astpercent, stlpercent, blkpercent,  # Advanced rate statistics
         distance # Shot location info (distance, proportion of shots from each distance range)
  )

# SCALING WNBA 
wnba_standard_stats <- wnba_select_stats %>%
  mutate(across(.cols = -(1:7), .fns = scale)) %>%
  mutate(across(.cols = -(1:7), .fns = as.numeric))


pred_wnba <- predict(nba_mclust_std2, newdata = dplyr::select(wnba_standard_stats, 8:ncol(wnba_standard_stats)))

pred_nba <- predict(nba_mclust_std2, newdata = dplyr::select(nba_standard_stats, 8:ncol(nba_standard_stats)))



# Appending WNBA Model Data ----------------------------------------------------


wnba_standard_stats <- wnba_standard_stats %>%
  bind_cols(pred_wnba$classification) %>%
  bind_cols(pred_wnba$z)

colnames(wnba_standard_stats)[17:ncol(wnba_standard_stats)] <- paste0('prob', 1:7)

colnames(wnba_standard_stats)[16] <- "pred_class"

wnba_model_results <- wnba_standard_stats %>%
  mutate(player_season = paste(player, season, sep = "_")
  ) %>% 
  mutate(uncertainty = 1 - pmax(prob1,
                                prob2,
                                prob3,
                                prob4,
                                prob5,
                                prob6,
                                prob7))

# write_csv(wnba_model_results, "data/wnba_playstyle_clusters.csv")

# Appending NBA Model Results ---------------------------------------------

nba_standard_stats <- nba_standard_stats %>%
  bind_cols(pred_nba$classification) %>%
  bind_cols(pred_nba$z)

colnames(nba_standard_stats)[17:ncol(nba_standard_stats)] <- paste0('prob', 1:7)

colnames(nba_standard_stats)[16] <- "pred_class"

nba_model_results <- nba_standard_stats %>%
  mutate(player_season = paste(player, season, sep = "_")
  ) %>% 
  mutate(uncertainty = 1 - pmax(prob1,
                                prob2,
                                prob3,
                                prob4,
                                prob5,
                                prob6,
                                prob7))

# write_csv(nba_model_results, "data/nba_playstyle_clusters.csv")

# Prob Distance Matrix ----------------------------------------------------

player_dist <- dist(bind_rows(dplyr::select(nba_model_results, prob1:prob7),
                              dplyr::select(wnba_model_results, prob1:prob7)))

player_dist_matrix <- as.matrix(player_dist)

# Combining WNBA and NBA datasets
nba_model_results <- nba_model_results %>%
  mutate(league = "NBA")
wnba_model_results <- wnba_model_results %>%
  mutate(league = "WNBA")
nba_wnba_combined <- bind_rows(nba_model_results, wnba_model_results)


rownames(player_dist_matrix) <- nba_wnba_combined$player_season
colnames(player_dist_matrix) <- nba_wnba_combined$player_season


player_dist_matrix[c(1:3, 2000:2003), c(1:3, 2000:2003)]


# Finding min dist players ------------------------------------------------

long_dist_matrix <- 
  as_tibble(player_dist_matrix) %>%
  mutate(player1 = rownames(player_dist_matrix),
         league = nba_wnba_combined$league) %>% 
  pivot_longer(cols = -c(player1, league),
               names_to = "player2",
               values_to = "distance")

long_dist_matrix %>%
  mutate(league2 = rep(nba_wnba_combined$league, times = nrow(nba_wnba_combined)),
         pred_class = rep(nba_wnba_combined$pred_class, each = nrow(nba_wnba_combined))) %>%
  filter(player1 != player2) %>%
  filter(league == "WNBA",
         league2 == "NBA") %>%
  group_by(player1) %>%
  filter(distance == min(distance)) %>%
  ungroup() %>%
  arrange(player1)




wnba21_nba22_comps <- long_dist_matrix %>%
  mutate(league2 = rep(nba_wnba_combined$league, times = nrow(nba_wnba_combined)),
         pred_class = rep(nba_wnba_combined$pred_class, each = nrow(nba_wnba_combined))) %>%
  filter(player1 != player2) %>%
  filter(league == "WNBA",
         league2 == "NBA",
         str_detect(player1, "2021"),
         str_detect(player2, "2022")) %>%
  group_by(player1) %>%
  slice_min(distance, n = 5) %>% 
  ungroup() %>% 
  separate(player1, c("wnba_player", "wnba_season"), sep = "_") %>% 
  separate(player2, c("nba_player", "nba_season"), sep = "_")

wide_wnba21_nba22_comps <- wnba21_nba22_comps %>%
  group_by(wnba_player) %>%
  slice_min(distance, n = 1) %>%
  ungroup()


# Making dataset wider so player1 player 2 etc are their own respective columns
for (i in 2:5) {
  player_list <- wnba21_nba22_comps$nba_player[seq(i, nrow(wnba21_nba22_comps), by = 5)]
  distance_list <- wnba21_nba22_comps$distance[seq(i, nrow(wnba21_nba22_comps), by = 5)]
  wide_wnba21_nba22_comps <- wide_wnba21_nba22_comps %>%
    mutate(nba_player_x = player_list,
           distance_x = distance_list) 
  
  colnames(wide_wnba21_nba22_comps)[(ncol(wide_wnba21_nba22_comps)-1):ncol(wide_wnba21_nba22_comps)] <- c(paste0("nba_player", i), paste0("distance", i))
}


# Trying to get player links into wide dataset for ease of shiny app pictures
wnba_tmp <- read_csv("data/wnba_all_stats_clean2.csv")


# WNBA Links

tmp2 <- wnba_tmp %>% 
  filter(season == 2021) %>%
  select(player, link, season) %>%
  rename(wnba_player = "player") %>%
  right_join(wide_wnba21_nba22_comps) %>%
  rename(wnba_link = "link",
         nba_player1 = "nba_player")


# NBA Links

nba_links <- nba_all_stats_filtered %>% 
  filter(season == 2022) %>% 
  select(player, link)

tmp3 <- tmp2 %>%
  left_join(nba_links, by = c("nba_player1" = "player")) %>% 
  left_join(nba_links, by = c("nba_player2" = "player")) %>%
  left_join(nba_links, by = c("nba_player3" = "player")) %>%
  left_join(nba_links, by = c("nba_player4" = "player")) %>%
  left_join(nba_links, by = c("nba_player5" = "player")) %>% 
  rename(nba_link1 = "link.x",
         nba_link2 = "link.y",
         nba_link3 = "link.x.x",
         nba_link4 = "link.y.y",
         nba_link5 = "link")

# ISOLATING IDs
  
wide_wnba21_nba22_links <- tmp3 %>%
    # WNBA PLAYER ID
    separate(wnba_link, sep = "/", into = c("A", "B", "C", "D", "E"), remove = FALSE) %>%
    separate(E, sep = "\\.", into = c("wnba_id", "F")) %>%
    select(-c(A:D), -`F`) %>%
    #NBA PLAYER 1 ID
    separate(nba_link1, sep = "/", into = c("A", "B", "C", "D"), remove = FALSE) %>%
    separate(D, sep = "\\.", into = c("nba_id1", "F")) %>%
    select(-c(A:C), -`F`) %>%
    # NBA PLAYER 2 ID
    separate(nba_link2, sep = "/", into = c("A", "B", "C", "D"), remove = FALSE) %>%
    separate(D, sep = "\\.", into = c("nba_id2", "F")) %>%
    select(-c(A:C), -`F`) %>%
    # NBA PLAYER 3 ID
    separate(nba_link3, sep = "/", into = c("A", "B", "C", "D"), remove = FALSE) %>%
    separate(D, sep = "\\.", into = c("nba_id3", "F")) %>%
    select(-c(A:C), -`F`) %>%
    # NBA PLAYER 4 ID
    separate(nba_link4, sep = "/", into = c("A", "B", "C", "D"), remove = FALSE) %>%
    separate(D, sep = "\\.", into = c("nba_id4", "F")) %>%
    select(-c(A:C), -`F`) %>%
    # NBA PLAYER 5 ID
    separate(nba_link5, sep = "/", into = c("A", "B", "C", "D"), remove = FALSE) %>%
    separate(D, sep = "\\.", into = c("nba_id5", "F")) %>%
    select(-c(A:C), -`F`) 

# write_csv(wide_wnba21_nba22_links, "data/wnba21_nba22_comps_links.csv")

  

    








