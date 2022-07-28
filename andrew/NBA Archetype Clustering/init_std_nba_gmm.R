# PURPOSE: Run GMM on Scaled NBA Data and Apply to Scaled WNBA Data



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
  select(1:9, # Basic info
         11, 14, 20, 24, # Per 100 poss statistics
         36, 39:42,  # Advanced rate statistics (FT rate, Block%, etc)
         53, 55:59 # Shot location info (distance, proportion of shots from each distance range)
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
  mutate(across(.cols = -(1:9), .fns = scale)) %>%
  mutate(across(.cols = -(1:9), .fns = as.numeric))

# NBA GMM Modeling --------------------------------------------------------
nba_standard_stats
set.seed(2001)
nba_mclust_std <- Mclust(dplyr::select(nba_standard_stats, -c(1:9)))
# write_rds(nba_mclust_std, "models/init_nba_mclust_std.rds")

summary(nba_mclust_std)

table("Cluster" = nba_mclust_std$classification,
      "Position" = nba_standard_stats$pos)




# Loading WNBA Data -------------------------------------------------------

wnba_clean <- read_csv("data/wnba_all_stats_clean.csv")

wnba_select_stats <- wnba_clean %>%
  select(1:7,  # Basic info
         9, 12, 18, 21, # Per 100 poss stats
         32, 34:37, # Advanced rate statistics (FT Rate, Block %, etc)
         46, 48:52, # Shooting tendency data
  )

# SCALING WNBA 
wnba_standard_stats <- wnba_select_stats %>%
  mutate(across(.cols = -(1:7), .fns = scale)) %>%
  mutate(across(.cols = -(1:7), .fns = as.numeric))


# Predictions -------------------------------------------------------------


pred_wnba <- predict(nba_mclust_std, newdata = dplyr::select(wnba_standard_stats, 8:22))


wnba_model_results <- wnba_standard_stats %>%
  mutate(pred_class = pred_wnba$classification,
         prob1 = pred_wnba$z[,1],
         prob2 = pred_wnba$z[,2],
         prob3 = pred_wnba$z[,3],
         prob4 = pred_wnba$z[,4],
         prob5 = pred_wnba$z[,5],
         prob6 = pred_wnba$z[,6],
         prob7 = pred_wnba$z[,7],
         prob8 = pred_wnba$z[,8],
         prob9 = pred_wnba$z[,9],
         
         player_season = paste(player, season, sep = "_")
  ) %>% 
  mutate(uncertainty = 1 - pmax(prob1, prob2, prob3, prob4, prob5, prob6, prob7, prob8, prob9))


# DOING SAME FOR NBA BEFORE TRYING TO COMPUTE DISTANCE MATRIX

nba_standard_stats <- nba_standard_stats %>% 
  select(colnames(wnba_select_stats)) 

pred_nba <- predict(nba_mclust_std, newdata = dplyr::select(nba_standard_stats, 8:22))

nba_model_results <- nba_standard_stats %>%
  mutate(pred_class = pred_nba$classification,
         prob1 = pred_nba$z[,1],
         prob2 = pred_nba$z[,2],
         prob3 = pred_nba$z[,3],
         prob4 = pred_nba$z[,4],
         prob5 = pred_nba$z[,5],
         prob6 = pred_nba$z[,6],
         prob7 = pred_nba$z[,7],
         prob8 = pred_nba$z[,8],
         prob9 = pred_nba$z[,9],
         player_season = paste(player, season, sep = "_")
  ) %>% 
  mutate(uncertainty = 1 - pmax(prob1, prob2, prob3, prob4, prob5, prob6, prob7, prob8, prob9))


nba_model_results %>%
  filter(season == 2022) %>%
  arrange(desc(uncertainty)) %>%
  filter(pred_class == 1)

nba_model_results %>%
  select(16:22) %>%
  pairs()


player_dist <- dist(bind_rows(dplyr::select(nba_model_results, prob1:prob9),
               dplyr::select(wnba_model_results, prob1:prob9)))

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

long_dist_matrix <- 
  as_tibble(player_dist_matrix) %>%
  mutate(player1 = rownames(player_dist_matrix),
         league = nba_wnba_combined$league) %>% 
  pivot_longer(cols = -c(player1, league),
               names_to = "player2",
               values_to = "distance")

long_dist_matrix %>%
  mutate(league2 = rep(nba_wnba_combined$league, times = nrow(nba_wnba_combined))) %>%
  filter(player1 != player2) %>%
  filter(league == "WNBA",
         league2 == "NBA") %>%
  group_by(player1) %>%
  filter(distance == min(distance))

#SHIT
nba_wnba_combined %>% filter(player_season %in% c("Sue Bird_2021", "Quincy Acy_2019")) %>% view()


plot(nba_mclust_std)

