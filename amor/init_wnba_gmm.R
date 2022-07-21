# PURPOSE: Clustering WNBA players using GMM

# Load and filter data -----------------------------------------------------------

library(tidyverse)
wnba_all_stats_raw <- read_csv("data/wnba_all_stats_clean.csv")


# Use PCA for variable selection ------------------------------------------

library(broom)
library(stringr)
library(factoextra)

#remove columns with percentages/containing na's
wnba_numeric <- wnba_all_stats_raw %>% 
  select_if(is.numeric) %>% 
  select(-season, -g, -mp, -gs, -mpg,
         -fgpercent, -x3ppercent, -x2ppercent, -ftpercent,
         -fga_2p, -fga_0_3, -fga_3_10, -fga_10_16,
         -fga_16_3p, -fga_3p, -fg_2p, -fg_0_3, -fg_3_10, 
         -fg_10_16, -fg_16_3p, -fg_3p, -fg_2p_asst, -fg_2p_asst,
         -corner_3s_attempt, -corner_3s_percent, -fg_3p_asst) %>% 
  select(-contains("percent"))

library(dplyr)
library(tidyr)


#remove highly correlated attempted variables
model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -x3pa, -x2pa, -fta, x3par, -fga_tot, 
                                   -x3pa_tot, -x2pa_tot, -fta_tot))

pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)


# PC's proportion of variance explained 
## how ‘important’ each Principal Component is

get_eig(pca_wnba) #top 10 dimensions explain ~90% of the variability

dim_plot <- pca_wnba %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x),
             color = "darkred", 
             linetype = "dashed") +
  theme_bw()

scree_plot <- fviz_eig(pca_wnba) #scree plot

library(patchwork)
dim_plot + scree_plot


#Table of variables and PC's

pca_var <- pca_wnba %>%
  # Extract variable coordinates
  tidy(matrix = "rotation") %>%
  # Format table form long to wide
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")%>%
  # Rename column with variable names
  rename(Variable=column) %>% 
  # Take absolute value
  mutate(across(PC1:PC45, ~ abs(.x))) 


# Pick drivers in top 10 PC's 

var <- data.frame(get_pca_var(pca_wnba)$contrib)

var %>% 
  select(1) %>% 
  arrange(Dim.1) 
#ft_tot, shoot_fouls_drawn, pts_tot, fg_tot, x2p_tot 

var %>% 
  select(2) %>% 
  arrange(Dim.2) 
#ast_tot, x3p, distance, trb, x3p_tot,orb  

var %>% 
  select(3) %>% 
  arrange(Dim.3) 
#ows, oncourt, ws_40, tov, ortg 

var %>% 
  select(4) %>% 
  arrange(Dim.4)
#stl_tot, ft, fg, pts, fga   

var %>% 
  select(5) %>% 
  arrange(Dim.5)
#x3p, x3par, ast, pf_tot, shoot_fouls_committed

var %>% 
  select(6) %>% 
  arrange(Dim.6)
#fga, tov, stl, drtg, ftr 

var %>% 
  select(7) %>% 
  arrange(Dim.7)
#x2p_tot, tov, ft, ftr, pf 

var %>% 
  select(8) %>% 
  arrange(Dim.8)
#pf, tov, ast, stl_tot, stl 

var %>% 
  select(9) %>% 
  arrange(Dim.9)
#ortg, drtg, blk, blk_tot, stl   

var %>% 
  select(10) %>% 
  arrange(Dim.10)
#per, blk, ast, oncourt, on_off 

#Remove very similar (tot) variables, repeated variables

## INITIAL VARIABLES FROM PCA TO USE IN CLUSTERING:
#shoot_fouls_drawn, x3p, distance, trb, orb, ows, oncourt, ws_40, ortg, ft, fg, pts   
#x3par, shoot_fouls_committed, fga, ftr, pf, tov, drtg, stl, per, blk, ast, on_off 


# Try mclust on selected variables ---------------------------------------------

library(mclust)
library(vip)

set.seed(2001)

##STANDARDIZE!!

wnba_select_stats <- wnba_all_stats %>% 
  dplyr::select(shoot_fouls_drawn, x3p, 
             distance, trb, orb, ows, oncourt, ws_40, 
             ortg, ft, fg, pts, x3par, shoot_fouls_committed, 
             fga, ftr, pf, tov, drtg, stl, per, blk, ast, on_off) 
             
wnba_std <- wnba_select_stats %>%
  mutate(across(.cols = (1:24), .fns = scale)) %>%
  mutate(across(.cols = (1:24), .fns = as.numeric))

init_wnba_mclust <- Mclust(wnba_std)

#write_rds(init_wnba_mclust, "models/init_wnba_mclust.rds")

summary(init_wnba_mclust)

#Position-Cluster table
table("Clusters" = init_wnba_mclust$classification, "Positions" = wnba_all_stats$pos)

wnba_player_probs <- init_wnba_mclust$z

colnames(wnba_player_probs) <- paste0("Cluster ", 1:5)

#Cluster probabilities
wnba_player_probs <- wnba_player_probs %>%
  as_tibble() %>%
  mutate(player = 
           wnba_all_stats$player, 
         season = wnba_all_stats$season) %>%
  pivot_longer(contains("Cluster"),
               names_to = "cluster",
               values_to = "prob")

#Uncertainty plot
wnba_all_stats %>%
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


#Add clusters to all stats

#facet plots, looking at players by stats from each cluster
#seeing if a clustrer exists in the wnba that isnt in the nba

# Try clustvarsel on selected variables -----------------------------------

library(clustvarsel)

set.seed(2001)
init_wnba_clustvarsel <- clustvarsel(dplyr::select(wnba_all_stats, shoot_fouls_drawn, x3p, 
                                         distance, trb, orb, ows, oncourt, ws_40, 
                                         ortg, ft, fg, pts, x3par, shoot_fouls_committed, 
                                         fga, ftr, pf, tov, drtg, stl, per, blk, ast, on_off))
summary(init_wnba_clustvarsel$model)
init_wnba_clustvarsel$subset



