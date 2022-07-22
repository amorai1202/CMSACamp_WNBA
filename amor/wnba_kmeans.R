# PURPOSE: Explore k-means clustering with variables chosen by PCA

# Load and filter data -----------------------------------------------------------

library(tidyverse)
wnba_all_stats <- read_csv("data/wnba_all_stats.csv") #1997-2022

# 2021 season
wnba_2021_stats <- wnba_all_stats %>% 
  select(-c("off_fouls_drawn", "link")) %>%  #remove off_fouls_drawn (all na's) and link column
  mutate(mpg = mp/g) %>% 
  filter(season == "2021") 

#Eliminating players that played less than 10 mpg and 5 games during the 2021 season

wnba_ecdf <- wnba_2021_stats %>% #justification
  ggplot(aes(x = mpg)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "mpg",
       y = "Proportion of WNBA players")

wnba_2021_stats <- wnba_2021_stats %>% 
  filter(mpg > 10, g > 5, !(tm == "TOT"))  #Removing TOT players (errors)

#Which players played for more than 1 team during the season
tbl <- table(wnba_2021_stats$player)
#tbl[tbl>1]

#Remove rows for the 2 players that played less games with their 2nd team 

wnba_2021_stats <- wnba_2021_stats %>% 
  slice(-c(122, 115)) #120 players left


# K-means clustering ------------------------------------------------------

var <- wnba_2021_stats %>% 
  dplyr::select(fg_tot, shoot_fouls_drawn, per, orb, distance, trb, tov, ortg, oncourt,
                shoot_fouls_committed, pf_tot, x3p, ft, pts, badpass, drtg, ftr,
                stl, pf, ftr, on_off, blk, dws) %>% 
  scale() 

#Find the optimal k

n_clusters_search <- 2:12
tibble(total_wss = 
         # Compute total WSS for each number by looping with sapply
         sapply(n_clusters_search,
                function(k) {
                  kmeans_results <- kmeans(var, 
                                           centers = k, nstart = 30)
                  # Return the total WSS for choice of k
                  return(kmeans_results$tot.withinss)
                })) %>%
  mutate(k = n_clusters_search) %>%
  ggplot(aes(x = k, y = total_wss)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters K", y = "Total WSS") +
  theme_bw()


# What num of cluster to choose?

# Try 6 clusters

set.seed(101)
k <- 6

init_kmeans <- kmeans(var, algorithm = "Lloyd", centers = k, nstart = 30)
km_centers <- as.data.frame(init_kmeans$centers) # SCALED cluster centers/means

# Name clusters 
km_centers$Cluster <- c("Cluster 1", "Cluster 2", "Cluster 3",
                        "Cluster 4", "Cluster 5", "Cluster 6")
km_centers$Cluster <- factor(km_centers$Cluster, levels = 
                               c("Cluster 1", "Cluster 2", "Cluster 3",
                                 "Cluster 4", "Cluster 5", "Cluster 6"))

km_centers <- km_centers %>% 
  pivot_longer(!Cluster, names_to = "feature", values_to = "val")

# View individual clusters -----------------------------------------------------------
library(gghighlight)

#Cluster 1:
km_centers %>% 
  ggplot(aes(x=feature, y=val, color=Cluster)) + 
  geom_point(alpha = 0.75) + 
  gghighlight(Cluster=='Cluster 1', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  
       title = "Visualizing K-Means Cluster Makeups", 
       subtitle = "Cluster 1") +  # plot subtitle
  theme_bw() +
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=90, size=10))

#View players in cluster 1

tibble(cluster=init_kmeans$cluster, name=wnba_2021_stats$player) %>%
  filter(cluster==1)

#All clusters:

km_centers %>% 
  ggplot(aes(x=feature, y=val, color=Cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create separate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_bw()+ 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())












