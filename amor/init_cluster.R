# PURPOSE: Explore clustering - WNBA data (2021 season)


# DOCUMENTATION -----------------------------------------------------------

# Columns 1-27 are per 100 stats (3pa, 2pa, etc) and shooting percentages
# Columns 28-45 are advanced stats (ast%, per, ts%, etc)
# Columns 46-62 are shooting tendencies - % of shots that are 2s, 3s, 
#distance where they're taken, etc
# Columns 63-73 are play-by-play stats - on off, turnover type, and 1s, fouls drawn/committed

#https://alexcstern.github.io/hoopDown.html

\
# Load the data -----------------------------------------------------------

library(tidyverse)
library(wehoop)
library(devtools)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(factoextra)
library(extrafont)
loadfonts() 
library(ggsci) 
library(broom)
library(igraph)


wnba_all_stats <- read_csv("data/wnba_all_stats.csv") #1997-2022
wnba_pbp <- read_csv("data/wnba_pbp.csv") #2018-2022

# Start by focusing on 2021 season
wnba_2021_stats <- wnba_all_stats %>% 
  filter(season == "2021", mp > 300) %>% #filter players that have played more than 300 minutes 


# K-means clustering ------------------------------------------------------

### CHOOSING PREDICTOR VARIABLES:
#x3pa, x2pa, x2ppercent, x3ppercent, fta, ftpercent
#trb, ast, stl, blk, tov, pts, orb
#distance

#colnames(wnba_2021_stats)

var <- wnba_2021_stats %>% 
  dplyr::select(x3pa, x2pa, x2ppercent, x3ppercent, fta, ftpercent,
                trb, ast, stl, blk, tov, pts, orb, distance) %>% 
  scale() %>% 
  na.omit() #remove rows with NA values

wnba_2021_stats <- wnba_2021_stats %>% 
  dplyr::select(player, x3pa, x2pa, x2ppercent, x3ppercent, fta, ftpercent,
                trb, ast, stl, blk, tov, pts, orb, distance) %>% 
  na.omit()

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

# CHOOSE 6 CLUSTERS FOR NOW

set.seed(101)
k <- 6

init_kmeans <- kmeans(var, algorithm = "Lloyd", centers = k, nstart = 30)
km_centers <- as.data.frame(init_kmeans$centers) # SCALED cluster centers/means

# Name clusters 
km_centers$Cluster <- c("Cluster 1", "Cluster 2", "Cluster 3",
                        "Cluster 4", "Cluster 5", "Cluster 6")
km_centers$Cluster <- factor(km_center$Cluster, levels = 
                        c("Cluster 1", "Cluster 2", "Cluster 3",
                        "Cluster 4", "Cluster 5", "Cluster 6"))

km_centers <- km_centers %>% 
  pivot_longer(!Cluster, names_to = "feature", values_to = "val")


# View individual clusters -----------------------------------------------------------

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
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_bw()+ 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())




# Try PCA -----------------------------------------------------------------













