# PURPOSE: Explore PCA - 2021 WNBA Season

# Load and filter data -----------------------------------------------------------

library(tidyverse)
library(wehoop)
library(devtools)

wnba_all_stats <- read_csv("data/wnba_all_stats.csv") #1997-2022
wnba_pbp <- read_csv("data/wnba_pbp.csv") #2018-2022

# Start by focusing on 2021 season
wnba_2021_stats <- wnba_all_stats %>% 
  select(-c(70,74)) %>%  #remove off_fouls_drawn (all na's) and link col's
  mutate(mpg = mp/g) %>% 
  filter(season == "2021") 

wnba_ecdf <- wnba_2021_stats %>%
  ggplot(aes(x = mpg)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "mpg",
       y = "Proportion of WNBA players")
    
#Eliminating players that played less than 10 mpg and 5 games during the 2021 season
#Removing TOT (errors)

wnba_2021_stats <- wnba_2021_stats %>% 
  filter(mpg > 10, g > 5, !(tm == "TOT"))   #183 -> 136 -> 131 -> 122

#Which players played for more than 1 team during the season
tbl <- table(wnba_2021_stats$player)
#tbl[tbl>1]

#Remove rows for the 2 players that played less games with their 2nd team 

wnba_2021_stats <- wnba_2021_stats %>% 
  slice(-c(122, 115)) #120 players left


names(which(colSums(is.na(wnba_2021_stats)) > 0))
#HOW DO WE DEAL WITH NA'S


# DEAL WITH STATS WITH MINIMAL ATTEMPTS


  




# PCA ---------------------------------------------------------------------

# REPLACE NA's WITH 0 FOR NOW

wnba_numeric <- wnba_2021_stats %>% 
  select_if(is.numeric) %>% 
  mutate_all(~replace_na(.,0))
  
model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -season, -g, -mp, -gs, -mpg))

pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)

summary(pca_wnba)

library(broom)
pca_wnba %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x),
             color = "darkred", 
             linetype = "dashed") +
  theme_bw()


library(stringr)
pca_var <- pca_wnba %>%
  # Extract variable coordinates
  tidy(matrix = "rotation") %>%
  # Format table form long to wide
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")%>%
  # Rename column with variable names
  rename(Variable=column) %>% 
  # Take absolute value
  mutate(across(PC1:PC65, ~ abs(.x))) 
  

library(factoextra)
fviz_eig(pca_wnba) # top 10 dimensions explain 80% of the variability
fviz_pca_biplot(pca_wnba, label = "var", 
                alpha.ind = .5, col.var = "darkblue",
                alpha.var = .75)
get_eig(pca_wnba) #table of eigen/variances

#Matrix of principal component scores (sign doesn't matter) -> pca_wnba$x

fviz_contrib(pca_wnba, choice = "var", axes = 1:2, top = 10) #contribution of var to first 2 dim
fviz_pca_ind(pca_wnba) #individual points in first 2 dim


# Pick drivers in top 16 PC's (90% of the variability explained) with low correlations

pc1 <- pca_var %>%  
  select(Variable:PC1) %>% 
  arrange(desc(PC1)) %>% 
  slice(1:5)

#cor()



pc2 <- pca_var %>%  
  select(c(Variable,PC2)) %>% 
  arrange(desc(PC2)) %>% 
  slice(1:5)


##EXTRA
pca_results <- get_pca_var(pca_wnba)
library("corrplot")
corrplot(pca_results$cos2, is.corr=FALSE)



# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(pca_results$coord, centers = 2, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(pca_wnba, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")


# Find correlations between chosen variables from each PC (correlation matrix)






