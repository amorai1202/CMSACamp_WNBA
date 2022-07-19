# PURPOSE: Update PCA using new dataset (without percentage stats)

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


# PCA with total numeric variables (no %'s) ----------------------------------------
library(broom)
library(stringr)
library(factoextra)

#remove columns with percentages/containing na's
wnba_numeric <- wnba_2021_stats %>% 
  select_if(is.numeric) %>% 
  select(-season, -g, -mp, -gs, -mpg,
         -fgpercent, -x3ppercent, -x2ppercent, -ftpercent,
         -fga_2p, -fga_0_3, -fga_3_10, -fga_10_16,
         -fga_16_3p, -fga_3p, -fg_2p, -fg_0_3, -fg_3_10, 
         -fg_10_16, -fg_16_3p, -fg_3p, -fg_2p_asst, -fg_2p_asst,
         -corner_3s_attempt, -corner_3s_percent, -fg_3p_asst) %>% 
  select(-contains("percent"))
  
#LOOK AT CORRELATION MATRIX

library(ggcorrplot)
wnba_cor_matrix <- cor(wnba_numeric)
ggcorrplot(wnba_cor_matrix)

library(dplyr)
library(tidyr)

corr_table <- cor(wnba_numeric) %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)


#remove highly correlated attempted variables
model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -x3pa, -x2pa, -fta, x3par, -fga_tot, 
                                   -x3pa_tot, -x2pa_tot, -fta_tot))

pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)


# PC's proportion of variance explained -----------------------------------
## how ‘important’ each Principal Component is

get_eig(pca_wnba) #top 10 dimensions explain 90% of the variability

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

##Extra biplot
# fviz_pca_biplot(pca_wnba, label = "var",  
#                 alpha.ind = .5, col.var = "darkblue",
#                 alpha.var = .75)


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


# Pick drivers in top 10 PC's (check for low correlations) --------------

pc1 <- pca_var %>%  #fg_tot, shoot_fouls_drawn, per
  select(Variable:PC1) %>% 
  arrange(desc(PC1)) %>% 
  slice(1:10)

var<-get_pca_var(pca_wnba)
a<-fviz_contrib(pca_wnba, "var", axes=1, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of first Principal Components")

pc2 <- update_var %>%  #orb, distance, trb
  select(c(Variable,PC2)) %>% 
  arrange(desc(PC2)) %>% 
  slice(1:10)

pc3 <- update_var %>%  #tov, ortg, oncourt
  select(c(Variable,PC3)) %>% 
  arrange(desc(PC3)) %>% 
  slice(1:10)

pc4 <- update_var %>%  #shoot_fouls_committed, pf_tot, x3p
  select(c(Variable,PC4)) %>% 
  arrange(desc(PC4)) %>% 
  slice(1:10)

pc5 <- update_var %>%  #ft, pts, badpass
  select(c(Variable,PC5)) %>% 
  arrange(desc(PC5)) %>% 
  slice(1:10)

pc6 <- update_var %>%  #tov, drtg, ftr
  select(c(Variable,PC6)) %>% 
  arrange(desc(PC6)) %>% 
  slice(1:10)

pc7 <- update_var %>%  #stl, drtg, pf
  select(c(Variable,PC7)) %>% 
  arrange(desc(PC7)) %>% 
  slice(1:10)

pc8 <- update_var %>% #stl, ftr, oncourt
  select(c(Variable,PC8)) %>% 
  arrange(desc(PC8)) %>% 
  slice(1:10)

pc9 <- update_var %>%  #on_off, blk, oncourt
  select(c(Variable,PC9)) %>% 
  arrange(desc(PC9)) %>% 
  slice(1:10)

pc10 <- update_var %>%  #on_off, dws, drtg
  select(c(Variable,PC10)) %>% 
  arrange(desc(PC10)) %>% 
  slice(1:10)

#Checking correlations for non-repeated top 3 variables in each PC

pc_corr <- wnba_2021_stats %>% 
  select(fg_tot, shoot_fouls_drawn, per, 
         orb, distance, trb,
         tov, ortg, oncourt,
         shoot_fouls_committed, pf_tot, x3p,
         ft, pts, badpass,
         drtg, ftr,
         stl, pf,
         ftr,
         on_off, blk,
         dws) %>%
  cor() %>% 
  round(1)

ggcorrplot(pc_corr, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

## INITIAL VARIABLES FROM PCA TO USE IN CLUSTERING:
## fg_tot, shoot_fouls_drawn, per, orb, distance, trb, tov, ortg, oncourt,
## shoot_fouls_committed, pf_tot, x3p, ft, pts, badpass, drtg, ftr,
## stl, pf, ftr, on_off, blk, dws



# Try clustering PC's -----------------------------------------------------


#10 columns with the highest eigenvalues
pca_transform <- as.data.frame(-pca_wnba$x[,1:10])
fviz_nbclust(pca_transform, kmeans, method = 'wss')
fviz_nbclust(pca_transform, kmeans, method = 'silhouette')
fviz_nbclust(pca_transform, kmeans, method = 'gap_stat')

k <- 5

kmeans_wnba = kmeans(pca_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_wnba, data = pca_transform)

#Look at players in each cluster














