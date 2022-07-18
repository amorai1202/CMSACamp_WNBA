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


wnba_numeric <- wnba_2021_stats %>% 
  select_if(is.numeric) 
  
#remove columns with percentages/containing na's
model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -season, -g, -mp, -gs, -mpg,
                                   -fgpercent, -x3ppercent, -x2ppercent, -ftpercent,
                                   -fga_2p, -fga_0_3, -fga_3_10, -fga_10_16,
                                   -fga_16_3p, -fga_3p, -fg_2p, -fg_0_3, -fg_3_10, 
                                   -fg_10_16, -fg_16_3p, -fg_3p, -fg_2p_asst, -fg_2p_asst,
                                   -corner_3s_attempt, -corner_3s_percent, -fg_3p_asst))

pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)


# PC's proportion of variance explained -----------------------------------

get_eig(pca_wnba) #top 10 dimensions explain 90% of the variability

pca_wnba %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x),
             color = "darkred", 
             linetype = "dashed") +
  theme_bw()

fviz_eig(pca_wnba) #scree plot

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
  mutate(across(PC1:PC61, ~ abs(.x))) 


# Pick drivers in top 10 PC's (check for low correlations) --------------

pc1 <- pca_var %>%  
  select(Variable:PC1) %>% 
  arrange(desc(PC1)) %>% 
  slice(1:10)

wnba_2021_stats %>% 
  select(fg_tot, pts_tot, x2p_tot, x2pa_tot, shoot_fouls_drawn) %>% 
  cor()

# TOTALS STAT ARE ALL VERY CORRELATED WITH EACH OTHER!


pc2 <- pca_var %>%  
  select(c(Variable,PC2)) %>% 
  arrange(desc(PC2)) %>% 
  slice(1:5)
















