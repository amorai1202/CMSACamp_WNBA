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

wnba_numeric <- wnba_2021_stats %>% 
  
  
  
  # select_if(is.numeric) %>% 
  # mutate_all(~replace_na(.,0))

#model_x <- as.matrix(dplyr::select(wnba_numeric, 
                                   -season, -g, -mp, -gs, -mpg))

#pca_wnba <- prcomp(model_x, center = TRUE, scale = TRUE)




