# PURPOSE: EDA for WNBA data


# Load in the data --------------------------------------------------------

library(tidyverse)


install.packages("devtools")
library(devtools)
install_github("rtelmore/ballr")

ballr::NBAPerGameAdvStatistics(season = 2022)

nba_adv_stats <- 

