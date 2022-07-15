# PURPOSE: Start working with RAPM



library(tidyverse)
library(ballr)

wnba_all_stats <- read_csv("data/wnba_all_stats.csv")



head(wnba_all_stats)


pbp <- read_rds("andrew/RAPM/data-and-code-example/data/pbp_1920.rds")

pbp %>% as_tibble()
