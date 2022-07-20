# PURPOSE: Create function to pull raw totals of players for given season from basketball-reference.com WNBA page
# 
# Cont: this will help cluster by player tendencies because we can use total attempts rather than percentages, etc


# Load in data/libraries --------------------------------------------------

library(rvest)
library(tidyverse)

season = 2022

WNBAPerGameStatistics <- function(season = 2022) {
  wnba_url <- paste("https://www.basketball-reference.com/wnba/years/",
                    season,
                    "_per_game.html",
                    sep = "")
  pg <- rvest::read_html(wnba_url)
  
  wnba_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    wnba_stats <- wnba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  } else {
    wnba_stats <- wnba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  
  links <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  link_names <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::tibble(player = as.character(link_names),
                            link   = as.character(links))
  links_df[] <- lapply(links_df, as.character)
  wnba_stats <- dplyr::left_join(wnba_stats, links_df, by = "player")
  wnba_stats <- dplyr::mutate_at(wnba_stats,
                                 dplyr::vars(-.data$player, -.data$pos, -.data$team, -.data$link),
                                 list(as.numeric))
  return(wnba_stats)
}

# Create totals dataset from 2018 to 2022
wnba_pergame_stats <- tibble()
for (i in 2018:2022) {
  temp <- WNBAPerGameStatistics(i) %>%
    mutate(season = i)
  wnba_pergame_stats <- bind_rows(wnba_pergame_stats, temp)
}

# WRITE CSV
write_csv(wnba_pergame_stats, "data/wnba_pergame_stats.csv")
