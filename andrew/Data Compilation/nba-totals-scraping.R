# PURPOSE: Compile NBA totals to add to complete dataset



# Load in libraries -------------------------------------------------------


library(ballr)
library(rvest)
library(tidyverse)


# Write function ----------------------------------------------------------

NBATotalStatistics <- function(season = 2022) {
  nba_url <- paste("https://www.basketball-reference.com/leagues/NBA_",
                    season,
                    "_totals.html",
                    sep = "")
  pg <- rvest::read_html(nba_url)
  
  nba_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  } else {
    nba_stats <- nba_stats %>%
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
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats,
                                 dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link),
                                 list(as.numeric))
  return(nba_stats)
}

# Create totals dataset from 1997 to 2022
nba_total_stats <- tibble()
for (i in 1997:2022) {
  temp <- NBATotalStatistics(i) %>%
    mutate(season = i)
  nba_total_stats <- bind_rows(nba_total_stats, temp)
}

# WRITE CSV
write_csv(nba_total_stats, "data/nba_total_stats.csv")
