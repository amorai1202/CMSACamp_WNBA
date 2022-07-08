# PURPOSE: Rework ballr functions to fit wnba basketball reference



# Load in data ------------------------------------------------------------

library(ballr)
library(tidyverse)



# Remaking ballr function for per 100 stats -------------------------------

WNBAPerGameStatisticsPer100Poss <- function(season = 2018) {
  wnba_url <- paste("https://www.basketball-reference.com/wnba/years/",
                   season,
                   "_per_poss.html",
                   sep = "")
  pg <- xml2::read_html(wnba_url)
  
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
                                dplyr::funs(as.numeric))
  return(wnba_stats)
}

#Testing wnba function
WNBAPerGameStatisticsPer100Poss(2021)

# Compiling per 100 poss stats from 1997-2022
wnba_per100_stats <- tibble()
for (i in 1997:2022){
  temp <- WNBAPerGameStatisticsPer100Poss(season = i) %>%
    mutate(season = i)
  wnba_per100_stats <- bind_rows(wnba_per100_stats, temp)
}

# Cleaning to make per100 dataset to match nba dataset

wnba_per100_stats <- wnba_per100_stats %>%
  rename(tm = "team")

# Write to csv

# write_csv(wnba_per100_stats, "data/wnba_per100_stats.csv")


### Rewriting ballr function for advanced wnba stats ------------------------

WNBAPerGameAdvStatistics <- function(season = 2018) {
  wnba_url <- paste("https://www.basketball-reference.com/wnba/years/",
                   season,
                   "_advanced.html",
                   sep = "")
  pg <- xml2::read_html(wnba_url)
  
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
                                dplyr::funs(as.numeric))
  return(wnba_stats)
}

# Trial
WNBAPerGameAdvStatistics(2021)


# Compiling adv stats from 1997-2022

wnba_adv_stats <- tibble()
for (i in 1997:2022){
  temp <- WNBAPerGameAdvStatistics(season = i) %>%
    mutate(season = i)
  wnba_adv_stats <- bind_rows(wnba_adv_stats, temp)
}

# Brief cleaning to lineup with nba datasets

wnba_adv_stats <- wnba_adv_stats %>%
  dplyr::select(-x) %>%
  rename(tm = "team")

# Write to csv

# write_csv(wnba_adv_stats, "data/wnba_adv_stats.csv")



# Reworking ballr function to get shooting tendencies -------------------------

WNBAPerGameShooting <- function(season = 2018) {
  wnba_url <- paste("https://www.basketball-reference.com/wnba/years/",
                    season,
                    "_shooting.html",
                    sep = "")
  pg <- xml2::read_html(wnba_url)
  
  wnba_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    wnba_stats <- wnba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$x != "Player") %>%
      #Renaming columns b/c shooting page doesn't automatically grab column names like other functions
      rename(player = "x",
             pos = "x_3",
             team = "x_2",
             g = "x_4",
             mp = "x_5", 
             g_2 = "x_6",
             mp_2 = "x_7",
             fgpercent = "x_8",
             distance = "x_9",
             fga_2p = 11,
             fga_0_3 = 12,
             fga_3_10 = 13,
             fga_10_16 = 14,
             fga_16_3p = 15,
             fga_3p = 16,
             fg_2p = 18,
             fg_0_3 = 19,
             fg_3_10 = 20,
             fg_10_16 = 21,
             fg_16_3p = 22,
             fg_3p = 23,
             fg_2p_asst = 25,
             fg_3p_asst = 26,
             corner_3s_attempt = 28,
             corner_3s_percent = 29
             )
  } else {
    wnba_stats <- wnba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$x != "Player") %>%
      rename(player = "x",
             pos = "x_3",
             team = "x_2",
             g = "x_4",
             mp = "x_5", 
             g_2 = "x_6",
             mp_2 = "x_7",
             fgpercent = "x_8",
             distance = "x_9",
             fga_2p = 11,
             fga_0_3 = 12,
             fga_3_10 = 13,
             fga_10_16 = 14,
             fga_16_3p = 15,
             fga_3p = 16,
             fg_2p = 18,
             fg_0_3 = 19,
             fg_3_10 = 20,
             fg_10_16 = 21,
             fg_16_3p = 22,
             fg_3p = 23,
             fg_2p_asst = 25,
             fg_3p_asst = 26,
             corner_3s_attempt = 28,
             corner_3s_percent = 29
      )
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
                                 dplyr::funs(as.numeric))
  return(wnba_stats)
}

WNBAPerGameShooting()

# Compiling all available shooting data
# Only goes back to 2018
wnba_shooting <- tibble()

for (i in 2018:2022){
  temp <- WNBAPerGameShooting(season = i) %>%
    mutate(season = i)
  wnba_shooting <- bind_rows(wnba_shooting, temp)
}

# Brief cleaning to lineup with nba datasets

wnba_shooting <- wnba_shooting %>%
  rename(tm = "team")

colnames(wnba_shooting)

wnba_shooting <- wnba_shooting %>%
  select(-starts_with("x"))


# Write csv
# write_csv(wnba_shooting, "data/wnba_shooting.csv")
