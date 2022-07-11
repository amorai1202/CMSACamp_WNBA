# PURPOSE: Creating cumulative NBA datasets with ballr Package



# Load in libraries -------------------------------------------------------

library(ballr)
library(tidyverse)



# Create Advanced Stats Dataset 1997-2022 ---------------------------------

# ballr::NBAPerGameAdvStatistics(season = 2022) #This is way easier


nba_adv_stats <- tibble()
for (i in 1997:2022){
  temp <- ballr::NBAPerGameAdvStatistics(season = i) %>%
    mutate(season = i)
  nba_adv_stats <- bind_rows(nba_adv_stats, temp)
}

# Remove filler columns
nba_adv_stats <- nba_adv_stats %>%
  dplyr::select(-c(x, x_2, rk))


# Write adv_stats to csv
# write_csv(nba_adv_stats, "data/nba_adv_stats.csv")


# Create per 100 poss Dataset 1997-2022 -----------------------------------

ballr::NBAPerGameStatisticsPer100Poss(season = 2018)


nba_per100_stats <- tibble()
for (i in 1997:2022){
  temp <- ballr::NBAPerGameStatisticsPer100Poss(season = i) %>%
    mutate(season = i)
  nba_per100_stats <- bind_rows(nba_per100_stats, temp)
}

# Remove filler columns
nba_per100_stats <- nba_per100_stats %>%
  dplyr::select(-c(x, rk))

# Write csv
# write_csv(nba_per100_stats, "data/nba_per100_stats.csv")





# Writing a function to grab shooting data --------------------------------

NBAPerGameShooting <- function(season = 2018) {
  nba_url <- paste(getOption("NBA_api_base"),
                   "/leagues/NBA_",
                   season,
                   "_shooting.html",
                   sep = "")
  pg <- xml2::read_html(nba_url)
  
  nba_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$x_2 != "Player") %>%
      rename(rk = 1,
             player = 2,
             pos = 3,
             age = 4,
             tm = 5, 
             g = 6,
             mp = 7,
             fgpercent = 8,
             distance = 9,
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
             dunks_attempt = 28,
             dunks_total = 29,
             corner_3s_attempt = 31,
             corner_3s_percent = 32,
             heaves_attempts = 34,
             heaves_makes = 35
             )
      
  } else {
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$x_2 != "Player") %>%
      rename(rk = 1,
             player = 2,
             pos = 3,
             age = 4,
             tm = 5, 
             g = 6,
             mp = 7,
             fgpercent = 8,
             distance = 9,
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
             dunks_attempt = 28,
             dunks_total = 29,
             corner_3s_attempt = 31,
             corner_3s_percent = 32,
             heaves_attempts = 34,
             heaves_makes = 35
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
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats,
                                dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link),
                                dplyr::funs(as.numeric))
  return(nba_stats)
}
NBAPerGameShooting()

nba_shooting <- tibble()
for (i in 1997:2022){
  temp <- NBAPerGameShooting(season = i) %>%
    mutate(season = i)
  nba_shooting <- bind_rows(nba_shooting, temp)
}

# Get rid of filler NA columns from html formatting
nba_shooting <- nba_shooting %>%
  select(-starts_with("x"))

# Write csv
# write_csv(nba_shooting, "data/nba_shooting.csv")



# Function to pull PBP data -----------------------------------------------

NBAPlayByPlay <- function(season = 2018) {
  nba_url <- paste(getOption("NBA_api_base"),
                   "/leagues/NBA_",
                    2018,
                    "_play-by-play.html",
                    sep = "")
  pg <- xml2::read_html(nba_url)
  
  nba_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      janitor::remove_empty("cols") %>%
      dplyr::filter(.data$x_2 != "Player") 
    
  } else {
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty("cols") %>%
      dplyr::filter(.data$x_2 != "Player") 
  }
  
  #Grabbing column names from html table  
  
  vec <-  pg %>%
    rvest::html_nodes(xpath = '//*[@id="pbp_stats"]/thead/tr[2]') %>%
    rvest::html_text2() %>%
    str_split(pattern = '\t')
  
  vec <- vec[[1]]
  
  col_names <- vec[vec!=" " & vec!= ""]
  
  #Renaming columns to avoid duplicates and messy col names
  
  col_names[14] <- "On_Off"
  col_names[17] <- "Shoot_fouls_committed"
  col_names[18] <- "Off_fouls_committed"
  col_names[19] <- "Shoot_fouls_drawn"
  col_names[20] <- "Off_fouls_drawn"
  
  # Assigning lower case column names to match other formats
  colnames(nba_stats) <- str_to_lower(col_names)
  
  # Grabbing player links
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
                                 dplyr::funs(as.numeric))
  return(nba_stats)
}


nba_pbp <- tibble()
for (i in 1997:2022){
  temp <- NBAPlayByPlay(season = i) %>%
    mutate(season = i)
  nba_pbp <- bind_rows(nba_pbp, temp)
}

# Write csv
# write_csv(nba_pbp, "data/nba_pbp.csv")
