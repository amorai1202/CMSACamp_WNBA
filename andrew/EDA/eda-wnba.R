# PURPOSE: Perform EDA On WNBA Dataset


# Load Libraries and Data -------------------------------------------------

library(rvest)
library(tidyverse)

wnba_all_stats <- read_csv("data/wnba_all_stats.csv")

wnba_pos <- wnba_all_stats %>%
  filter(season >= 2018) %>%
  mutate(pos = case_when(
    pos == "C-F" ~ "C",
    pos == "F-C" ~ "F",
    pos == "F-G" ~ "F",
    pos == "G-F" ~ "G",
    TRUE ~ pos
  ))

# Position Distribution

wnba_pos %>%
  filter(season <= 2021) %>%
  group_by(player, pos) %>%
  summarize(n = n()) %>% 
  ggplot(aes(x = pos)) + 
  geom_bar() + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Position",
       y = "Count",
       caption = "WNBA Players Since 2018",
       title = "Guards and Forwards are much more prevalent than Centers in the WNBA")


# Distribution of Minutes Per Game across WNBA Players

wnba_pos %>% 
  filter(season <= 2021) %>%
  mutate(mpg = mp/g) %>%
  ggplot(aes(x = mpg, color = pos, fill = pos)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))  + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(color = "none") + 
  labs(x = "Minutes Per Game",
       y = "Density",
       title = "Guards tend to play more minutes than Forwards and Centers",
       caption = "WNBA player season data since 2018",
       fill = "Position")


### Shot location

wnba_pos %>%
  filter(season <= 2021, g >= 5) %>%
  mutate(mpg = mp/g) %>%
  select(1:4, 46:58, 91) %>% 
  filter(mpg > 5) %>%
  select(1:5, mpg, contains("fga")) %>% 
  na.omit() %>%
  ggplot(aes(x = distance, color = pos, fill = pos)) + 
  stat_ecdf() + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))  + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  labs(x = "Average Shot Distance",
       y = "Proportion of Players",
       title = "A clear chasm exists between the 3 main WNBA positions regarding average shot distance",
       color = "Position")
  
  




player_id <- wnba_all_stats %>%
  separate(link, sep = "/", into = c("A", "B", "C", "D", "E"), remove = FALSE) %>%
  separate(E, sep = "\\.", into = c("id", "F")) %>%
  select(player, link, id)

wnba_all_stats <- wnba_all_stats %>%
  mutate(id = player_id$id)

paste0("https://www.basketball-reference.com/req/202106291/images/wnba-players/", id, ".jpg")


search_target <- wnba_all_stats %>%
  filter(player == "Sue Bird")

typeof(search_target$id[1])



