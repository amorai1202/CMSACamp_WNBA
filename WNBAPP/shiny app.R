#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load packages ------------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)

# Prepare WNBA pergame data table -------------------------------------------------

# Read in WNBA per game stats
wnba_pergame_stats <- read_csv("data/wnba_pergame_stats.csv")

# Create column for real positions (e.g., F-C is simplified to F)

wnba_pergame_stats <- wnba_pergame_stats %>%
    mutate(
        real_pos = case_when(
            pos == "G" ~ "G",
            pos == "F" ~ "F",
            pos == "C" ~ "C",
            pos == "G-F" ~ "G",
            pos == "F-G" ~ "F",
            pos == "F-C" ~ "F",
            pos == "C-F" ~ "C"
        )
    )

# Create player id

player_id <- wnba_pergame_stats %>%
    separate(link, sep = "/", into = c("A", "B", "C", "D", "E"), remove = FALSE) %>%
    separate(E, sep = "\\.", into = c("id", "F")) %>%
    select(player, link, id)

wnba_pergame_stats <- wnba_pergame_stats %>%
    mutate(id = player_id$id)

# Create ACTIVE WNBA players table ----------------------------------------

# filter pergame data for only active players

act_players <- filter(wnba_pergame_stats, season == 2022)

# Remove extra rows for traded WNBA players

traded_players <- act_players %>% filter(team == "TOT")
act_players <- act_players %>% filter(!(player %in% traded_players$player)) %>%
    bind_rows(traded_players)

# Assign teams for traded WNBA players

act_players <- act_players %>%
    mutate(
        team = case_when(
            player == "AD Durr" ~ "ATL",
            player == "Crystal Dangerfield" ~ "NYL",
            player == "Emma Cannon" ~ "IND",
            player == "Evina Westbrook" ~ "WAS",
            player == "Moriah Jefferson" ~ "MIN",
            player == "Reshanda Gray" ~ "PHO",
            player == "Tina Charles" ~ "SEA",
            TRUE ~ team
        )
    )

# Create column for unabbreviated team name

act_players <- act_players %>%
    mutate(
        full_team_name = case_when(
            team == "ATL" ~ "Atlanta Dream",
            team == "CHI" ~ "Chicago Sky",
            team == "CON" ~ "Connecticut Sky",
            team == "DAL" ~ "Dallas Wings",
            team == "IND" ~ "Indiana Fever",
            team == "LVA" ~ "Los Vegas Aces",
            team == "LAS" ~ "Los Angeles Sparks",
            team == "MIN" ~ "Minnesota Lynx",
            team == "NYL" ~ "New York Liberty",
            team == "PHO" ~ "Phoenix Mercury",
            team == "SEA" ~ "Seattle Storm",
            team == "WAS" ~ "Washington Mystics"
        )
    )

# Read in NBA comparison links

comps_links <- read_csv("data/all_comps_nba22.csv")

# Identify qualified players (as determined externally by minimum games/minutes played)

qual_players <- comps_links %>%
    semi_join(act_players, by = c("wnba_player" = "player"))

# Filter act_players for only qualified players

act_players <- act_players %>%
    filter(player %in% qual_players$wnba_player)

# Prepare NBA pergame data table ------------------------------------------

# Read in NBA per game stats

nba_pergame_stats <- read_csv("data/nba_pergame_stats.csv")

# Remove extra rows for traded NBA players

nba_traded_players <- nba_pergame_stats %>% filter(tm == "TOT" & season == 2022)
nba_pergame_stats <- nba_pergame_stats %>% filter(season == 2022,
                                                  !(player %in% nba_traded_players$player)) %>%
    bind_rows(nba_traded_players)


# Prepare WNBA archetype data table ---------------------------------------

# Read in archetype data

wnba_archetypes <- read_csv("data/wnba_archetypes.csv")

# Filter for players' most recent season

wnba_archetypes <- wnba_archetypes %>%
    group_by(player) %>%
    filter(season == max(season))


# Shiny app code ----------------------------------------------------------

ui <- fluidPage(
    
    theme = bs_theme(bootswatch = "minty"),
    
    fluidRow(
        
        column(6,
               h2("WNBA Player Profiles")),
        
        column(4, offset = 2,
               selectizeInput(
                   inputId = "searchme",
                   label = "Search",
                   multiple = FALSE,
                   choices = c("Search Players" = "", unique(act_players$player)),
                   options = list(
                       create = FALSE,
                       placeholder = "Search Players",
                       maxItems = '1',
                       onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}")
                   )
               ))
        
    ),
    
    fluidRow(
        column(2,
               uiOutput("wnba_player_image")),
               
        
        column(5,
               h2(textOutput("searched_player")),
                  
                  fluidRow(
                      column(6,
                             h4(textOutput("team_name"))),
                      
                      column(6,
                             h4(textOutput("real_pos"))),
                      
                  )
        ),
        
        column(5,
               
               fluidRow(
                   column(12, align = "center",
                          h6("Player Archetype:"),
                          
                          hr(),
                          
                          fluidRow(
                              column(12,
                                     h3(tags$b(textOutput("player_arch"), style = "color:orange")))
                          ))
                   
               
    ))),
    
    fluidRow(
        
        column(12, align = "center",
               h4("2022 SEASON STATS"))
        
    ),
    
    fluidRow(
        
        column(3, align = "center",
               h5("PTS")),
        
        column(3, align = "center",
               h5("REB")),
        
        column(3, align = "center",
               h5("AST")),
        
        column(3, align = "center",
               h5("STL"))
    ),
    
    fluidRow(
        
        column(3, align = "center",
               h3(textOutput("player_pts_pg"))),
        
        column(3, align = "center",
               h3(textOutput("player_trb_pg"))),
        
        column(3, align = "center",
               h3(textOutput("player_ast_pg"))),
        
        column(3, align = "center",
               h3(textOutput("player_stl_pg"))),
        
    ),
    
    hr(),
    
    fluidRow(
       
        column(3, align = "center",
               h4(textOutput("nba_comp1_name"))),
        
        column(3, align = "center",
               h4(textOutput("nba_comp2_name"))),
        
        column(3, align = "center",
               h4(textOutput("nba_comp3_name"))),
        
        column(3, align = "center",
               h4(textOutput("nba_comp4_name")))
        
    ),
    
    fluidRow(
        
        column(3, align = "center",
               uiOutput("nba1_image"),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp1_ppg"))),
                   column(3,
                          h6("PTS")),
                   column(3,
                          h4(textOutput("nba_comp1_trbpg"))),
                   column(3,
                          h6("REB"))
               ),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp1_astpg"))),
                   column(3,
                          h6("AST")),
                   column(3,
                          h4(textOutput("nba_comp1_stlpg"))),
                   column(3,
                          h6("STL"))
               ), 
               
               style = "border-right: 1px solid"),
        
        column(3, align = "center",
               uiOutput("nba2_image"),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp2_ppg"))),
                   column(3,
                          h6("PTS")),
                   column(3,
                          h4(textOutput("nba_comp2_trbpg"))),
                   column(3,
                          h6("REB"))
               ),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp2_astpg"))),
                   column(3,
                          h6("AST")),
                   column(3,
                          h4(textOutput("nba_comp2_stlpg"))),
                   column(3,
                          h6("STL"))
               ), 
               
               style = "border-right: 1px solid"),
        
        column(3, align = "center",
               uiOutput("nba3_image"),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp3_ppg"))),
                   column(3,
                          h6("PTS")),
                   column(3,
                          h4(textOutput("nba_comp3_trbpg"))),
                   column(3,
                          h6("REB"))
               ),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp3_astpg"))),
                   column(3,
                          h6("AST")),
                   column(3,
                          h4(textOutput("nba_comp3_stlpg"))),
                   column(3,
                          h6("STL"))
               ),
               
               style = "border-right: 1px solid"),
        
        column(3, align = "center",
               uiOutput("nba4_image"),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp4_ppg"))),
                   column(3,
                          h6("PTS")),
                   column(3,
                          h4(textOutput("nba_comp4_trbpg"))),
                   column(3,
                          h6("REB"))
               ),
               
               fluidRow(
                   column(3,
                          h4(textOutput("nba_comp4_astpg"))),
                   column(3,
                          h6("AST")),
                   column(3,
                          h4(textOutput("nba_comp4_stlpg"))),
                   column(3,
                          h6("STL"))
               )
        )
    
))


server <- function(input, output, session) {
    
    
    # Show player name
    output$searched_player <- renderText({
        paste(input$searchme)
    })
    
    # Show current team name
    
    output$team_name <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        unique(search_target$full_team_name)
    })
    
    #Show player position
    
    output$real_pos <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        search_target$real_pos
        
    })
    
    
    # Show searched player image
    
    output$wnba_player_image <- renderUI({
        
        search_target <- wnba_pergame_stats %>%
            filter(player == input$searchme)
        
        imgurl <- paste0("https://www.basketball-reference.com/req/202106291/images/wnba-players/", search_target$id[1], ".jpg")
        
        tags$img(src=imgurl, width = 120, height = 180)
        
    })
    
    # Show WNBA player's archetype
    
    output$player_arch <- renderText({
        
        search_target <- wnba_archetypes %>%
            filter(player == input$searchme)
        
        search_target$cluster
        
    })
    
    # Show player's WNBA 2022 season stats
    
    output$player_pts_pg <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        search_target$pts
        
    })
    
    output$player_trb_pg <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        search_target$trb
        
    })
    
    output$player_ast_pg <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        search_target$ast
        
    })
    
    output$player_stl_pg <- renderText({
        
        search_target <- act_players %>%
            filter(player == input$searchme)
        
        search_target$stl
        
    })
    
    # Show NBA comparisons' stats
    
        # 1st comp
    
    output$nba_comp1_ppg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player1 <- search_target$nba_player1[1]
        
        nba_player1_pts <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player1) %>%
            pull(pts)
        
        nba_player1_pts
        
    })
    
    output$nba_comp1_trbpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player1 <- search_target$nba_player1[1]
        
        nba_player1_trbpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player1) %>%
            pull(trb)
        
        nba_player1_trbpg
        
    })
    
    output$nba_comp1_astpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player1 <- search_target$nba_player1[1]
        
        nba_player1_astpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player1) %>%
            pull(ast)
        
        nba_player1_astpg
        
    })
    
    output$nba_comp1_stlpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player1 <- search_target$nba_player1[1]
        
        nba_player1_stlpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player1) %>%
            pull(stl)
        
        nba_player1_stlpg
        
    })
    
        # 2nd comp
    
    output$nba_comp2_ppg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player2 <- search_target$nba_player2[1]
        
        nba_player2_pts <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player2) %>%
            pull(pts)
        
        nba_player2_pts
        
    })
    
    output$nba_comp2_trbpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player2 <- search_target$nba_player2[1]
        
        nba_player2_trbpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player2) %>%
            pull(trb)
        
        nba_player2_trbpg
        
    })
    
    output$nba_comp2_astpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player2 <- search_target$nba_player2[1]
        
        nba_player2_astpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player2) %>%
            pull(ast)
        
        nba_player2_astpg
        
    })
    
    output$nba_comp2_stlpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player2 <- search_target$nba_player2[1]
        
        nba_player2_stlpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player2) %>%
            pull(stl)
        
        nba_player2_stlpg
        
    })
    
        # 3rd comp
    
    output$nba_comp3_ppg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player3 <- search_target$nba_player3[1]
        
        nba_player3_pts <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player3) %>%
            pull(pts)
        
        nba_player3_pts
        
    })
    
    output$nba_comp3_trbpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player3 <- search_target$nba_player3[1]
        
        nba_player3_trbpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player3) %>%
            pull(trb)
        
        nba_player3_trbpg
        
    })
    
    output$nba_comp3_astpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player3 <- search_target$nba_player3[1]
        
        nba_player3_astpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player3) %>%
            pull(ast)
        
        nba_player3_astpg
        
    })
    
    output$nba_comp3_stlpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player3 <- search_target$nba_player3[1]
        
        nba_player3_stlpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player3) %>%
            pull(stl)
        
        nba_player3_stlpg
        
    })
    
    
        # 4th comp
    
    output$nba_comp4_ppg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player4 <- search_target$nba_player4[1]
        
        nba_player4_pts <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player4) %>%
            pull(pts)
        
        nba_player4_pts
        
    })
    
    output$nba_comp4_trbpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player4 <- search_target$nba_player4[1]
        
        nba_player4_trbpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player4) %>%
            pull(trb)
        
        nba_player4_trbpg
        
    })
    
    output$nba_comp4_astpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player4 <- search_target$nba_player4[1]
        
        nba_player4_astpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player4) %>%
            pull(ast)
        
        nba_player4_astpg
        
    })
    
    output$nba_comp4_stlpg <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        nba_player4 <- search_target$nba_player4[1]
        
        nba_player4_stlpg <- nba_pergame_stats %>%
            filter(season == 2022,
                   player == nba_player4) %>%
            pull(stl)
        
        nba_player4_stlpg
        
    })
    
    # Show player comparison images
    
    output$nba1_image <- renderUI({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        imgurl <- paste0("https://www.basketball-reference.com/req/202106291/images/players/", search_target$nba_id1[1], ".jpg")
        
        tags$img(src=imgurl, width = 120, height = 180)
        
    })
    
    output$nba2_image <- renderUI({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        imgurl <- paste0("https://www.basketball-reference.com/req/202106291/images/players/", search_target$nba_id2[1], ".jpg")
        
        tags$img(src=imgurl, width = 120, height = 180)
        
    })
    
    output$nba3_image <- renderUI({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        imgurl <- paste0("https://www.basketball-reference.com/req/202106291/images/players/", search_target$nba_id3[1], ".jpg")
        
        tags$img(src=imgurl, width = 120, height = 180)
        
    })
    
    output$nba4_image <- renderUI({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        imgurl <- paste0("https://www.basketball-reference.com/req/202106291/images/players/", search_target$nba_id4[1], ".jpg")
        
        tags$img(src=imgurl, width = 120, height = 180)
        
    })
    
    # Show player comparison names
    
    output$nba_comp1_name <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        search_target$nba_player1
        
    })
    
    output$nba_comp2_name <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        search_target$nba_player2
        
    })
    
    output$nba_comp3_name <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        search_target$nba_player3
        
    })
    
    output$nba_comp4_name <- renderText({
        
        search_target <- comps_links %>%
            filter(wnba_player == input$searchme)
        
        search_target$nba_player4
        
    })
    
    # Show Selected Value in Console
    observe({
        print(input$searchme)
    })
    
}

shinyApp(ui = ui, server = server)

















