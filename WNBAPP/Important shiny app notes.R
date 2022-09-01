
library(shiny)
library(tidyverse)
runApp("WNBAPP")

read_csv("data/")


library(rvest)
test_url <- "https://www.basketball-reference.com/players/d/duranke01.html"
test_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="meta"]/div[2]/p[4]/span[1]') %>%
  html_text()

theme = bs_theme(bootswatch = "minty"),

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


