
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(shinyBS)
library(leaflet)

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17)
  })
})
