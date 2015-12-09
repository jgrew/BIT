

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(leaflet)

dashboardPage(
  dashboardHeader(title = 'BIT'),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Fish Response Index", tabName = "fri", icon = icon("dashboard")
    ),
    menuItem(
      "Some Other Tool", tabName = "ot", icon = icon("dashboard")
    ),
    menuItem(
      "About", tabName = 'about', icon = icon('question')
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = 'fri',
            fluidRow(column(
              width = 12,
              tabsetPanel(
                tabPanel(
                  'Map',
                  leafletOutput('map', width = '100%', height = '600px'),
                  absolutePanel(
                    top = 5, right = 15,
                    bsButton('startBut', 'Get Started!', style = 'success')
                  ),
                  bsModal(
                    'modalWelcome', 'Load Data', 'startBut', size = 'large',
                    
                    fluidRow(
                      fluidRow(column(
                        3, fileInput("splist_fn", h4("Fish species list (e.g., FRI.csv):"), accept = ".csv")
                      ),
                      column(7, h4(
                        textOutput("splist_fn_txt")
                      ))),
                      fluidRow(column(
                        3, fileInput("trwlstn_fn", h4("Trawl stations:"), accept = ".csv")
                      ),
                      column(7, h4(
                        textOutput("trwlstn_fn_txt")
                      ))),
                      fluidRow(column(
                        3, fileInput("abun_fn", h4("Fish abundance:"), accept = ".csv")
                      ),
                      column(7, h4(
                        textOutput("abun_fn_txt")
                      )))
                      
                      
                    )
                  )
                ),
                tabPanel('Data Summary'),
                tabPanel('Results')
              )
            ))),
    tabItem(tabName = 'ot',
            box(
              status = "warning", width = NULL,
              "Box content"
            ))
  ))
)
