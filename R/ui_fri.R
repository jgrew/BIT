# ui.R - Shiny_FRI2 ####

shinyUI(fluidPage( title = "Fish Response Index - v.1.00",
  # Application title
  titlePanel("Fish Response Index Analysis"),
    fluidRow(
      tabsetPanel(
        tabPanel( h4( "Select Data files"),
          fluidRow( 
            column( 3, fileInput( "splist_fn", h4("Fish species list (e.g., FRI.csv):"), accept = ".csv" ) ),
            column( 7, h4( textOutput( "splist_fn_txt" ) ) ),
            hr()
          ),
          fluidRow( 
            column( 3, fileInput( "trwlstn_fn", h4("Trawl stations:"), accept = ".csv" ) ),
            column( 7, h4( textOutput( "trwlstn_fn_txt" ) ) ),
            hr()
          ),
          fluidRow( 
            column( 3, fileInput( "abun_fn", h4("Fish abundance:"), accept = ".csv" ) ),
            column( 7, h4( textOutput( "abun_fn_txt" ) ) ),
            hr()
          )
        ),
      tabPanel( h4( "FRI analysis" ),
        actionButton( "FRI_calc", "Calculate" ),
        br(), 
        h5( htmlOutput( "FRI_summary" ) ),
        br(),
        downloadButton( "download_txt_report", "Save Results" ),
        tableOutput( "FRI_results_table" )
      ),
      tabPanel( h4( "Map" ),
        fluidRow( 
          column( 2, actionButton( "Show_map", h4("Show map" ) ) ),
          column( 3, selectInput( 
            "select_Region", label = h4("Show region"), 
             choices = c(  "Southern California Bight",
                           "Los Angeles & Long Beach Harbors",
                           "San Diego Bay" ), 
             selected = "Southern California Bight" ) ),
          column( 3, selectInput( 
            "selectMapStyle", label = h4("Map style"),
             choices = c( "Land/Sea + MPAs",
                          "Google terrain",
                          "Google satellite",
                          "Google roadmap" ), 
             selected = "Land/Sea + MPAs" ) )
        ),
        hr(),
        plotOutput("MapPlot")        
      ),
      tabPanel( h4( "Google Map" ),
        fluidRow( 
          actionButton( "showGoogle_map", h4( "Google map" ) ),
#                  textOutput( "text1" ),
                  uiOutput( "Google_map" )
        )
      )
    )
  )
))

