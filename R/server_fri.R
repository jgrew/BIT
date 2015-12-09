# server.R - Shiny_FRI2 ####

library(shiny)
library(plyr)
library(plotrix)
library(PBSmapping)
#require(rgdal) ## to read KML file
library(maptools)
library(ggmap)
library( plotGoogleMaps )

proj_dir = "/srv/shiny-server/FRI_analysis"
setwd( proj_dir )
#splist_FN <- file.path( proj_dir, "Data", "FRI.csv" )
#trwlstn_FN <- file.path( proj_dir, "Data", "B13_AreaWeights_TrawlStations.csv" )
#abun_FN <- file.path( proj_dir, "Data", "TrawlFishAbundance_10_Nov_2014.csv" )

load( file.path( proj_dir, "MapData", "SCB.RData" ) ) ###

funct_path <- file.path( proj_dir, "FRI_functions.R" )
source( funct_path )

shinyServer(function(input, output) {
  # Global variables ####
  splist_file <- NULL
  trwlstn_file <- NULL
  abun_file <- NULL
  FRI_summary_txt <- paste( " ")
  makeReactiveBinding( "FRI_summary_txt" )
  FRI_locations <- NULL
  FRI_locations2 <- NULL
  #makeReactiveBinding( "FRI_locations" )
  FRI_results <- NULL
  makeReactiveBinding( "FRI_results" )
  mapStyle <- NULL
  mapRegion <- NULL
  map.plt <- NULL
  map.mgp <- NULL
  map.cex.axis <- NULL
  map.addPolys <- TRUE
  map.legend.pos <- NULL
  POLALB <- NULL
  SDB <- NULL
  map.c_lon <- NULL
  map.c_lat <- NULL
  map.c_zoom <- NULL
  text1 <- "Help..."
  makeReactiveBinding( "text1" )
  #
  # Input three files ####
  output$splist_fn_txt <- renderText({
    splist_file <<- input$splist_fn
    return( splist_file$name )
  })
  #
  output$trwlstn_fn_txt <- renderText({
    trwlstn_file <<- input$trwlstn_fn
    return( trwlstn_file$name )
  })
  #
  output$abun_fn_txt <- renderText({
    abun_file <<- input$abun_fn
    return( abun_file$name )
  })
  # Calculate ####
  observe({
    if( input$FRI_calc > 0 ) {
      isolate({
        if( !is.null(splist_file) & !is.null(trwlstn_file) & !is.null(abun_file) ) {
          FRI <- read.csv( splist_file$datapath )
          subpops <- read.csv( trwlstn_file$datapath )
          abun <- read.csv( abun_file$datapath )
          #FRI <- read.csv( splist_FN )  #############
          #subpops <- read.csv( trwlstn_FN ) #############
          #abun <- read.csv( abun_FN ) #################
          FRI_locations <<- FRI_calc( FRI, subpops, abun ) 
          FRI_locations2 <<- FRI_loc2_calc( FRI_locations )
          FRI_results <<- FRI_loc2res( FRI_locations )
        }
      })
    }
  })
  observe({
    if( input$FRI_calc > 0 ) {
      #text1 <<- "FRI_calc"
      FRI_summary_txt <<- isolate(
        if( is.null( FRI_results ) )
          paste( "Select the files first" ) else {
            
            n_R <- sum(FRI_results$FRI == "Reference" )
            n_NR <- sum(FRI_results$FRI == "Non-Reference" )
            FRI_summary_txt <- HTML( paste( 
              paste( n_R, "Reference stations" ),
              paste( n_NR, "Non-Reference stations" ), 
              sep = "<br/>", collapse = "" ) )
          }
      )
    }
  })
  output$FRI_summary <- renderText( FRI_summary_txt )
  #
  output$download_txt_report <- downloadHandler(
    filename = function() {
      paste( "FRI_results.txt" )
    },
    content = function( file ) {
      Report_str <- data.frame( FRI_results )
      write.table( Report_str, 
                   file = file, eol = "\r\n", quote = FALSE, 
                   row.names = FALSE, col.names = TRUE )
    },
    contentType = "text/csv"
  )
  #
  observe({
    switch( input$selectMapStyle, 
            "Land/Sea + MPAs" = { mapStyle <<- "Land/Sea" },
            "Google terrain" = { mapStyle <<- "terrain" },
            "Google satellite" = { mapStyle <<- "satellite" },
            "Google roadmap" = { mapStyle <<- "roadmap" }
    )
  })
  #
  observe({
    switch( input$select_Region,
      "Southern California Bight" = 
      { mapRegion <<- "SCB"
        x_lim <<- c(-120.7,-117)
        y_lim <<- c(32.4,34.6)
        map.plt <<- c(0.05,0.995,0.05,0.995)
        map.mgp <<- c(3, .75, 0)
        map.cex.axis <<- 1.75
        map.addPolys <<- TRUE
        map.legend.pos <<- "bottomleft"
        POLALB <<- data.frame(PID=rep(1,4), POS=1:4, X=c(-118.3,-118.1,-118.1,-118.3), 
                        Y=c(33.69,33.69,33.77,33.77))
        POLALB <<- as.PolySet(POLALB, projection="LL")
        SDB <<- data.frame(PID=rep(1,4), POS=1:4, X=c(-117.37,-117.08,-117.08,-117.37), 
                     Y=c(32.54,32.54,32.799,32.799))
        SDB <<- as.PolySet(SDB, projection="LL")
        map.c_lon <<- -118.7
        map.c_lat <<- 33.3
        map.c_zoom <<- 8
      },
      "Los Angeles & Long Beach Harbors" = 
      { mapRegion <<- "LALB" 
        x_lim <<- c(-118.3,-118.1)
        y_lim <<- c(33.69,33.77)  
        map.plt <<- c(0.11, 0.98, 0.12, 0.88)
        map.mgp <<- c(3, 0.2, 0)
        map.cex.axis <<- 0.8
        map.addPolys <<- FALSE
        map.legend.pos <<- "topleft"
        POLALB <<- NULL
        SDB <<- NULL
        map.c_lon <<- -118.2
        map.c_lat <<- 33.72
        map.c_zoom <<- 12
      },
      "San Diego Bay" = 
      { mapRegion <<- "SDB" 
        x_lim <<- c(-117.37,-117.08)
        y_lim <<- c(32.54,32.799)  
        map.plt <<- c(0.11, 0.98, 0.12, 0.88)
        map.mgp <<- c(3, 0.2, 0)
        map.cex.axis <<- 0.8
        map.addPolys <<- TRUE
        map.legend.pos <<- "topright"
        POLALB <<- NULL
        SDB <<- NULL
        map.c_lon <<- -117.2
        map.c_lat <<- 32.65
        map.c_zoom <<- 11
      }      
    )
  })
  #
  observe({
    if( input$Show_map > 0 ) {
      output$MapPlot <- isolate({
        MapPlot <- renderPlot( {
          if( !is.null( FRI_results ) ) {
            if( mapStyle == "Land/Sea" ) {
              plotMap( SCB, xlim = x_lim, ylim = y_lim, 
                     col = "lightgrey", bg = "lightblue", lwd = 1, xlab = "", ylab = "", 
                     plt = map.plt, mgp = map.mgp, cex.axis = map.cex.axis )
              addLines( alPoly, col=icol ) 
              if( map.addPolys ) {
                addPolys( MPAs.LL, lwd = 1, col = rgb(blue=1,red=1,green=1,alpha=.5) )
              }
              if( !is.null( FRI_locations ) ) {
                addPoints( FRI_locations,  polyProps = FRI_locations )
              }
              if( !is.null( FRI_locations2 ) ) {
                addPoints( FRI_locations2,  polyProps = FRI_locations2 )
              }
              legend( map.legend.pos, title="Fish Response Index", legend=c("Reference", "Non-Reference", "MPA"),
                    inset=0.05, cex=0.9, y.intersp=1.1,
                    pch=c(21,21,22), # different symbols
                    pt.bg=c("green", "red", "#cee7ef"),
                    pt.cex=c(1,1,2),
                    col=c("black", "black", "black"))
              addLabels( data=geo_labels_main, col="black", cex=.6)
              if( !is.null( POLALB ) ) {
                addPolys(POLALB, lwd=1.5, border="white", density=0)
              }
              if( !is.null( SDB ) ) {
                addPolys(SDB, lwd=1.5, border="white", density=0)
              }
            
            } else {  # Google "terrain", "satellite" or "roadmap"
              mapImageData1 <- get_map(location = c(lon = map.c_lon, lat = map.c_lat ),
                                     color = "color", source = "google", maptype = mapStyle, 
                                     zoom = map.c_zoom )
              disp_map <- ggmap(mapImageData1, extent = "device",
                              ylab = "Latitude", xlab = "Longitude", 
                              legend = map.legend.pos ) 
              disp_map <- disp_map + 
                geom_point( data = FRI_locations,
                          mapping = aes( x = X, y = Y, fill = col ), 
                          shape = 21, size = 4 ) +
                scale_shape_identity() +
                scale_fill_manual( 
                  values = c( "green" = "#00FF00", "red" = "#FF0000" ), 
                  labels = c("Reference","Non-Reference") ) +
                guides( fill = guide_legend( title = "Fish Response Index" ) ) +
                theme( legend.background = element_rect( fill = "white", colour = "black" ),
                     legend.margin = grid::unit(2,"cm"),
                     legend.key = element_rect("white"),
                     legend.key.size = grid::unit(0.5,"cm"),
                     legend.text = element_text(colour = "black" ),
                     legend.text.align = 0,
                     legend.title = element_text(colour = "black" ),
                     legend.title.align = 0.5,
                     legend.position = c( 0, 0 ),
                     legend.direction = "vertical" ) +
                geom_rect( xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5, fill = "yellow" )
              print( disp_map )
            }                      
          }
        })
      })
    } 
  })
  #
  output$Google_map <- renderUI({
#  observe({
    if( input$showGoogle_map > 0 ) {
      isolate({
        text1 <<- "Google map"
        FRI_locs <- as.data.frame( FRI_locations )
        FRI_locs$col[FRI_locs$col=="green"]<-"Reference"
        FRI_locs$col[FRI_locs$col=="red"]<-"Non-Reference"
        FRI_locs$pch <- NULL
        FRI_locs$cex <- NULL
        coordinates( FRI_locs )<-~X+Y # convert to SPDF
        proj4string( FRI_locs ) <- CRS("+proj=longlat")
        text1 <<- paste( "proj4string", names(FRI_locs) )
        ic <- iconlabels( FRI_locs$col, height = 1, 
                        colPalette = c( "#FF0000", "#00FF00" ),
                        icon = TRUE, scale = 0.6 )
        text1 <<- paste( "iconlabels", names(FRI_locs) )
        m <- plotGoogleMaps( FRI_locs, filename = "myMap1.html",
                           zcol = "col", legend = FALSE, 
                           control = FALSE, iconMarker = ic,
                           colPalette = c( "#FF0000", "#00FF00" ),
                           control.width="100%", control.height="1%" )
        #
        tags$iframe(
          srcdoc = paste(readLines( "myMap1.html"), 
                         collapse = '\n'),
          width = "90%",
          height = "600px"
        )
#        
      })
    }
  })
  #
  observe({
    output$FRI_results_table <- renderTable({
      if( !is.null( FRI_results ) ) {
        #FRI_results_table <- data.frame( XX=c("aaa","bbb"),YY=c(1,4))
        FRI_results_table <- as.data.frame( FRI_results )
      }
    })
  })
  #
  observe({
    output$text1 <- renderText({
      text1 <- text1
    })
  })
  #
  output$mymap <- renderUI({
    tags$iframe(
      srcdoc = paste(readLines( "myMap1.html"), 
                     collapse = '\n'),
      width = "90%",
      height = "600px"
    )
  })
  
  #
})
