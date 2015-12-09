# FRI_functions.R ####
# FRI_calc ####
FRI_calc <- function( FRI, subpops, abun ) {
  # switch appropriate strings from integers to factors: dat$stringname<-as.factor(dat$stringname)
  abun$StationID<-as.factor(abun$StationID)
  abun$SampleID<-as.factor(abun$SampleID)
  subpops$StationID<-as.factor(subpops$StationID)
  # adjust meta info to correct shelfzones; add MPA column
  subpops$Bight13_Stratum_Actual <- as.factor(subpops$Bight13_Stratum_Actual)
  subpops$ShelfZone <- as.factor(ifelse(subpops$Bight13_Stratum_Actual %in% c("Bay", "Marina"), 
                                        "Bays & Harbors", as.character(subpops$Bight13_Stratum_Actual)))
  subpops$ShelfZone <- as.factor(ifelse(subpops$ShelfZone=="MPA", as.character(subpops$Bight_Stratum_Actual2), as.character(subpops$ShelfZone)))
  # add response exponent data
  subpops$FRI_F1 <- ifelse(subpops$StartDepth > 215, NA, ifelse(subpops$StartDepth > 120, .5, ifelse(subpops$StartDepth > 40, .25, 0)))
  subpops$FRI_F2 <- ifelse(subpops$StartDepth > 215, NA, ifelse(subpops$StartDepth > 100, .5, ifelse(subpops$StartDepth > 30, .25, 0)))
  # match trawl metadata to records
  abun$ShelfZone <- subpops$ShelfZone[match(abun$StationID, subpops$StationID)]
  abun$EvalCode <- subpops$Eval_Code[match(abun$StationID, subpops$StationID)]
  abun$FRI_F1 <- subpops$FRI_F1[match(abun$StationID, subpops$StationID)]
  abun$FRI_F2 <- subpops$FRI_F2[match(abun$StationID, subpops$StationID)]
  # match FRI values to species/depth combos
  abun$spF1 <- paste(abun$Species, abun$FRI_F1)
  abun$spF2 <- paste(abun$Species, abun$FRI_F2)
  FRI$spF <- paste(FRI$Species, FRI$F_val)
  abun$FRI_P1 <- FRI$P_i[match(abun$spF1, FRI$spF)]
  abun$FRI_P2 <- FRI$P_i[match(abun$spF2, FRI$spF)]
  abun$spF1 <- NULL
  abun$spF2 <- NULL
  ### IF REMOVING EXTRA STATIONS:
  abun <- abun[!is.na(abun$EvalCode),]
  abun <- abun[!abun$EvalCode=="NP_TS",]
  # drop levels
  abun <- droplevels(abun)
  ####################################
  # FRI Values
  ####################################
  # remove Upper Slope strata
  abun <- abun[abun$ShelfZone!="Upper Slope",]
  # create table summarizing fish abundance by species and station
  FRI_main <- ddply(abun, c("StationID", "Species", "ShelfZone"), summarize,
                    Abundance = sum(Abundance),
                    F1 = mean(FRI_F1),
                    F2 = mean(FRI_F2),
                    P1 = mean(FRI_P1),
                    P2 = mean(FRI_P2))
  # remove any row where P1 is NA
  FRI_main_shelf <- FRI_main[!is.na(FRI_main$P1),]
  # remove any row where P1 and P2 are both 0
  FRI_main_shelf <- FRI_main_shelf[!(FRI_main_shelf$P1 == 0 & FRI_main_shelf$P2 == 0),]
  # calculate FRI components
  FRI_main_shelf$SNV1 <- ((FRI_main_shelf$Abundance)^FRI_main_shelf$F1)*FRI_main_shelf$P1
  FRI_main_shelf$SNV2 <- ((FRI_main_shelf$Abundance)^FRI_main_shelf$F2)*FRI_main_shelf$P2
  FRI_main_shelf$SDV1 <- ((FRI_main_shelf$Abundance)^FRI_main_shelf$F1)
  FRI_main_shelf$SDV2 <- ((FRI_main_shelf$Abundance)^FRI_main_shelf$F2)
  # change all SDV values to 0 if SNV = 0
  FRI_main_shelf$SDV1[FRI_main_shelf$SNV1 == 0] <- 0
  FRI_main_shelf$SDV2[FRI_main_shelf$SNV2 == 0] <- 0
  # calculate FRI by station
  FRIxStationID <- ddply(FRI_main_shelf, c("StationID", "ShelfZone"), summarize,
                         FRI = (sum(SNV1) + sum(SNV2)) / (sum(SDV1) + sum(SDV2)))
  #################
  ## PLOTTING  MAP
  #################
  # State outlines
  #################
  #usa <- importShapefile( file.path( proj_dir, "USA_adm1.shp" ) )
  ###############
  # MPA outlines
  ###############
  # read in using rgdal package   #!!!!! NOTE readOGR does not follow this pathname to file ~/Dropbox/VRG Files/R Code/Mapping/  , therefore the shp files must be in the working directory
  #MP <- readOGR( "Adopted_MLPA_South__964.shp",layer="Adopted_MLPA_South__964")
  ###################
  # bathymetry lines
  ###################
  #SCB_topography <- read.csv("SCB_Topography.csv", header=F,col.names=c("x","y","z"))  #Reads topography data obtained from http://topex.ucsd.edu/cgi-bin/get_data.cgi
  ##############
  subpops$FRI <- FRIxStationID$FRI[match(subpops$StationID, FRIxStationID$StationID)]
  subpops$FRI_Ref <- ifelse(subpops$FRI < 45, "Reference", "Non-Reference")
  subpops <- subpops[!is.na(subpops$FRI),]
  ## Create Table of Station Info
  Site_List <- unique(subpops[,c("StationID", "TargetLatitude", "TargetLongitude", "ShelfZone", "FRI", "FRI_Ref" )] )  # create list of sites in data set                                                  
  # sort by Site (ordered factor)
  Site_List$Num_Label <- 1:length(Site_List$StationID)
  # change name to work with code below or on other analyses
  site_locs <- Site_List
  Site_Table <- Site_List
  ### geographic location labels
  geo_labels_main <<- data.frame(PID=1:11, X=c(-120.35, -120.35, -119.8, -118.8, -118.2, -118.2, -118.2, -117.55, -117.15, -117.15, -117.15), Y=c(34.55, 34.5, 34.47, 34.1, 33.9, 33.85, 33.8, 33.5, 32.95, 32.9, 32.85),
                                 label=c("Point", "Conception", "Santa Barbara", "Point Dume", "Los Angeles", "& Long Beach", "Harbors", "Dana Point", "San", "Diego", "Bay"))
  geo_labels_main <<- as.PolyData(geo_labels_main, projection="LL")
  #### FRI
  ### COLORED POINTS (REFERENCE/NONREFERENCE)
  ## Convert to EventData table format for points (PBSmapping package)
  # Num_Label
  FRI_locations <<- data.frame(PID=1:length(site_locs$StationID), 
                               X=site_locs$TargetLongitude, Y=site_locs$TargetLatitude,
                               Station=site_locs$StationID, label=site_locs$Num_Label, cex=2.0, pch=19,
                               col=revalue(site_locs$FRI_Ref, replace=c("Reference" = "green", "Non-Reference" = "red")))
  FRI_locations$col <<- as.character(FRI_locations$col)
  FRI_locations$pch <<- as.numeric(FRI_locations$pch)
  FRI_locations <<- as.PolyData(FRI_locations, projection="LL")
  #
  return( FRI_locations )
}
#
FRI_loc2res <- function( FRI_locations ) {
  #
  cols <- c( 1,4,2,3,8 )
  FRI_results <- FRI_locations[,cols]
  names(FRI_results) <- c( "No.","Station","Lon","Lat","FRI" )
  indx_ref <- FRI_results$FRI == "green"
  FRI_results$FRI[indx_ref] <- "Reference"
  indx_nref <- FRI_results$FRI == "red"
  FRI_results$FRI[indx_nref] <- "Non-Reference"
#  
  return( FRI_results) 
}
#
FRI_loc2_calc <- function( FRI_locations ) {
  #
  FRI_locations2 <- as.data.frame( FRI_locations )
  FRI_locations2$pch <- 1
  FRI_locations2$col <- "black"
  FRI_locations2 <<- as.PolyData(FRI_locations2, projection="LL")
  #
  return( FRI_locations2 )
}
