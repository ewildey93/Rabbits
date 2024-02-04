library(camtrapR)
library(ggplot2)
library(lubridate)
library(stringr)
setwd("C:/Users/eliwi/OneDrive/Documents/R/Rabbits/Rabbits")


camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/images.csv")
str(camdf)
table(camdf$deployment_id)
table(camdf$common_name)
camdf$timestamp <- as.POSIXct(camdf$timestamp,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
#BUSH31 fix dates
datefix <- c("7-19-2022 00:00:00", "4-24-2022 00:00:00")
datefix <- as.POSIXct(datefix, format="%m-%d-%Y %H:%M:%S")
datefix[1]-datefix[2] #86 days
datefix[2] + 60*60*24*86
for (i in 1:nrow(camdf)){
  if(camdf$timestamp[i] < "2022-07-19 00:00:00" & camdf$deployment_id[i] == "BUSH31") {
    camdf$timestamp[i] <- camdf$timestamp[i] + 60*60*24*86
  }
}
#check to make sure BUSH31 dates are fixed
ggplot(camdf[camdf$deployment_id == "BUSH31",], aes(x=as.Date(timestamp))) + geom_histogram(binwidth = 1)
Bush31 <- camdf[camdf$deployment_id == "BUSH31",]


#winnow dataframe down to lagomorphs
df <- camdf[camdf$common_name=="White-tailed Jackrabbit" | camdf$common_name == "Mountain Cottontail",]
df <- df[,c(2,4,14,16,17,22)]
colnames(df) <- c("cam","file","species","datetime","count","comments")
#replace BUSH3 with BUSH4
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
table(df$cam)
df$cam <- as.factor(df$cam)

#make camera operability matrix
DTimes2 <- read.csv("./Deployment Times - Sheet2.csv", na.strings = c("", "NA"))
DTimes2$setup <- str_extract(DTimes2$X, ".*(?=-)")
DTimes2$retrieval <- ifelse(!is.na(DTimes2$X.1)==FALSE, str_extract(DTimes2$X, "(?<=-).*"),
                            str_extract(DTimes2$X.1, "(?<=-).*"))
DTimes2$probFrom <-ifelse(!is.na(DTimes2$X.1)==FALSE, NA,
                          str_extract(DTimes2$X, "(?<=-).*"))
DTimes2$probTo <-ifelse(!is.na(DTimes2$X.1)==FALSE, NA,
                        str_extract(DTimes2$X.1, ".*(?=-)"))
camOps <- cameraOperation(CTtable = DTimes2, stationCol = "Camera", )


JackDetect <- detectionHistory(recordTable = camdf, species = "White-tailed Jackrabbit", camOp = ,
                               output="binary", stationCol = "deployment_id", speciesCol = "common_name",
                                recordDateTimeCol = "timestamp", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                               occasionLength = 1, timeZone = "America/Denver")
CottontDetect <-



#####Covariates######################
# Camera Grid shapefile
Grid <- readOGR("C:/Users/eliwi/OneDrive/Documents/Salida/GeospatialLayers/Grid3.shp")
CamLocs <- read.csv("./CamLocs.csv")
str(CamLocs)
CamLocs <- CamLocs[-c(38:41),]
CamLocs[25, c(2,3)] <- c("38.498457","-105.986953")
#Line Length- length of trails in grid cell
LineLength <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/LineLength.csv")
LineLengthGrid <- LineLength[-5,]
LineLengthGrid <- LineLengthGrid[, c(9,11)]
colnames(LineLengthGrid)[1] <- "LengthGrid"

LineLength150 <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/LineLength150.csv")
LineLength150 <- LineLength150[-5,]
colnames(LineLength150)[2] <- "Length150"
LineLength150 <- LineLength150[, c(4,2)]
#lc at different distances
lc <- raster("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/CoVs/NLCD_2019_Land_Cover_L48_20210604_JbsuwO6GkIW9V4xHbi6d.tiff")
plot(lc)
projection(lc)
freq(lc)
rcl <- matrix(data=c(22,23,24,42,43,81,82,95,21,21,21,41,41,71,71,90), nrow=8, ncol=2)
lc2 <- reclassify(lc, rcl=rcl)
plot(lc2)
CamLocsSF<-st_as_sf(CamLocs, coords=c("Long", "Lat"), crs=CRS("+init=epsg:4326"))
CamLocsSF<-st_transform(CamLocsSF, projection(lc))
lc100<-raster::extract(lc2, CamLocsSF, buffer=100)
lc250 <- raster::extract(lc2, CamLocsSF, buffer=250)
lc385 <- raster::extract(lc2, CamLocsSF, buffer=385)

z <- sort(unique(raster::values(lc2)))

summaryValueslc100 <- lapply(lc100,FUN = summarizeLC,LC_classes = z)
summaryValueslc250 <- lapply(lc250,FUN = summarizeLC,LC_classes = z)
summaryValueslc385 <- lapply(lc385,FUN = summarizeLC,LC_classes = z)

listnames <- CamLocs$Camera

names(summaryValueslc100) <- listnames
names(summaryValueslc250) <- listnames
names(summaryValueslc385) <- listnames
lclist <- list('LC100'=summaryValueslc100,'LC250'=summaryValueslc250,'LC385'=summaryValueslc385)


lclist <- lapply(lclist, function (x) lapply(x, function (x) as.data.frame(t(x))))
lclist <- lapply(lclist, function (x) lapply(x, function (x) cbind(x, lc= rownames(x))))
lclist <- lapply(lclist, function (x) rbindlist(x, idcol = T))


LCDF <- reduce(lclist, full_join, by=c(".id", "lc"))
colnames(LCDF)[c(2,4,5)] <- c('lc100','lc250', 'lc385')
LCDF <- LCDF[LCDF$.id != "BUSH03",]
colnames(LCDF)[1] <- "Camera"
ForestDF <- LCDF[LCDF$lc == 41,]
ShrubDF <- LCDF[LCDF$lc == 52,]


ForestDF$Camera[7:14] <- c("BUSH1","BUSH2","BUSH4","BUSH5","BUSH6","BUSH7","BUSH8","BUSH9")
ShrubDF$Camera[7:14] <- c("BUSH1","BUSH2","BUSH4","BUSH5","BUSH6","BUSH7","BUSH8","BUSH9")
colnames(ForestDF)[2:5] <- c("lc100.Forest","Forest", "lc250.Forest","lc385.Forest")
colnames(ShrubDF)[2:5] <- c("lc100.Shrub","Shrub", "lc250.Shrub","lc385.Shrub")

#convert herbaceous cover to polygon and get distance to values
herb <- rasterToPolygons(lc2, function (x) x == 71, n=16, dissolve = T)
writeOGR(herb, "./", "herbPoly.shp", driver="ESRI Shapefile")
herb <- st_read(dsn="./", layer="herbPoly.shp")
crs(herb)
crs(CamLocsSF)
CamLocs$distidx <- st_nearest_feature(CamLocsSF, herb)
CamLocs$distherb <- st_distance(CamLocsSF, herb[CamLocs$distidx,], by_element = T)
#CamLocs2$distherb <- CamLocs$distherb
CamLocs2 <- CamLocs[,-c(4:11,12)]

#get relative activity of humans
camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/wildlifeinsights5.5/images.csv")
df <- camdf[camdf$genus=="Homo",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
df$week <- week(df$datetime)
df <- df %>% group_by(cam, week)%>% mutate(HumanSum= sum(count))
HumansatCam <- df%>%group_by(cam)%>%summarise(mean(HumanSum))
colnames(HumansatCam)[1] <- "Camera"

#relative activity of predators
camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/wildlifeinsights5.5/images.csv")
predators <-c("Grey Fox", "Red Fox", "Coyote", "Canis Species", "Bobcat", "Puma", "Canine Family", "Carnivorous Mammal")
df <- camdf[camdf$genus=="Homo",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
df$week <- week(df$datetime)
df <- df %>% group_by(cam, week)%>% mutate(HumanSum= sum(count))
HumansatCam <- df%>%group_by(cam)%>%summarise(mean(HumanSum))
colnames(HumansatCam)[1] <- "Camera"

#slope
Slope <- raster("./Slope2.tif")
projection(Slope)
CamLocsSF<-st_transform(CamLocsSF, projection(Slope))
CamLocs2$slope<-raster::extract(Slope, CamLocsSF)
CamLocs2$slope100 <- raster::extract(Slope, CamLocsSF, buffer=100, fun=function(x) mean(x))


#join all dfs in prep for regression
CamLocs2
CamLocs2$Camera[7:15] <- c("BUSH1","BUSH2","BUSH3","BUSH4","BUSH5","BUSH6","BUSH7","BUSH8","BUSH9")

CoVsList <- list(HumansatCam,CamLocs2,LineLengthGrid, LineLength150, ForestDF,ShrubDF)
CoVs <- CoVsList%>% reduce(left_join, by="Camera")
CoVs[CoVs$Camera == "BUSH3",]
saveRDS(CoVs, "CoVsRabbitCo.rds")
CoVs <- readRDS("./CoVsRabbitCo.rds")

#models
CoVs <- RegDF[,c(2, 5:10,12:14,16,17)]
corrplot(cor(CoVs),
         method = "number",
         type = "upper" # show only upper side
)

#functions
summarizeLC <- function(x,LC_classes,LC_names = NULL){
  # Find the number of cells
  y <- length(x)
  # Make a table of the cells
  tx <- table(x)
  # Create an empty array to store landcover data
  LC <- array(NA,c(1,length(LC_classes)))
  # Loop through the landcover types & return
  # the number of cells within each landcover type
  for(i in seq(LC_classes)){
    LC[1,i] <- ifelse(LC_classes[i] %in% dimnames(tx)[[1]],
                      #if true
                      tx[which(dimnames(tx)[[1]]==LC_classes[i])],
                      # if false
                      0)
  } # end loop
  # Convert to percentage
  LC <- LC/y
  #
  if(!is.null(LC_names)){
    colnames(LC)<-LC_names}
  else{colnames(LC)<-LC_classes}

  return(LC)
}

####assess temporal independence between records   ####

assessTemporalIndependence2 <- function(intable,
                                        deltaTimeComparedTo,
                                        columnOfInterest,     # species/individual column
                                        cameraCol,
                                        camerasIndependent,
                                        stationCol,
                                        minDeltaTime,
                                        eventSummaryColumn,
                                        eventSummaryFunction)
{
  # check if all Exif DateTimeOriginal tags were read correctly
  if(any(is.na(intable$DateTimeOriginal))){
    which.tmp <- which(is.na(intable$DateTimeOriginal))
    if(length(which.tmp) == nrow(intable)) stop("Could not read any Exif DateTimeOriginal tag at station: ", paste(unique(intable[which.tmp, stationCol])), " Consider checking for corrupted Exif metadata.")
    warning(paste("Could not read Exif DateTimeOriginal tag of", length(which.tmp),"image(s) at station", paste(unique(intable[which.tmp, stationCol]), collapse = ", "), ". Will omit them.\nConsider checking for corrupted Exif metadata. Or does your selected time zone have daylight saving time and the image(s) fall in the misisng hour at spring formward (cameras don't usually record DST)?. \n",
                  paste(file.path(intable[which.tmp, "Directory"],
                                  intable[which.tmp, "FileName"]), collapse = "\n")), call. = FALSE, immediate. = TRUE)
    intable <- intable[-which.tmp ,]
    rm(which.tmp)
  }

  # prepare to add time difference between observations columns
  intable <- data.frame(intable,
                        delta.time.secs  = NA,
                        delta.time.mins  = NA,
                        delta.time.hours = NA,
                        delta.time.days  = NA,
                        independent      = ifelse(minDeltaTime == 0, TRUE, NA),   # all independent if no temporal filtering
                        stringsAsFactors = FALSE,
                        check.names      = FALSE)        # to prevent ":" being converted to ".", e.g. in EXIF:Make

  # sort records by station, species, then time
  intable <- intable[order(intable[, stationCol], intable[, columnOfInterest], intable$DateTimeOriginal),]

  for(xy in 1:nrow(intable)){     # for every record


    which.columnOfInterest <- which(intable[, columnOfInterest]  == intable[xy, columnOfInterest])          # same species/individual
    which.stationCol       <- which(intable[, stationCol]        == intable[xy, stationCol])                # at same station
    which.independent      <- which(intable$independent          == TRUE)                                   # independent (first or only record of a species at a station)
    #which.earlier          <- which(intable$DateTimeOriginal     <  intable$DateTimeOriginal[xy])          # earlier than record xy (takes long)
    which.earlier          <- 1: (xy-1)                                                                  # earlier than record xy  (fast alternative, relies on table being sorted by date/time before anything else)
    if(camerasIndependent) {
      which.cameraCol      <- which(intable[, cameraCol]  == intable[xy, cameraCol])                        # at same camera
    }

    # set independent = TRUE and delta.time = 0 if it is the 1st/only  record of a species / individual

    if(camerasIndependent == TRUE){
      which.tmp <- Reduce(intersect, list(which.columnOfInterest,
                                          which.stationCol,
                                          which.cameraCol))
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp])){    # cameras at same station assessed independently
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    } else {
      #here
      #find common elements of multiple vectors, which index values are the same, subset to those
      which.tmp <- Reduce(intersect, list(which.columnOfInterest, #species
                                          which.stationCol))      #camera
      duplicateTime <- which(duplicated(intable$DateTimeOriginal) ==TRUE)
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp]) & !(xy %in% duplicateTime)) {
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp]) & (xy %in% duplicateTime)){
        intable$independent[xy]       <- FALSE
        intable$delta.time.secs[xy]   <- 0
      }
    }

    # calculate time difference to previous records of same species at this station (if not the 1st/only record)
    if(is.na(intable$delta.time.secs[xy])) {

      if(deltaTimeComparedTo == "lastIndependentRecord"){

        if(camerasIndependent == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest,
                                                which.stationCol,
                                                which.cameraCol,
                                                which.independent,
                                                which.earlier))
        } else {
          which_time2 <- Reduce(intersect, list(which.columnOfInterest,
                                                which.stationCol,
                                                which.independent,
                                                which.earlier))
        }
      }  else {    # if(deltaTimeComparedTo == "lastRecord"){'
        if(camerasIndependent  == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest,
                                                which.stationCol,
                                                which.cameraCol,
                                                which.earlier))
        } else {
          #here
          which_time2 <- Reduce(intersect, list(which.columnOfInterest,
                                                which.stationCol,
                                                which.earlier))
        }
      }

      # time difference to last (independent) record
      diff_tmp <- min(na.omit(difftime(time1 = intable$DateTimeOriginal[xy],            # delta time to last independent record
                                       time2 = intable$DateTimeOriginal[which_time2],
                                       units = "secs")))

      # save delta time in seconds
      intable$delta.time.secs[xy] <-  diff_tmp
      if(intable$delta.time.secs[xy] >= (minDeltaTime * 60)){
        intable$independent[xy] <- TRUE
      } else {
        intable$independent[xy] <- FALSE
      }

    }   # end   if(intable$DateTimeOriginal[xy] == min(...)} else {...}
  }     # end for(xy in 1:nrow(intable))



  # keep only independent records
  outtable <- intable[intable$independent,]


  # compute delta time in hours and days
  outtable$delta.time.secs  <- round(outtable$delta.time.secs, digits = 0)
  outtable$delta.time.mins  <- round(outtable$delta.time.secs  / 60, digits = 1)
  outtable$delta.time.hours <- round(outtable$delta.time.mins  / 60, digits = 1)
  outtable$delta.time.days  <- round(outtable$delta.time.hours / 24, digits = 1)

  # remove "independent" column



  return(outtable)
}
