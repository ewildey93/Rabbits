camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/images.csv")
str(camdf)
table(camdf$deployment_id)
table(camdf$common_name)
df <- camdf[camdf$common_name=="White-tailed Jackrabbit" | camdf$common_name == "Mountain Cottontail",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")


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
saveRDS(CoVs, "CoVs.rds")
CoVs <- readRDS("./CoVs.rds")

colnames(estN)[1] <- "Camera"
RegDF <- left_join(CoVs, estN, by="Camera")
colnames(RegDF)[2] <- "HumanAct"
RegDF <- RegDF[!is.na(RegDF$N),]
RegDF$CamType <- ifelse(grepl(pattern="ACORN", x=RegDF$Camera) == TRUE, "ACORN", "BUSH")
saveRDS(RegDF, "./RegDF.rds")
RegDF <- readRDS("./RegDF.rds")
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
