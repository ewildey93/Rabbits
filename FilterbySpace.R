library(data.table)

path <- "/scratch/alpine/c834561176@colostate.edu/Rabbits/"
Locs <- readRDS(paste0(path,"/Locs/TagID.rds"))
TagID <- unique(Locs$TagId)
TimeRange <- readRDS(paste0(path, "/TimeRange/TagIDTimeRange.rds"))
#########################apply time spread filters#########################
DateNamesList <-paste(TagID, paste0(format(as.Date(keep$DateKeyStart), "%m/%d"), "-", format(as.Date(keep$DateKeyEnd), "%m/%d")))
Locs2 <- Tag781[-c(1:length(Tag781$TagId)),]
for (i in 1:length(keep$DateKeyStart)){
  x <- Tag781[between(Tag781$DateTime, keep$DateKeyStart[i], keep$DateKeyEnd[i]),]
  x$DateKeyStart <- keep$DateKeyStart[i]
  Locs2 <- rbind(x, Locs2)
}
table(Locs2$Date)
Locs3 <- split(Locs2, f=Locs2$DateKeyStart)
names(Locs3) <- DateNamesList


#########################filter Locs by point spread########################
library(sf)
library(sp)
Locs4 <- lapply(Locs3, function (x) st_as_sf(x, coords = c("x.est", "y.est"), crs=CRS("+init=epsg:32613")))
AvgLocs <- lapply(Locs4, function (x) x%>%summarise(geometry=st_union(geometry))%>%st_centroid)
AvgLocs <- lapply(AvgLocs, function (x) st_coordinates(x))
Locs5 <- Map(function (x,y) mutate(x, distance=st_distance(x,y)), Locs4, AvgLocs)
Locs6 <- rbindlist(Locs5, idcol = TRUE)
Locs7 <- Locs6%>%group_by(.id)%>%mutate(SDdistance=sd(distance))
Locs6%>%group_by(.id)%>%summarise(SDdistance=sd(distance))
########################scrap###################################
x <- as.data.frame(t(st_distance(AvgLocs[["781E4B78 01/19-01/20"]], Locs4[["781E4B78 01/19-01/20"]])))
y <- as.data.frame(Locs5[["781E4B78 01/19-01/20"]][["distance"]])
test <- cbind(x,y)
