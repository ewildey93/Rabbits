library(readr)
library(dplyr)
library(data.table)
myout <- "C:/Users/eliwi/OneDrive/Documents/Supercomputer"


myfiles <- list.files(file.path(myout, "nodes"), 
                      pattern="gps",recursive = TRUE, full.names = TRUE)
myfiles <- myfiles[1:5]
listnames <- list.files(path = "C:/Users/eliwi/OneDrive/Documents/Supercomputer/nodes",pattern = "gps",recursive=T,full.names = FALSE)
listnames <- gsub(listnames,pattern = "/gps.csv", replacement = "")
listnames <- listnames[1:5]
Nodegps <- lapply(myfiles, function (x) read_csv(x))
names(Nodegps) <- listnames
Nodegps <- lapply(Nodegps, function (x) x[x$Latitude > 38.489  & x$Latitude < 38.513, ])
Nodegps <- lapply(Nodegps, function (x) x[x$Longitude > -106.036  & x$Longitude < -106.008,])
Nodegps <- lapply(Nodegps, function (x) {attributes(x$Time)$tzone <- "America/Denver"
;x})
Nodegps <- lapply(Nodegps, function (x) mutate(x,Date=as.Date(Time)))
NodeGPS <- rbindlist(l = Nodegps,use.names = TRUE,fill = TRUE, idcol=TRUE)
NodeGPS[,c('Latitude','Longitude')] <- round(NodeGPS[,c('Latitude','Longitude')],4)
NodeGPS <- NodeGPS%>%group_by(.id, Date)%>%mutate(difLat=abs(Latitude-lag(Latitude)))%>%ungroup()
NodeGPS <- NodeGPS%>%group_by(.id, Date)%>%mutate(difLong=abs(Longitude-lag(Longitude)))%>% ungroup()
NodeGPS$difLat[is.na(NodeGPS$difLat)] <- 0
NodeGPS$difLong[is.na(NodeGPS$difLong)] <- 0
NodeGPS <- NodeGPS[NodeGPS$difLat > .0002 | NodeGPS$difLat == 0 ,]
NodeGPS <- NodeGPS[NodeGPS$difLong > .0002 |NodeGPS$difLong == 0,]
NodeGPS <- distinct(NodeGPS, .id,  Latitude, Longitude, Date, .keep_all = TRUE)
Dates <- data.frame(Date=seq.Date(as.Date("2021-10-10"), as.Date("2021-12-17"),by = 1))
NodesbyDate <- merge(NodeGPS, Dates, by="Date", all.y = TRUE)
NodesbyDate$Date <- as.factor(NodesbyDate$Date)
NodesbyDateList <- split(NodesbyDate, f=NodesbyDate$Date)
