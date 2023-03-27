library(dplyr)

path <- "/scratch/alpine/c834561176@colostate.edu/Rabbits/"
######read in each Tags locations files and combine into named list################
#TagLocs <- list.files(file.path("/scratch/alpine/c834561176@colostate.edu/Locs/"),
#                      pattern="Tag",recursive = TRUE, full.names = TRUE)
#listnames <- list.files(path = "/scratch/alpine/c834561176@colostate.edu/Locs/",pattern = "Tag",recursive=T,full.names = FALSE)
#listnames <- gsub(listnames,pattern = ".rds", replacement = "")
#TagList <- lapply(myfiles, function (x) read_csv(x))
#names(TagList) <- listnames
Locs <- readRDS(paste0(path,"/Locs/TagID.rds"))
#table(Locs3.28$TagId)
str(Locs)
#Locs3.28$DateTime <- as.POSIXct(paste(Locs3.28$Date, Locs3.28$Time), format= "%Y-%m-%d %H:%M:%S", tz="America/Denver")
#Tag781 <- Locs3.28[Locs3.28$TagId == "781E4B78",]
#TagList <- split(Locs3.28, f=Locs3.28$TagId)

###########get range of dates of locations by TagId#################
DateRangeTag781 <- range(Tag781$Date)
#DateRange <- lapply(TagList, function (x) range(x$DateTime))
#DateRange <- lapply(DateRange, function (x) as.Date(x, tz="America/Denver"))

DateSeq <- seq.POSIXt(as.POSIXct(paste(DateRangeTag781[1] -1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"),as.POSIXct(paste(DateRangeTag781[2] + 1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"), by="1 day")
#DateSeq <- lapply(DateRange, function (x)
#  seq.POSIXt(as.POSIXct(paste(x[1] -1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"),as.POSIXct(paste(x[2] + 1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"), by="1 day"))

DateKey <- data.frame(TagId=Tag781$TagId[1], Date=DateSeqTag781, Date2=DateSeqTag781 + 86400)
#DateKey <- lapply(DateSeq, function (x) data.frame(Date=x, Date2=x + 86400))
#DateKey <- Map(cbind, DateKey, name=names(DateKey))
#locsdf <- Locs3.28%>%group_by(TagId, Date)%>%summarise(count=n(), range=range(Time))

######filter dataframes by Days with observations covering that day

TimeRange <- data.frame(ObsStart=as.character(), ObsEnd=as.character(), TimeDiff=as.numeric(),
                        DateKeyStart=as.character(), DateKeyEnd=as.character(), TagId=as.character())
for (i in 1:length(DateKey$Date)){
subsetTag <- Tag781[between(Tag781$DateTime, DateKey$Date[i], DateKey$Date2[i]),]
if(nrow(subsetTag) > 0){
  y <- range(subsetTag$DateTime)
  y <- as.data.frame(as.list(y))
  colnames(y) <- c("ObsStart", "ObsEnd")
  y$TimeDiff <- as.numeric(difftime(y[1,2], y[1,1]))
  y$DateKeyStart <- DateKey$Date[i]
  y$DateKeyEnd <- DateKey$Date2[i]
  y$TagId <- unique(subsetTag$TagId)
  TimeRange <- rbind(TimeRange, y)
}
}
keep <- TimeRange[TimeRange$TimeDiff >= 20,]
keepname <- paste0(unique(Locs$TagId, "TimeRange.rds"))
saveRDS(keep, paste0("/scratch/alpine/c834561176@colostate.edu/Rabbits/TimeRange/", keepname))
