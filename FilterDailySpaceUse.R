library(dplyr)

#read in each Tags locations files and combine into named list
TagLocs <- list.files(file.path("/scratch/alpine/c834561176@colostate.edu/Locs/"),
                      pattern="Tag",recursive = TRUE, full.names = TRUE)
listnames <- list.files(path = "/scratch/alpine/c834561176@colostate.edu/Locs/",pattern = "Tag",recursive=T,full.names = FALSE)
listnames <- gsub(listnames,pattern = ".rds", replacement = "")
TagList <- lapply(myfiles, function (x) read_csv(x))
names(TagList) <- listnames
#Locs3.28 <- read.csv("./3.28Est.Locations.Filter.Distance.187.5_Results.csv")
#table(Locs3.28$TagId)
#str(Locs3.28)
#Locs3.28$DateTime <- as.POSIXct(paste(Locs3.28$Date, Locs3.28$Time), format= "%Y-%m-%d %H:%M:%S", tz="America/Denver")
#Tag781 <- Locs3.28[Locs3.28$TagId == "781E4B78",]

#
DateRangeTag781 <- range(Tag781$Date)
DateRangeTag781 <- as.Date(DateRangeTag781)
DateSeqTag781 <- seq.POSIXt(as.POSIXct(paste(DateRangeTag781[1] -1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"),
                            as.POSIXct(paste(DateRangeTag781[2] + 1, "05:00:00"),format= "%Y-%m-%d %H:%M:%S", tz="America/Denver"), by="1 day")
DateKey <- data.frame(TagId=Tag781$TagId[1], Date=DateSeqTag781, Date2=DateSeqTag781 + 86400)
locsdf <- Locs3.28%>%group_by(TagId, Date)%>%summarise(count=n(), range=range(Time))

TimeRange <- data.frame(ObsStart=as.character(), ObsEnd=as.character(), TimeDiff=as.numeric(),
                        DateKeyStart=as.character(), DateKeyEnd=as.character(), TagId=as.character())
for (i in 1:length(DateKey$Date)){
subsetTag <- Tag781[between(Tag781$DateTime, DateKey$Date[i], DateKey$Date2[i]),]
if(nrow(subsetTag) > 0){
  x <- range(subsetTag$DateTime)
  x <- as.data.frame(as.list(x))
  colnames(x) <- c("ObsStart", "ObsEnd")
  x$TimeDiff <- as.numeric(difftime(x[1,2], x[1,1]))
  x$DateKeyStart <- DateKey$Date[i]
  x$DateKeyEnd <- DateKey$Date2[i]
  x$TagId <- unique(subsetTag$TagId)
  TimeRange <- rbind(TimeRange, x)
}
}
keep <- TimeRange[TimeRange$TimeDiff >= 20,]
