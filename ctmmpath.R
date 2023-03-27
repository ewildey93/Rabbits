setwd("C:/Users/eliwi/OneDrive/Documents/R/Rabbits/Rabbits")
library(lubridate)
library(ctmm)
library(tidyr)
library(dplyr)
library(move)
library(data.table)
library(sf)
library(adehabitatLT)

#read in data
RabbitTest <- read.csv("./3.28Est.Locations.Filter.Distance.187.5_Results.csv")
table(RabbitTest$TagId)
RabbitTest$DateTime <- as.POSIXct(paste(RabbitTest$Date, RabbitTest$Time), "%Y-%m-%d %H:%M:%S", tz = "GMT")

R192D <- RabbitTest[RabbitTest$TagId == "192D2A1E",]
head(R192D)

R192D <- R192D%>%na.omit(DateTime)%>%arrange(DateTime)
R192Dmove<- move(x =R192D$x.est,y = R192D$y.est,time = R192D$DateTime,data=R192D, proj=CRS("+init=epsg:32613"))
R192DT <- as.telemetry(R192Dmove,timeformat = "",timezone = "", projection=NULL,datum=NULL,timeout=Inf,na.rm = "col",mark.rm = FALSE, keep=FALSE,drop=TRUE)
plot(R192DT)

vg.R192D <- variogram(R192DT)
plot(vg.R192D)
plot(vg.R192D,fraction = 0.005)
#SAVE to guess look for gear button in upright of plot window, button at bottom of pop up box
variogram.fit(vg.R192D)
fitted.mods192D <- ctmm.select(R192DT,CTMM = GUESS, verbose=TRUE)
summary(fitted.mods192D)
R192DOU <- fitted.mods192D[[1]]

?simulate
SIM <- simulate(data=R192DT, object=R192DOU, dt=60, res=1, complete=TRUE )
plot(SIM)

SIM$distance <- c(NA,geosphere::distVincentyEllipsoid(cbind(SIM$longitude, SIM$latitude)))
speed=mean(SIM$distance, na.rm=T)/60
res=speed * R192DOU$tau
