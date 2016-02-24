#
n=2
fileName = paste("gps",as.character(n),".RData",sep="")

# Set working directory
setwd("~/Data Science/project2-project2-group2/data")
x = c("leaflet","dplyr","data.table","XML")

#install.packages(x)
lapply(x,library,character.only = TRUE)
source("../lib/getGps.R")
load("resData.Rdata")
Data = resData[((n-1)*2500+1):(n*2500),]
gps = lapply(Data$addr,getGps)
gps = do.call(rbind,gps)
save(gps,file = fileName)




