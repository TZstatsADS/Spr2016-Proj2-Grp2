#basic set up
setwd("~/Data Science/project2-project2-group2/data")
x = c("leaflet","dplyr","data.table","XML")
source("../lib/makeIcon.R")
source("../lib/drawMapFood.R")
#install.packages(x)
lapply(x,library,character.only = TRUE)
#load data
load("resColumbia.RData")
drawMapFood(resColumbia)

