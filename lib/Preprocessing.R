#'
#'Preprocessing of Data
#'@Data Created Date
#'@Zip  Zip code of incident
#'@X    X coordinate of incident
#'@Y    Y coordinate of incident
#'
library(data.table)
library(dplyr)
setwd("~/Data Science/project2-project2-group2/data")
colsToKeep = c("Created Date","Incident Zip","X Coordinate (State Plane)","Y Coordinate (State Plane)")
miceData = fread("Rat_Sightings.csv",select = colsToKeep)
colnames(miceData) = c("Date","Zip","X","Y")
miceData %>% 
  na.omit() %>%
  transmute(Date = as.Date(Date,"%m/%d/%Y"))
save(miceData, file = "miceData.RData")  
  