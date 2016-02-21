setwd("~/Desktop/4249-2")
library(data.table)
library(dplyr)
library(XML)
addr <- 'HOP SHING RESTAURANT,CHATHAM SQUARE,MANHATTAN'  # set your address here
url = paste('http://maps.google.com/maps/api/geocode/xml?address=', addr,'&sensor=false',sep='')  # construct the URL
doc = xmlTreeParse(url) 
root = xmlRoot(doc) 
lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]) 
long = xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
lat
long

