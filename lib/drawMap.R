#'
#'@data Data for map. With variable Longitue & Latitude
#'
drawMap = function(data){
  m = leaflet()%>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    addCircleMarkers(lng = data$Longitude,lat = data$Latitude,radius = 3, stroke = FALSE,col = "yellow")
  return(m)
}