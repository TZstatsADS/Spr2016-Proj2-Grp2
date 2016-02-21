#'
#'@data Data for map. With variable Longitue & Latitude
#'
drawMapCluster = function(data){
  m = leaflet(data)%>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    addCircleMarkers(~Longitude, ~Latitude,radius = 3, stroke = FALSE,col = "yellow", clusterOptions = markerClusterOptions())
  return(m)
}