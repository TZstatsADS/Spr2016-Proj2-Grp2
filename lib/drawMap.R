#'
#'@data Data for map. With variable Longitue & Latitude
#'
drawMap = function(data){
  m = leaflet(data)%>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    addCircleMarkers(~Longitude, ~Latitude,radius = 3, stroke = FALSE,col = "yellow", popup = ~Date)
  return(m)
}