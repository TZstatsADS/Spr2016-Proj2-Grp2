drawMapFood = function(data,emoji){
  m = leaflet(data)%>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    addMarkers(~Longitude, ~Latitude,icon = icon1)
  return(m)
}