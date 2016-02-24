drawMapFood = function(data){
  m = leaflet(data)%>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    addMarkers(~Longitude, ~Latitude,icon=Type)
  return(m)
}