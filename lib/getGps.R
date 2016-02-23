getGps = function(addr){
  url = paste('http://maps.google.com/maps/api/geocode/xml?address=', addr,'&sensor=false',sep='')  # construct the URL
  doc = xmlTreeParse(url) 
  root = xmlRoot(doc) 
  lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]) 
  log = xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
  return(c(lat,log))
}



