getGps = function(addr,g){
  url = paste('http://maps.google.com/maps/api/geocode/xml?address=', addr,'&sensor=false',sep='')  # construct the URL
  doc = xmlTreeParse(url) 
  root = xmlRoot(doc) 
  lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]) 
  log = xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
  print("yeah")
  return(c(lat,log))
}

url = paste('http://maps.google.com/maps/api/geocode/xml?address=', resData$addr[1:15],'&sensor=false',sep='')  # construct the URL


