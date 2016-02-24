getDataGps = function(Data){
  for(i in 1:nrow(data)){
    gps = getGps(resData[i,addr])
    Longtitude = c(Longtitude,gps[1])
    Latitude = c(Latitude,gps[2])
  }
  return(cbind(Longtitude,Latitude))
}