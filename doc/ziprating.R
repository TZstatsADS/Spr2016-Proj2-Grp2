load("data/rating with punishment increasing.RData")
load("data/rating with punishment decreasing.RData")
load("data/rating each year increase.RData")

zip.list = total.zip.rating.with.punishment.increase[,1]
time = c(2010,2011,2012,2013,2014,2015,2016)


zip.rate = function( zipcode , punishment, year, increase ){
  # given the rate for one zipcode
  # punishment: 1 use punishment data, 0 for not
  # increase: 1 for yes, 0 for no
  zip.index = which(zip.list == zipcode)
  m = which(time == year )
  if( punishment==1 ){
    if( increase==1 ){
      rate = total.zip.rating.with.punishment.increase[zip.index,(m+1)]
    }else{
      rate = total.zip.rating.with.punishment.decrease[zip.index,(m+1)]
    }
  }else{
    if ( increase==1 ){
      rate = total.zip.rating.each.year[zip.index,(m+1)]
    }else{
      rate = 185 - total.zip.rating.each.year[zip.index,(m+1)]
    }
  }
  cat("the rate of zipcode", zipcode, "in 185 zipcode regions is", rate)
}



ratingplot = function(x){
  # for ploting the zipcode rating each year use score increasely sort
  # x is a list of zipcode
  # this plot give the y label decreasely from top to bottom
  n = length(x)
  zip.index = vector()
  for ( i in 1:n){
    zip.index[i] = which(x[i] == zip.list)
  }
  plot(c(1:7),total.zip.rating.each.year[zip.index[1],c(2:8)], type="b",col=1, 
       ylim = rev(range(total.zip.rating.each.year[,c(2:8)])), xaxt= "n", main = "rating each year",
       xlab = "year", ylab ="rating")
  axis(1, c(1:7), labels = time,col.axis="black")
  for ( i in 2:n){
    lines(c(1:7),total.zip.rating.each.year[zip.index[i],c(2:8)],type = "b",col=i)
  }
  legend("topright", as.character(x), lty =rep(1,n), col = as.character(c(1:n)),cex = 0.5)
}






