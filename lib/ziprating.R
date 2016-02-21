load("data/miceData.RData")
dat = miceData[,1:2]
dat[,1] = format(miceData[,1], "%Y" )
dat[,1] = as.numeric(dat[,1])


zip.score = function (year){
  # year(numerical) = data after this year is used
  data.index = which(dat[,1]>=year)
  new.dat = dat[data.index,]
  count = table(new.dat)
  score = mat.or.vec(dim(count)[2],2)
  score[,1] = sort(na.omit(unique(new.dat[,2])))
  for (i in 1:dim(count)[2]){
    n = dim(count)[1]
    score[i,2] = 0
    for ( j in 1:n){
      this.score = count[j,i]*0.9^(n-j) # weight is 0.9 to the power of # of years from 2016
      score[i,2] = score[i,2] + this.score
    }
  }
 return(score)
}

zip.rate = function( zipcode , year ){
  score = zip.score(year)
  rate.list = order(score[,2])
  zip.index = which(score[,1] == zipcode)
  rate = which(rate.list == zip.index)
  cat("the rate of zipcode", zipcode, "in 185 zipcode regions is", rate)
}


