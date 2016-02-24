setwd("~/Documents/stat4249/project2-project2-group2/data")
load("miceData.RData")

#plot and trend of mice number of all date
plot(table(miceData$Date),ylab = "Mice number", col = "red", main = "Mice number according to Time")
lines(lowess(table(miceData$Date)),type = "b", col = "yellow")

#This date x, y can be changed into costumized date
x <- as.Date("2011-09-10") 
y <- as.Date("2015-07-22")

#plot and trend of mice number of between costumized date x and y
plot(table(miceData$Date[miceData$Date<y&miceData$Date>x]),type="h",ylab = "Mice number",xlab = "Date",  col = "black", main = "Mice number according to Time")
lines(lowess(table(miceData$Date[miceData$Date<y&miceData$Date>x])),type = "b", col = "yellow")

