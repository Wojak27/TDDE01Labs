set.seed(1234567890)
library(geosphere)
stations <- read.csv("/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 3/Assignment\ 1/stations.csv", fileEncoding="latin1")
temps <- read.csv("/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 3/Assignment\ 1/temps50k.csv")
st <- merge(stations,temps,by="station_number")
#distHaversine
#density(c(-20, rep(0,98), 20))

h_distance <- 100*1000 # large distances in meters (this is why *1000)
h_date <- 30 #30 days in a month?
h_time <-4 # because we have 2h intervalls
#Tried to explain smoothing factors logically, but is not 
#place.to.predict <- c(14.826, 58.4274)# The point to predict (up to the students)
#place.to.predict = c(20.2253, 67.8558) #kiruna
#place.to.predict = c(18.0686, 59.3293) # stockholm
place.to.predict = c(17.3069, 62.3908) #sundsvall
#date <- "2013-12-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "00:00:00")
temp <- vector(length=length(times))
# Students’ code here
mydate = "2013-01-14"
mydate = "2004-09-03"

selected.dates = subset(st, as.Date(st$date) < as.Date(mydate))

#plot(gaussian.kernel(c(st$longitude[1], st$latitude[1]), place.to.predict, h_distance))

get.diff.distance.smooth = function(data, point.coord,h){
  return.vector = 1:length(data[,1])
  for (i in 1:length(data[,1])) {
    #apply smoothing factor here? Wonder if it's correct...
    x = distHaversine(c(data$longitude[i], data$latitude[i]), point.coord)/h
    return.vector[i] = exp(-x^2)
  }
  return(return.vector)
  
}

get.diff.date.smooth = function(data, day.of.intresst, h){
  return.vector = 1:length(data[,1])
  for( i in return.vector){
    x = as.numeric(as.Date(day.of.intresst)-as.Date(data$date[i]))/h
    #apply smoothing factor here? Wonder if it's correct...
    return.vector[i] = exp(-(x)^2)
  }
  print(head(return.vector))
  return(return.vector)
}

get.diff.time.smooth= function(data, mytime,h){

  return.vector = 1:length(data[,1])
  for( i in return.vector){
    time1 = as.POSIXct(as.character(data$time[i]), format = "%H:%M:%S", tz = "UTC")
    time2 = as.POSIXct(as.character(mytime), format = "%H:%M:%S", tz = "UTC")
    #apply smoothing factor here? Wonder if it's correct...
    x = (time1-time2)/h
    return.vector[i] = exp(-as.numeric(x)^2)
    }
  return(return.vector)
}


diff.distance = get.diff.distance.smooth(selected.dates, place.to.predict, h_distance)
plot(1:length(diff.distance), diff.distance)
diff.date = get.diff.date.smooth(selected.dates, mydate, h_date)
plot(1:length(diff.date), diff.date)
diff.time.vector = 1:length(selected.dates$time)

for(i in 1:length(times)) {
  #comp_time = convert_to_hours(times[i])
  diff.time.vector = get.diff.time.smooth(selected.dates,times[i], h_time)
  
  sum.kernels = diff.time.vector+diff.distance+diff.date
  prod.kernels = diff.time.vector*diff.distance*diff.date
  temp[i] = sum(sum.kernels*selected.dates$air_temperature)/sum(sum.kernels)
}

plot(1:length(diff.time.vector), diff.time.vector)

plot(temp, type = "o", xlab = "Hours", ylab = "Temperature", xaxt="n", main = paste("Temperature for: ", mydate, "at: ", as.character(place.to.predict[1]),", ", as.character(place.to.predict[2])))
axis(1, at=1:length(temp), labels=times)

