library(rworldmap)
library(ggmap)
library(sp)
library(bfast)

# file named deter.csv with samples of time series related to DETER evaluation
deter = read.csv("/home/christopher/Documents/Master Thesis/Orientation/deter.csv")

# subset of area of Mato Grosso with bbox of -61,6325, -18,0418, -50,2244, -7,3487
matogrosso = subset(deter, (lon <= -50.2244 & lon >= -61.6325) & (deter$lat <= -7.3487 & deter$lat >= -18.0418))
matogrosso

# Print a map where the points represent the center of the associated alert polygon by DETER
matogrosso2 = matogrosso
coordinates(matogrosso2) <- ~lon+lat
bbox = bbox(matogrosso2)
map <- get_map(location = c(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]), zoom = 5, maptype="hybrid")
p <- ggmap(map)
p <- p + geom_point(data = matogrosso, aes(x = lon, y = lat, color="red", label="1"))
p <- p + labs(list(title = "Places related to an DETER alert", x = "Longitude", y="Latitude"))
print(p)

# convert dates to Date object needed 
matogrosso$dates = strsplit(as.vector(matogrosso$dates), ';')
matogrosso$dates = lapply(matogrosso$dates, function(x) lapply(x, function (x) as.Date(x, format="%Y-%m-%d")))

# the filtered evi values
matogrosso$evi = strsplit(as.vector(matogrosso$evi), ';')
matogrosso$evi = lapply(matogrosso$evi, function(x) lapply(x, function (x) as.numeric(x)))

# the original evi values
matogrosso$evio = strsplit(as.vector(matogrosso$evio), ';')
matogrosso$evio = lapply(matogrosso$evio, function(x) lapply(x, function (x) as.numeric(x)))

# function for converting from the registered DETER event to a c(period, cycle) structure
dateToPeriodCycle = function(date){
  period = as.numeric(format(date, "%Y"))
  month = as.numeric(format(date, "%m"))
  cycle = month * 2
  c(period, cycle)
}

# function for converting from a numeric date to c(period, cycle) structure
numericDateToPeriodCycle = function(numericDate, freq){
  c(floor(numericDate), round((numericDate - floor(numericDate)) * freq) + 1)
}

# init vectors for output dataframe
dateeventVector = c() 
dateeventAsPeriodCycleVector = c()
timebrpEvioVector = c()
timebrpEviVector = c()
bfmEvioVector = c()
bfmEviVector = c()

for (i in c(1:2951)) {
  timeseriesEvio = ts(unlist((matogrosso$evio[i])), c(2000, 6), frequency=23)
  timeseriesEvi = ts(unlist((matogrosso$evi[i])), c(2000, 6), frequency=23)
  
  # get the registered date of an event related to the sample (dateevent) and start monitoring before this event
  dateevent = as.Date(as.vector(matogrosso$dateevent[i]))
  dateeventVector[i] = dateevent
  
  # convert registered date of an event to c(period, cycle) representation
  dateeventAsPeriodCycle = dateToPeriodCycle(dateevent)
  dateeventAsPeriodCycleVector[i] = dateeventAsPeriodCycle
  
  # going back 5 measurements before the time of the event
  timeBeforeEvent = c(timeAsPeriodCycle[1], timeAsPeriodCycle[2] - 5)
  
  bfmEvio = bfastmonitor(timeseriesEvio, start=timeBeforeEvent)
  bfmEvioVector[i] = bfmEvio
  timebrpEvio = bfmEvio$breakpoint
  timebrpEvioVector[i] = timebrpEvio
  
  bfmEvi = bfastmonitor(timeseriesEvi, start=timeBeforeEvent)
  bfmEviVector[i] = bfmEvi
  timebrpEvi = bfmEvi$breakpoint
  timebrpEviVector[i] = timebrpEvi  
}

output = data.frame(dateeventVector, dateeventAsPeriodCycleVector, timebrpEvioVector,
                    timebrpEviVector, bfmEvioVector, bfmEviVector)

