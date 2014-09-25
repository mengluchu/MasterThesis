library(rworldmap)
library(ggmap)
library(sp)

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

# the time stamps
matogrosso$dates = strsplit(as.vector(matogrosso$dates), ';')
dates = unlist(dates)


matogrosso$dates = lapply(matogrosso$dates, function(x) lapply(x, function (x) as.Date(x, format="%Y-%m-%d")))


# the filtered evi values
evi = strsplit(as.vector(matogrosso$evi), ';')
matogrosso$evi = as.numeric(unlist(evi))

# the original evi values
evio = strsplit(as.vector(matogrosso$evio), ';')
matogrosso$evio = as.numeric(unlist(evio))

mgts = ts(evi, frequency = 23)


