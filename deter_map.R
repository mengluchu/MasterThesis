deter = read.csv("/home/christopher/Documents/Studium/Master Thesis/Orientation/deter.csv")
lat = deter$lat
lon = deter$lon
library(rworldmap)
library(ggmap)
library(sp)

newmap <- getMap(resolution = "low")
plot(newmap, asp = 1)
points(lon, lat, col = "red", cex = .6)

# getting the bounding box
deter2 = deter
coordinates(deter2) <- ~lon+lat
bbox = bbox(deter2)

# drawing the map
# map <- get_map(location = "Brazil" , zoom = 5, maptype="toner")
map <- get_map(location = c(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]), zoom = 5, maptype="toner")
map <- get_map(location = c(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]), zoom = 5, maptype="satellite")
map <- get_map(location = c(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]), zoom = 5, maptype="terrain")
map <- get_map(location = c(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]), zoom = 5, maptype="hybrid")

p <- ggmap(map)
p <- p + geom_point(data = deter, aes(x = lon, y = lat, color="red", label="1"))
p <- p + labs(list(title = "Places related to an DETER alert", x = "Longitude", y="Latitude"))
# p <- p + geom_text(data = deter, aes(label=deter$dateevent, size=1))
print(p)

