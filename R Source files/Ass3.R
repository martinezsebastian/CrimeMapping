library(foreign)
setwd("/Users/SebastianMartinez/Dropbox/0. Hertie/3/Collaborative Social Science Data Analysis/GitHub/CrimeMapping/R Source files/")
crimes <- read.csv("crimes_2015.csv")
View(crimes)

boundaries <- read.dbf("PoliceDistrict.dbf")
centroids <- read.csv("centroids.csv")

total <- merge(crimes, centroids, by.x = "district", by.y = "DIST_NUM")
total$y_dist <- total$ycoordinate - total$Y
total$x_dist <- total$xcoordinate - total$X
total$distance <- sqrt((total$x_dist)^2+(total$y_dist)^2)
total$count <- 1

community_centroids <- read.csv("Comm_centroids.csv")
View(community_centroids)
total <- merge(total, community_centroids, by.x = "communityarea", by.y = "AREA_NUMBE")


counter <- aggregate(count ~ district, data = total, FUN=mean)

for(i in counter$district) 
{
  temp <- total[total[,2]!=i,]
  nam <- paste("District", i, sep = "")
  assign(nam, temp)
}



aggtotal <- aggregate(distance ~ district*primarytype, data=total, FUN=mean)
aggtotal$number <- aggregate(count ~ district*primarytype, data=total, FUN=sum)








library(maptools)
library(ggmap)
library(ggplot2)
gpclibPermit()
area <- readShapePoly("PoliceDistrict.shp")
mapImage <- get_map(location = c(lon = -87.6, lat = 41.8), color = "color", source = "google", zoom = 10)
areapoints <- fortify(area)
head(area)
ggmap(mapImage) + geom_polygon(aes(x = long, y = lat, group = group), data = areapoints, alpha = 0.5) + labs(x = "Longitude", y = "Latitude")


 

