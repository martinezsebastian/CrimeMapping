library(foreign)
library(rio)
library(plyr)
library(GISTools)
library(rgeos)
setwd("/Users/SebastianMartinez/Dropbox/0. Hertie/3/Collaborative Social Science Data Analysis/GitHub/CrimeMapping/R Source files/")

# CRIME INFORMATION
## We use the whole data set available from the City fo Chicago Data Unit. There was a recent change in the API and it only allows the user to download 
## 1000 observations, instead of the whole set of information. It can be downloaded from 
## https://data.cityofchicago.org/Public-Safety/Crimes-2012/hx8q-mf9v
#crimes <- import("https://data.cityofchicago.org/resource/hx8q-mf9v.json")
crimes <- read.csv("Crimes2012.csv")

# Police Districts
## Police Districts are downloaded from
## 
## as shapefiles. To access the information contained inside, the .dbf file associated to the shapefile was used. 
## The shapefiles are loaded into R, and the Centroids of the polygons are calculated. 
boundaries <- read.dbf("PoliceDistrict.dbf")
sids <- readShapePoly("PoliceDistrict.shp")
PoliceDistrictCentroids = as.data.frame(gCentroid(sids,byid=TRUE))

## A plot of the districts with the centroids is generated
plot(sids)
points(coordinates(sids),pch=1)
points(PoliceDistrictCentroids,pch=2)
## Information is saved onto the boundaries data frame to be then merged with the additional information
boundaries$centroid_x <- PoliceDistrictCentroids$x
boundaries$centroid_y <- PoliceDistrictCentroids$y

# Similarly, information for the boundaries of the communities are also added. 
commareas_dbf <- read.dbf("CommAreas.dbf")
commareas <- readShapePoly("CommAreas.shp")
CommAreasCentroids <- as.data.frame(gCentroid(commareas,byid=TRUE))

## A plot of the community districts with the centroids is generated
plot(commareas)
points(coordinates(commareas),pch=1)
points(CommAreasCentroids,pch=2)
## Information is saved onto the boundaries data frame to be then merged with the additional information
commareas_dbf$centroid_x <- CommAreasCentroids$x
commareas_dbf$centroid_y <- CommAreasCentroids$y


# Census Data
## Data from the Census is downloaded and merged with the crimes information for 2012.
census <- import("https://data.cityofchicago.org/resource/kn9c-c2s2.json")
total <- merge(crimes, census, by.x = "Community.Area", by.y = "ca")


## Data from the boundaries and the community areas with their respective centroids is merged with the crime and census data
total <- merge(total, boundaries, by.x = "District", by.y = "DIST_NUM")
total <- merge(total, commareas_dbf, by.x = "Community.Area", by.y = "AREA_NUM_1")
### renaming
total$center_x <- total$centroid_y.x
total$center_y <- total$centroid_y.y
total$x <- total$X.Coordinate
total$y <- total$Y.Coordinate

# Distance
## The Distance from the place where a crime was committed to the center of the police district is calculated. This is done so that we can
## determine which crimes occur closer to the border of each district, and hence farthest away from the location of the police station for that district
total$y_dist <- total$y - total$center_y
total$x_dist <- total$x - total$center_x
total$distance <- sqrt((total$x_dist)^2+(total$y_dist)^2)
total$count <- 1
# NA values for distance are omitted from the analysis
total <- total[!(is.na(total$distance)),]

counter <- aggregate(count ~ District, data = total, FUN=mean)

# Relative Distance
## To determine the relative distances 
district_list <- list()
for(i in counter$District) 
{
  temp <- total[total[,2]==i,]
  nam <- paste("District", i, sep = "")
  assign(nam, temp)
  district_list[[nam]] <- temp
}

for(i in counter$District) 
{
  nam <- paste("District", i, sep = "")
  temp <- district_list[[nam]]
  temp$max_distance <- max(abs(temp$distance))
  assign(nam, temp)
  district_list[[nam]] <- temp
}


library(plyr)
Crime_Data <- ldply(district_list, data.frame)
Crime_Data$rel_dist <- Crime_Data$distance/Crime_Data$max_distance

Crime_Data <- Crime_Data[order(Crime_Data$rel_dist),]
View(Crime_Data$rel_dist)




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


 

