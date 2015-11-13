


## A plot of the districts with the centroids is generated
plot(sids)
points(coordinates(sids),pch=1)
points(PoliceDistrictCentroids,pch=2)
## Information is saved onto the boundaries data frame to be then merged with the additional information
boundaries$centroid_x <- PoliceDistrictCentroids$x
boundaries$centroid_y <- PoliceDistrictCentroids$y

# Similarly, information for the boundaries of the communities are also added. 
commareas_dbf <- read.dbf("RSourceFiles/CommAreas.dbf")
commareas <- readShapePoly("RSourceFiles/CommAreas.shp")
CommAreasCentroids <- as.data.frame(gCentroid(commareas,byid=TRUE))

## A plot of the community districts with the centroids is generated
plot(commareas)
points(coordinates(commareas),pch=1)
points(CommAreasCentroids,pch=2)
## Information is saved onto the boundaries data frame to be then merged with the additional information
commareas_dbf$centroid_x <- CommAreasCentroids$x
commareas_dbf$centroid_y <- CommAreasCentroids$y
