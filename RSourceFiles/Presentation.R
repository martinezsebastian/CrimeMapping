
## NOTE: Running this file may take a few minutes because R will load ~340.000 observations (crime in the year 2012).


## Working Directory ##

# Create list of used working directories
possible_dir <- c('C:/Users/Fabian/Documents/GitHub/CrimeMapping', '/Users/SebastianMartinez/Dropbox/0. Hertie/3/Collaborative Social Science Data Analysis/GitHub/CrimeMapping/')

# Set to first valid directory in the possible_dir vector
repmis::set_valid_wd(possible_dir)

## Loading required packages ## 

# If this file does not run, check/install required r packages.
library(repmis)
library(rio)
library(RSocrata)
library(foreign)
library(rio)
library(plyr)
library(GISTools)
library(rgeos)
library(knitr)







## R code to import the respective data files is sourced from TWO separate R file. The first one contains the crime data. NOTE that running this file may take short while because R will load ~340.000 crime observations for the year 2012! The second contains import code for all other data. 

## Crime Data (NOTE: May take a few minutes)
source('RSourceFiles/DataImportCrime.R')

## Border Data & Controls 
source('RSourceFiles/DataImportRest.R')


## To access the information contained in the shapefiles, the .dbf file is used. 
## The shapefiles are loaded into R, and the Centroids of the polygons are calculated. 
boundaries <- read.dbf("RSourceFiles/PoliceDistrict.dbf")
sids <- readShapePoly("RSourceFiles/PoliceDistrict.shp")
PoliceDistrictCentroids = as.data.frame(gCentroid(sids,byid=TRUE))
stations <- read.dbf("RSourceFiles/PoliceStationsDec2012.dbf")
stations_ <- readShapePoints("RSourceFiles/PoliceStationsDec2012.shp")
stations = as.data.frame(stations_)


## A plot of the districts with the centroids is generated
plot(sids)
points(coordinates(stations_), pch=1)
points(coordinates(sids),pch=1)


## Information is saved onto the boundaries data frame to be then merged with the additional information
boundaries$centroid_x <- PoliceDistrictCentroids$x
boundaries$centroid_y <- PoliceDistrictCentroids$y


# Similarly, information on the boundaries of the communities is added. 
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




## A new dataframe named "total" is created that contains all crime and census data. NOTE: Requires loading crime data!
## Contains 335481 obs. and 30 var. 
total <- merge(crimes, census, by.x = "Community.Area", by.y = "ca")



## Data from the boundaries and the community areas with their respective centroids is merged with the crime and census data
total <- merge(total, boundaries, by.x = "District", by.y = "DIST_NUM")
total <- merge(total, commareas_dbf, by.x = "Community.Area", by.y = "AREA_NUM_1")
total <- merge(total, stations, by.x = "District", by.y = "DIST")


total$center_x <- total$centroid_x.y
total$center_y <- total$centroid_y.y
total$x <- total$X.Coordinate
total$y <- total$Y.Coordinate
total$st_location_x <- total$coords.x1
total$st_location_y <- total$coords.x2


# Absolute Distance
## The Distance from the place where a crime was committed to the center of the police district is calculated. 

total$y_dist <- total$y - total$center_y
total$x_dist <- total$x - total$center_x
total$x_st_dist <- total$x - total$st_location_x
total$y_st_dist <- total$y - total$st_location_y
total$distance <- sqrt((total$x_dist)^2+(total$y_dist)^2)
total$distance_sta <- sqrt((total$x_st_dist)^2+(total$y_st_dist)^2)
total$count <- 1
# NA values for distance are omitted from the analysis
total <- total[!(is.na(total$distance)),]
total <- total[!(is.na(total$distance_sta)),]

counter <- aggregate(count ~ District, data = total, FUN=mean)


# Relative Distance
## To determine the relative distances (crime farthest away=1)

## Data has to be adapted because distance needs to be calculated for each district separately. 

### Loop to split the data set into smaller datasets, one for every district
district_list <- list()
for(i in counter$District) 
{
  temp <- total[total[,2]==i,]
  nam <- paste("District", i, sep = "")
  assign(nam, temp)
  district_list[[nam]] <- temp
}

### Max distance is calculated and included into the data set
for(i in counter$District) 
{
  nam <- paste("District", i, sep = "")
  temp <- district_list[[nam]]
  temp$max_distance <- max(abs(temp$distance))
  temp$max_distance_sta <- max(abs(temp$distance_sta))
  assign(nam, temp)
  district_list[[nam]] <- temp
}

### Different data frames are appended into a single big data set again. 
Crime_Data <- ldply(district_list, data.frame)
Crime_Data$rel_dist <- Crime_Data$distance/Crime_Data$max_distance
Crime_Data$rel_dist_sta <- Crime_Data$distance/Crime_Data$max_distance_sta
#Crime_Data <- Crime_Data[order(Crime_Data$rel_dist),]


##################################
##################################
## Close or Far away Crime
##################################
##################################

Crime_Data$half_distance  <- 2*Crime_Data$max_distance/3
Crime_Data$close <- ifelse(Crime_Data$distance>=Crime_Data$half_distance,1,0)


##################################
##################################
#CRIME GROUPING
# CRIMES THAT WERE CONSIDERED VIOLENT
#"ASSAULT"
#"BATTERY"
#"CRIM SEXUAL ASSAULT"
#"HOMICIDE"
#"INTIMIDATION"
#"KIDNAPPING"
#"OFFENSE INVOLVING CHILDREN"
#"ROBBERY"
#"SEX OFFENSE"
##################################
##################################
Crime_Data$violent <- ifelse(Crime_Data$Primary.Type=="ASSAULT" | Crime_Data$Primary.Type=="BATTERY" | Crime_Data$Primary.Type=="CRIM SEXUAL ASSAULT" | Crime_Data$Primary.Type=="HOMICIDE" | Crime_Data$Primary.Type=="INTIMIDATION" | Crime_Data$Primary.Type=="KIDNAPPING" | Crime_Data$Primary.Type=="OFFENSE INVOLVING CHILDREN" | Crime_Data$Primary.Type=="ROBBERY" | Crime_Data$Primary.Type=="SEX OFFENSE", 1,0)


###################

violent_probit1 <- glm(Crime_Data$close ~ as.integer(Crime_Data$hardship_index) + Crime_Data$violent, family=binomial(link="probit"), data=Crime_Data)
violent_probit2 <- glm(Crime_Data$close ~  Crime_Data$violent + as.integer(Crime_Data$per_capita_income_), family=binomial(link="probit"), data=Crime_Data)
violent_model1 <- lm(Crime_Data$close ~ as.integer(Crime_Data$hardship_index) + Crime_Data$violent, data=Crime_Data)
summary(violent_probit2)
summary(violent_model1)


# Aggregate descriptive statistics
## A "Collapse" is done on the relative distance by District and by typ of crime, and then
## only to types of crime to see if there is a relationship between their occurence in relation to the 
## border of the district. 
agg_crime_type <- aggregate(rel_dist ~ District*Primary.Type, data=Crime_Data, FUN=mean)
agg_crime_type$number <- aggregate(count ~ District*Primary.Type, data=Crime_Data, FUN=sum)
agg_crime <- aggregate(rel_dist ~ Primary.Type, data=agg_crime_type, FUN=mean)
agg_crime_count <- aggregate(count ~ Primary.Type, data=Crime_Data, FUN=sum)
agg_crime <- merge(agg_crime, agg_crime_count, by = "Primary.Type")
agg_crime <- agg_crime[order(agg_crime$rel_dist),]
#write.csv(agg_crime, "agg_crime.csv")

# Aggregate descriptive statistics
## A "Collapse" is done on the relative distance by District and by typ of crime, and then
## only to types of crime to see if there is a relationship between their occurence in relation to the 
## border of the district. 
agg_crime_type_sta <- aggregate(rel_dist_sta ~ District*Primary.Type, data=Crime_Data, FUN=mean)
agg_crime_type_sta$number <- aggregate(count ~ District*Primary.Type, data=Crime_Data, FUN=sum)
agg_crime_sta <- aggregate(rel_dist_sta ~ Primary.Type, data=agg_crime_type_sta, FUN=mean)
agg_crime_sta_count <- aggregate(count ~ Primary.Type, data=Crime_Data, FUN=sum)
agg_crime_sta <- merge(agg_crime_sta, agg_crime_sta_count, by = "Primary.Type")
agg_crime_sta <- agg_crime_sta[order(agg_crime_sta$rel_dist_sta),]
#write.csv(agg_crime_sta, "agg_crime_sta.csv")


table_temp <- agg_crime
table_temp$distance_from_border_type <- agg_crime$Primary.Type
table_temp$distance_from_border <- agg_crime$rel_dist
table_temp$distance_to_station_type <- agg_crime_sta$Primary.Type
table_temp$distance_to_station <- agg_crime_sta$rel_dist_sta
table_temp$distance_from_border_number <- agg_crime$count
table_temp$distance_to_station_number <- agg_crime_sta$count
subdata1 <- c("distance_from_border_type", "distance_from_border_number")
subdata2 <- c("distance_from_border_type", "distance_from_border", "distance_from_border_number", "distance_to_station_type", "distance_to_station", "distance_to_station_number")
table1 <- table_temp[subdata1]
table2 <- table_temp[subdata2]
kable(as.matrix(table1), digits = 2, caption = "Aggregate Crime Statistics (Relative Distance to Policy District Center accross all Districts") 
kable(as.matrix(table2), digits = 2, caption = "Aggregate Crime Statistics (Relative Distance to Policy District Center accross all Districts") 





reldist <- as.numeric(agg_crime$rel_dist)
hist(reldist, freq=FALSE, main ="Relative Distance across Police Districts")
curve(dnorm(x, mean=mean(reldist), sd=sd(reldist)), add=TRUE, col="darkblue", lwd=2) 
View(agg_crime)


reldistind <- as.numeric(agg_crime_type$rel_dist)
hist(reldistind, freq=TRUE, main ="Relative Distance for individual Police Districts")
curve(dnorm(x, mean=mean(reldistind), sd=sd(reldistind)), add=TRUE, col="darkblue", lwd=2) 



## additional packages
library(maptools)
library(ggmap)
library(ggplot2)

## Source map from google maps & add data points

gpclibPermit()
area <- readShapePoly("RSourceFiles/PoliceDistrict.shp")
mapImage <- get_map(location = c(lon = -87.6, lat = 41.8), color = "color", source = "google", zoom = 10)
areapoints <- fortify(area)
head(area)
ggmap(mapImage) + geom_polygon(aes(x = long, y = lat, group = group), data = areapoints, alpha = 0.5) + labs(x = "Longitude", y = "Latitude")







