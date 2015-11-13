###################################
# This file contains R Source Code to import the data from the City of Chicago Data Portal
# NOTE: It excludes the crime data that is loaded in a separate file!
# Sebastian Martinez and Fabian Bohnenberger
###################################

library("rio")


## POLICE DISTRICTS DATA

## Police Districts are downloaded from the City fo Chicago Data Portal as shapefiles. 
## Because we did not want to create unnecessary traffic, the files were saved in the GitHub repository and are sources from there. 
## The original files can be imported from: https://data.cityofchicago.org/Public-Safety/Boundaries-Police-Districts-current-/fthy-xz3r (police districts) 
## and https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6 (community districts)

## To access the information contained inside, the .dbf file associated to the shapefile was used. 


## CONTROL VARIABLES DATA FOR REGRESSION

# Census Data (2008-2012)
## Data from the Census is downloaded and merged with the crimes information for 2012.
census <- import("https://data.cityofchicago.org/resource/kn9c-c2s2.json")
