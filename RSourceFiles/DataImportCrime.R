###################################
# This file contains R Source Code to import the data from the City of Chicago Data Portal
# NOTE: Because of the files size (loading time), this is only the crime data!
# Sebastian Martinez and Fabian Bohnenberger
###################################

library("rio")


# CRIME DATA

## We use the whole data set available from the City fo Chicago Data Portal. Because of a recent change in the API, the Portal 
## only allows the user to download 1000 observations, instead of the whole set of information (335487 observations). 
## It can be downloaded from https://data.cityofchicago.org/Public-Safety/Crimes-2012/hx8q-mf9v
#crimes <- import("https://data.cityofchicago.org/resource/hx8q-mf9v.json")

## The full dataset (335487 obs.) is available as a csv-file in our repository:
crimes <- read.csv("RSourceFiles/Crimes2012.csv")




