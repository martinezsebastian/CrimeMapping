library("rio")
crime <- import("https://data.cityofchicago.org/resource/vwwp-7yr9.json")

library("RSocrata")
x <-  read.socrata("https://data.cityofchicago.org/resource/vwwp-7yr9.json")