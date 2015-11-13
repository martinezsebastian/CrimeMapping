library("rio")
crime <- import("https://data.cityofchicago.org/resource/qjr3-bm53.json")




## Crime data 


install.packages("RSocrata")
library("RSocrata")
#####x <-  read.socrata("https://data.cityofchicago.org/resource/vwwp-7yr9.json")





## Import of data for Control Variables 

### Census Data (2008-2012) online accessible: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2
library("rio")

Census <- import("https://data.cityofchicago.org/resource/kn9c-c2s2.json")
View(Census)



hist(as.numeric(Census$hardship_index))
PerCapitaIncome <- as.numeric(Census$per_capita_income_)



### why does the curve only work with freq=FALSE ? 
hist(PerCapitaIncome, freq=TRUE, main = "header")
curve(dnorm(x, mean=mean(PerCapitaIncome), sd=sd(PerCapitaIncome)), add=TRUE, col="darkblue", lwd=2) 