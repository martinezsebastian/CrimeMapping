
table(agg_crime)




reldist <- as.numeric(agg_crime$rel_dist)
hist(reldist, freq=FALSE, main ="Relative Distance between Crime Types and District Center across Police Districts")
curve(dnorm(x, mean=mean(reldist), sd=sd(reldist)), add=TRUE, col="darkblue", lwd=2) 

reldistind <- as.numeric(agg_crime_type$rel_dist)
hist(reldistind, freq=FALSE, main ="Relative Distance between Crime Types and District Center across Police Districts")
curve(dnorm(x, mean=mean(reldistind), sd=sd(reldistind)), add=TRUE, col="darkblue", lwd=2) 




PerCapitaIncome <- as.numeric(Census$per_capita_income_)



### why does the curve only work with freq=FALSE ? 
hist(PerCapitaIncome, freq=TRUE, main = "header")
curve(dnorm(x, mean=mean(PerCapitaIncome), sd=sd(PerCapitaIncome)), add=TRUE, col="darkblue", lwd=2) 