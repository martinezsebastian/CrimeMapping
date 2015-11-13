


hist(as.numeric(Census$hardship_index))
PerCapitaIncome <- as.numeric(Census$per_capita_income_)



### why does the curve only work with freq=FALSE ? 
hist(PerCapitaIncome, freq=TRUE, main = "header")
curve(dnorm(x, mean=mean(PerCapitaIncome), sd=sd(PerCapitaIncome)), add=TRUE, col="darkblue", lwd=2) 