

mean(as.integer(Crime_Data$per_capita_income_))
summary(as.integer(Crime_Data$per_capita_income_))
summary(Over16Unemployed)


##defining variables 
PerCapitaIncome <- as.integer(Crime_Data$per_capita_income_)
Over16Unemployed <- as.integer(Crime_Data$percent_aged_16_unemployed)
CrimeIsViolent <- as.factor(Crime_Data$violent)
CrimeIsClose <- Crime_Data$close

x <- list(PerCapitaIncome, Over16Unemployed, CrimeIsViolent, CrimeIsClose)
CrimeData <- as.data.frame(x)


library(Zelig)
Z1 <- zelig(CrimeIsClose ~ PerCapitaIncome + Over16Unemployed + CrimeIsViolent, cite = FALSE, data = CrimeData, model = 'logit')

summary(Z1)

# setting fitted values

setZ1 <- setx(Z1, Over16Unemployed = 4:22)

##Run simulation 
simZ1 <- sim(Z1, x = setZ1, num = 500)

plot(simZ1)

Crime_Data$






##setZ1 <- setx(Z1, PerCapitaIncome = 30000:40000)
##15090:88670)
