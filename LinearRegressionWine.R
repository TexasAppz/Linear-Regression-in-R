#Linear Regression
Wine = read.csv("Wine.csv")
str(Wine)

WineReg = lm(Price ~ WinterRain + AGST + HarvestRain + Age + FrancePop, data=Wine)
summary(WineReg)

# Getting Ideas to Refine the Model
cor(Wine)
Wine
Wine2<-Wine[,-1]
Wine2
MyCor <- cor(Wine2)
which.min(MyCor)
MyCor[30]
cor(Wine$Age, Wine$FrancePop)

# Refining the Model
WineReg = lm(Price ~ WinterRain + AGST + HarvestRain + Age, data=Wine)
summary(WineReg)

WineReg = lm(Price ~ 0 + WinterRain + AGST + HarvestRain + Age, data=Wine)
summary(WineReg)

WineReg = lm(Price ~ 0 + AGST + HarvestRain + Age, data=Wine)
summary(WineReg)

# Making Predictions
WineTest = read.csv("WineTest.csv")
WinePredictions = predict(WineReg, newdata=WineTest)

#Computing R^2 on Testing Set 
SSE = sum((WineTest$Price - WinePredictions)^2)
SST = sum((WineTest$Price - mean(Wine$Price))^2)
1 - SSE/SST

