library(tidyr)
library(stringr)
library(dplyr)

#------------------Data preparation and exploration-----------------------------
#Read raw data
CarData <- read.csv("CarPrice_Assignment.csv")

View(CarData)

str(CarData)

summary(CarData)

#Converting Car name to character values

CarData$CarName <- as.character(CarData$CarName)

#1. Find NAs and missing values

colSums(is.na(CarData))

#Since all column sums are 0, there are no missing values

#2. Since CarId can be identified as primary key, so it should be unique for each row
# Find unique values of Car Id and if the same is equal to total number of rows

unique(CarData)
nrow(unique(CarData)) == nrow(CarData)

# Thus, the number of unique car rows is equal to the number of car rows and all rows
# are unique

#3. Separating carname and company name
CarData <- separate(CarData, CarName, into = c("Company", "CarName"), sep = " ", extra = "drop", fill = "right")
View(CarData)
head(CarData)
str(CarData)
summary(CarData)
#Checking for unique company names
unique(CarData$Company)
#There are instances of same car being spelt in different ways,
#1. vokswagen, volkswagen and vw
#2. toyota, toyouta
#3. porsche, porcshce
#4. Nissan, nissan
#5. maxda, mazda
#Converting these to standard names
which(CarData$Company == "vw")
which(CarData$Company == "vokswagen")
CarData$Company[c(which(CarData$Company == "vw"),which(CarData$Company == "vokswagen"))] <- "volkswagen"
#c(which(CarData$Company == "vw"),which(CarData$Company == "vokswagen"))

which(CarData$Company == "toyouta")
CarData$Company[which(CarData$Company == "toyouta")] <- "toyota"

which(CarData$Company == "porcshce")
CarData$Company[which(CarData$Company == "porcshce")] <- "porsche"

which(CarData$Company == "Nissan")
CarData$Company[which(CarData$Company == "Nissan")] <- "nissan"

which(CarData$Company == "maxda")
CarData$Company[which(CarData$Company == "maxda")] <- "mazda"

unique(CarData$Company)

CarData$Company <- as.factor(CarData$Company)
levels(CarData$Company)

#Converting symboling to factor
CarData$symboling <- as.factor(CarData$symboling)
levels(CarData$symboling)

#4. Outliers treatment. Detect and Cap/floor any outliers of all numeric variables
# wheelbase
quantile(CarData$wheelbase, seq(0,1,0.01))
boxplot(CarData$wheelbase, col = "red")
#There is a jump in data from the 99 percentile to the 100th percentile,
#so, the data needs to be capped at the 99th percentile value
CarData$wheelbase[which(CarData$wheelbase > 115.544)] <- 115.544
summary(CarData$wheelbase)

# carlength
quantile(CarData$carlength, seq(0,1,0.01))
#There seems to be a jump of 9 inches from 0 to 2% and from 98% to 100%
CarData$carlength[which(CarData$carlength > 199.568)] <- 199.568
CarData$carlength[which(CarData$carlength < 150)] <- 150

boxplot(CarData$carlength,col = "blue")$stat
boxplot.stats(CarData$carlength)$out

#carwidth
quantile(CarData$carwidth, seq(0,1,0.01))
#There is only 1 big jump from 0% to 1%, hence width needs to be floored at 1% value
CarData$carwidth[which(CarData$carwidth < 62.536)] <- 62.536
boxplot.stats(CarData$carwidth)$out

#carheight
quantile(CarData$carheight, seq(0,1,0.01))
#There doesn't seem to be any big jump and consequently any outliers
boxplot.stats(CarData$carheight)$out
boxplot(CarData$carheight)

#curbweight
quantile(CarData$curbweight, seq(0,1,0.01))
#there is a big jump from 0% to 1% and so curbweight will be floored at 1%
CarData$carwidth[which(CarData$curbweight < 1819.72)] <- 1819.72

boxplot.stats(CarData$curbweight)$out
plot(CarData$curbweight, CarData$price)

#boreratio
quantile(CarData$boreratio, seq(0,1,0.01))
#There is abig jump from 0% to 1%, hence boreratio will be floored at 1%
CarData$boreratio[which(CarData$boreratio < 2.9100)] <- 2.9100
boxplot(CarData$boreratio)

#stroke
quantile(CarData$stroke, seq(0,1,0.01))
#There is a big jump from 1% to 2% and also from 99% to 100%. So, stroke will be
#floored at 2% and capped at 99% values
CarData$stroke[which(CarData$stroke < 2.6400)] <- 2.6400
CarData$stroke[which(CarData$stroke > 3.9000)] <- 3.9000
boxplot(CarData$stroke)
boxplot.stats(CarData$stroke)$out
plot(CarData$stroke, CarData$price)

#compressionratio
quantile(CarData$compressionratio, seq(0,1,0.01))
#There is a big jump from 90% to 91%. However, 10%of data cannot be termed outliers
boxplot(CarData$compressionratio)
boxplot.stats(CarData$compressionratio)$out

#horsepower
quantile(CarData$horsepower, seq(0,1,0.01))
#There is a big jump from 97% to 98% and then from 99% to 100%.Hence capping at
#97%
CarData$horsepower[which(CarData$horsepower > 184.00)] <- 184.00
boxplot(CarData$horsepower)
boxplot.stats(CarData$horsepower)$out

#peakrpm
quantile(CarData$peakrpm, seq(0,1,0.01))
#There is a big jump from 99%to 100%. Hence capping at 99% value
CarData$peakrpm[which(CarData$peakrpm > 6000)] <- 6000
boxplot(CarData$peakrpm)
boxplot.stats(CarData$peakrpm)$out

#citympg
quantile(CarData$citympg, seq(0,1,0.01))
#There is a big jump from 98% to 99% and then from 99% to 100%. Hence capping at
#98% value.
CarData$citympg[which(CarData$citympg > 38.00)] <- 38.00
boxplot(CarData$citympg)
boxplot.stats(CarData$citympg)$out

#highwaympg
quantile(CarData$highwaympg, seq(0,1,0.01))
#There is abig jump from 98% to 99% and then from 995 to 100%. Hence capping at
#98% value
CarData$highwaympg[which(CarData$highwaympg > 46.92)] <- 46.92
boxplot(CarData$highwaympg)
boxplot.stats(CarData$highwaympg)$out

quantile(CarData$price, seq(0,1,0.01))
boxplot(CarData$price)
boxplot.stats(CarData$price)$out

#5.creating dummy variables
str(CarData)

#Company
dummy1 <- model.matrix(~ Company -1, data = CarData)
View(dummy1)
dummy1 <- dummy1[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("Company")], dummy1)
str(CarData)

#Symboling
dummy2 <- model.matrix(~ symboling -1, data = CarData)
View(dummy2)
dummy2 <- dummy2[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("symboling")], dummy2)
str(CarData)

#fueltype
dummy3 <- model.matrix(~ fueltype -1, data = CarData)
View(dummy3)
#dummy3 <- dummy3[, -1]
#CarData <- cbind(CarData[!colnames(CarData) %in% c("fueltype")], dummy3[,-1])
dummy3 <- as.data.frame(dummy3)
CarData$fueltypegas <- dummy3$fueltypegas
CarData <- CarData[!colnames(CarData) %in% c("fueltype")]
str(CarData)

#aspiration
dummy4 <- model.matrix(~ aspiration -1, data = CarData)
View(dummy4)
dummy4 <- as.data.frame(dummy4)
CarData$aspirationturbo <- dummy4$aspirationturbo
CarData <- CarData[!colnames(CarData) %in% c("aspiration")]
str(CarData)

#doornumber
dummy5 <- model.matrix(~ doornumber -1, data = CarData)
View(dummy5)
dummy5 <- as.data.frame(dummy5)
CarData$doornumbertwo <- dummy5$doornumbertwo
CarData <- CarData[!colnames(CarData) %in% c("doornumber")]
str(CarData)

#carbody
dummy6 <- model.matrix(~ carbody -1, data = CarData)
View(dummy6)
dummy6 <- dummy6[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("carbody")], dummy6)
str(CarData)

#drivewheel
dummy7 <- model.matrix(~ drivewheel -1, data = CarData)
View(dummy7)
dummy7 <- dummy7[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("drivewheel")], dummy7)
str(CarData)

#enginelocation
dummy8 <- model.matrix(~ enginelocation -1, data = CarData)
View(dummy8)
dummy8 <- as.data.frame(dummy8)
CarData$enginelocationrear <- dummy8$enginelocationrear
CarData <- CarData[!colnames(CarData) %in% c("enginelocation")]
str(CarData)

#enginetype
dummy9 <- model.matrix(~ enginetype -1, data = CarData)
View(dummy9)
dummy9 <- dummy9[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("enginetype")], dummy9)
str(CarData)

#cylindernumber
dummy10 <- model.matrix(~ cylindernumber -1, data = CarData)
View(dummy10)
dummy10 <- dummy10[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("cylindernumber")], dummy10)
str(CarData)

#fuelsystem
dummy11 <- model.matrix(~ fuelsystem -1, data = CarData)
View(dummy11)
dummy11 <- dummy11[, -1]
CarData <- cbind(CarData[!colnames(CarData) %in% c("fuelsystem")], dummy11)
str(CarData)
#summary(CarData$enginesize)

#6. Adding derived variables
# car horsepowerand mileage usually share an inverse relationship. So, cars
# which deliver higher horsepower per unit of city mileage might be priced higher
CarData$hpcity <- CarData$horsepower/CarData$citympg
plot(CarData$hpcity, CarData$price)

str(CarData)
# ratio1 <- CarData$horsepower/CarData$citympg
# plot(ratio1, CarData$price)
# plot(CarData$horsepower, CarData$price)
# plot(CarData$citympg, CarData$price)

# ratio2 <- CarData$highwaympg/CarData$citympg
# plot(ratio2, CarData$price)
# plot(CarData$highwaympg, CarData$price)
# plot(CarData$citympg, CarData$price)

#Ratio of highway to city mpg
CarData$highwaycity <- CarData$highwaympg/CarData$citympg
plot(CarData$highwaycity, CarData$price)

str(CarData)

#Dropping car_ID and CarName as these would be irrelevant for understanding car
#price
CarData_1 <- CarData[!colnames(CarData) %in% c("car_ID", "CarName")]
str(CarData_1)

#----------------------------Model Building-----------------------------------

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(CarData_1), 0.7*nrow(CarData_1))
car_train = CarData_1[trainindices,]
car_test = CarData_1[-trainindices,]

# Build model 1 containing all variables
carmodel_1 <- lm(price ~ .,data=car_train)
summary(carmodel_1)

library(MASS)

step <- stepAIC(carmodel_1, direction="both")

step

#Executing model provided by step
carmodel_2 <- lm(price ~ wheelbase + carlength + carwidth + curbweight + 
                   enginesize + boreratio + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + Companychevrolet + 
                   Companydodge + Companyisuzu + Companyjaguar + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_2)

#Check for multicollinearity
library(car)
vif(carmodel_2)

#Wheelbase has vif of 12.22 and also high p-value of 0.214895, hence removing
#wheelbase from the model

carmodel_3 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + boreratio + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + Companychevrolet + 
                   Companydodge + Companyisuzu + Companyjaguar + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_3)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_3)

#fuelsystemspdi has high vif of 3.227248 and also high p-value of 0.174954.
#Dropping fuelsystemspdi from the model

carmodel_4 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + boreratio + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + Companychevrolet + 
                   Companydodge + Companyisuzu + Companyjaguar + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_4)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_4)

#Companyjaguar has high vif of 3.312716 and also high p-value of 0.255726
#Dropping Companyjaguar from the model

carmodel_5 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + boreratio + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + Companychevrolet + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_5)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_5)

#boreratio has high vif of 10.842980 and also high p-value of 0.306631
#Dropping boreratio from the model

carmodel_6 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + Companychevrolet + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_6)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_6)

#Companychevrolet has high vif of 3.438319 and also high p-value of 0.264538
#Dropping Companychevrolet from the model

carmodel_7 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity + highwaycity, data = car_train)

summary(carmodel_7)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_7)

#highwaycity has high vif of 4.431304  and also high p-value of 0.067376
#Dropping highwaycity from the model

carmodel_8 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity, data = car_train)

summary(carmodel_8)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_8)

#cylindernumberfive has high vif of 8.438202  and also high p-value of 0.107214
#Dropping cylindernumberfive from the model

carmodel_9 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + horsepower + peakrpm + highwaympg + 
                   Companyaudi + Companybmw + Companybuick + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity, data = car_train)

summary(carmodel_9)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_9)

#highwaympg has high vif of 10.260405  and also high p-value of 0.129098
#Dropping highwaympg from the model

carmodel_10 <- lm(price ~ carlength + carwidth + curbweight + 
                   enginesize + horsepower + peakrpm +  
                   Companyaudi + Companybmw + Companybuick + 
                   Companydodge + Companyisuzu + Companymazda + 
                   Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                   Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                   Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                   enginetypeohc + enginetyperotor + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                   hpcity, data = car_train)

summary(carmodel_10)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_10)

#carwidth has low vif of 1.895795  but high p-value of 0.092769
#Dropping highwaympg from the model

carmodel_11 <- lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + peakrpm +  
                    Companyaudi + Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + symboling1 + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                    enginetypeohc + enginetyperotor + cylindernumberfour + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_11)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_11)

#symboling1 has low vif of 1.673413  but high p-value of 0.110980
#Dropping symboling1 from the model

carmodel_12 <- lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + peakrpm +  
                    Companyaudi + Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                    enginetypeohc + enginetyperotor + cylindernumberfour + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_12)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_12)

#drivewheelfwd has high vif of 4.181843  and high p-value of 0.105400
#Dropping drivewheelfwd from the model

carmodel_13 <- lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + peakrpm +  
                    Companyaudi + Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetypeohc + enginetyperotor + cylindernumberfour + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_13)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_13)

#curbweight has high vif of 20.474194  and relatively high p-value of 0.028864
#Dropping curbweight from the model

carmodel_14 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companyaudi + Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetypeohc + enginetyperotor + cylindernumberfour + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_14)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_14)

#Companyaudi has high vif of 2.344889  and relatively high p-value of 0.030584
#Dropping Companyaudi from the model.

carmodel_15 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetypeohc + enginetyperotor + cylindernumberfour + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_15)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_15)

#cylindernumberfour has high vif of 5.214241  and high p-value of 0.255776
#Dropping cylindernumberfour from the model.

carmodel_16 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetypeohc + enginetyperotor + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_16)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_16)

#enginetypeohc has high vif of  4.750486  and high p-value of 0.167412
#Dropping enginetypeohc from the model.

carmodel_17 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetyperotor + 
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_17)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_17)

#fuelsystem2bbl has high vif of  4.104142  and high p-value of 0.018377
#Dropping fuelsystem2bbl from the model.

carmodel_18 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetyperotor + 
                    cylindernumbersix + fuelsystemmpfi + 
                    hpcity, data = car_train)

summary(carmodel_18)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_18)

#fuelsystemmpfi has high vif of  4.603003  and relatively high p-value of 0.019467
#Dropping fuelsystemmpfi from the model.

carmodel_19 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    enginetyperotor + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_19)

#Virtually no change in R-square and Adjusted R square
#Checking vif

vif(carmodel_19)

#enginetyperotor has high vif of  2.208156  and high p-value of 0.070049
#Dropping enginetyperotor from the model.

carmodel_20 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companysubaru + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_20)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_20)

#Companysubaru has low vif of  1.440507  but high p-value of 0.066322
#Dropping Companysubaru from the model.

carmodel_21 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                    Companyporsche + Companyrenault + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_21)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_21)

#Companypeugeot has low vif of  1.679105  but relatively high p-value of 0.014278
#Dropping Companypeugeot from the model.

carmodel_22 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + Companymazda + 
                    Companymitsubishi + Companynissan + Companyplymouth + 
                    Companyporsche + Companyrenault + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_22)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_22)

#Companymazda has low vif of  1.450313  but relatively high p-value of 0.036518
#Dropping Companymazda from the model.

carmodel_23 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + 
                    Companymitsubishi + Companynissan + Companyplymouth + 
                    Companyporsche + Companyrenault + Companytoyota + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_23)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_23)

#Companytoyota has low vif of  1.616381  but relatively high p-value of 0.112792
#Dropping Companytoyota from the model.

carmodel_24 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + 
                    Companymitsubishi + Companynissan + Companyplymouth + 
                    Companyporsche + Companyrenault + 
                    Companyvolkswagen + aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_24)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_24)

#Companyvolkswagen has low vif of  1.197416 but relatively high p-value of 0.125876
#Dropping Companyvolkswagen from the model.

carmodel_25 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + 
                    Companymitsubishi + Companynissan + Companyplymouth + 
                    Companyporsche + Companyrenault + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_25)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_25)

#Companynissan has low vif of  1.145469 but relatively high p-value of 0.119907
#Dropping Companynissan from the model.

carmodel_26 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + Companyisuzu + 
                    Companymitsubishi + Companyplymouth + 
                    Companyporsche + Companyrenault + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_26)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_26)

#Companyisuzu has low vif of  1.028057 but relatively high p-value of 0.047720
#Dropping Companyisuzu from the model.

carmodel_27 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companydodge + 
                    Companymitsubishi + Companyplymouth + 
                    Companyporsche + Companyrenault + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_27)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_27)

#Companydodge has low vif of  1.143438 but relatively high p-value of 0.015950
#Dropping Companydodge from the model.

carmodel_28 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companymitsubishi + Companyplymouth + 
                    Companyporsche + Companyrenault + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_28)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_28)

#Companyplymouth has low vif of 1.105538 but relatively high p-value of 0.024351
#Dropping Companyplymouth from the model.

carmodel_29 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companymitsubishi + 
                    Companyporsche + Companyrenault + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_29)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_29)

#Companyrenault has low vif of 1.070145 but relatively high p-value of 0.014874
#Dropping Companyrenault from the model.

carmodel_30 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companymitsubishi + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_30)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_30)

#Companymitsubishihas low vif of 1.089163 but relatively high p-value of 0.005575
#Dropping Companymitsubishi from the model.

carmodel_31 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_31)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_31)

#Both horsepower and hpcity have high vifs indicating multicollinearity
cor(CarData_1$horsepower, CarData_1$hpcity)

#Since horsepower and hpcity have high correlation of 0.976, we will drop both
# one by one and see the effect of adjusted R square

carmodel_32 <- lm(price ~ carlength + 
                    enginesize + horsepower + peakrpm +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix, data = car_train)

summary(carmodel_32)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_32)

#Dropping hpcity makes horsepower insignificant but also decrease Adjusted R square
# by a couple of points

#Dropping horsepower instead of hpcity

carmodel_33 <- lm(price ~ carlength + 
                    enginesize + peakrpm +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    cylindernumbersix + 
                    hpcity, data = car_train)

summary(carmodel_33)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_33)

#Dropping horsepower has decreased Adjusted R square by lesser points but significance
#of hpcity is retained while slao reducing vif of hpcity to 8.812312
#Hence we will retain this model

#cylindernumbersix has high vif of 2.284877 and high p-value of 0.186318
#Dropping cylindernumbersix from the model.

carmodel_34 <- lm(price ~ carlength + 
                    enginesize + peakrpm +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_34)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_34)

#peakrpm has high vif of 2.308869 and high p-value of  0.23432
#Dropping peakrpm from the model.

carmodel_35 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    aspirationturbo + carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_35)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_35)

#aspirationturbo has low vif of 1.288084 but high p-value of  0.43038
#Dropping aspirationturbo from the model.

carmodel_36 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    carbodyhardtop + 
                    carbodyhatchback + carbodysedan + carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_36)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_36)

#carbodysedan has high vif of 11.619416 and high p-value of  0.01227
#Dropping carbodysedan from the model.

carmodel_37 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    carbodyhardtop + 
                    carbodyhatchback + carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_37)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_37)

#carbodyhardtop has low vif of 1.284110 but high p-value of  0.0601
#Dropping carbodyhardtop from the model.

carmodel_38 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    carbodyhatchback + carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_38)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_38)

#carbodyhatchback has low vif of  1.582427 but high p-value of  0.090152
#Dropping carbodyhatchback from the model.

carmodel_39 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    carbodywagon + 
                    hpcity, data = car_train)

summary(carmodel_39)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_39)

#carbodywagon has low vif of  1.166594  but high p-value of  0.0965
#Dropping carbodywagon from the model.

carmodel_40 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    hpcity, data = car_train)

summary(carmodel_40)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_40)

#only enginesize, hpcity and carlength have high vifs
cor(CarData_1$enginesize, CarData_1$hpcity)
cor(CarData_1$carlength, CarData_1$hpcity)
cor(CarData_1$carlength, CarData_1$enginesize)

#Since all three variables are highly correlated, their effect on model can be seen
# by dropping them 1 by 1.

carmodel_41 <- lm(price ~ carlength + 
                    enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche, data = car_train)

summary(carmodel_41)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_41)

carmodel_42 <- lm(price ~ carlength + 
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    hpcity, data = car_train)

summary(carmodel_42)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_42)

carmodel_43 <- lm(price ~ enginesize +  
                    Companybmw + Companybuick + 
                    Companyporsche + 
                    hpcity, data = car_train)

summary(carmodel_43)

#Very slight decrease in R-square and Adjusted R square
#Checking vif

vif(carmodel_43)

#Best values of adjusted r squared and vifs are obtained from model 42.
#Hence retaining model 42

#--------------------------Model Validation------------------------------------

# predicting the results in test dataset
Predict <- predict(carmodel_42,car_test[,!colnames(car_test) %in% c("price")])
car_test$test_price <- Predict

# Now, we need to test the r square between actual and predicted prices. 
r <- cor(car_test$price,car_test$test_price)
rsquared <- cor(car_test$price,Predict_9)^2
rsquared

