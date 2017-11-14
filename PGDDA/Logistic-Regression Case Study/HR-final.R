############################# HR Analytics Case Study ###################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# The company XYZ has a database of employee data on various parameters like Age,
# Gender, Income, Promotion, years, attrition etc.

## AIM:

# The aim is to identify major factors responsible for attrition so that management
# can take some actions on them to reduce attrition

################################################################

# Data Understanding
#------------------Data Understanding and preprocessing ----------------------------
#Read raw data

# The following packages are loaded assuming they are already installed in the 
# R environment. If not, they can be installed using the command 
# "install.packages("package-name", dependencies = TRUE)"

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(GGally)

general <- read.csv("general_data.csv")
e_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
m_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE, colClasses = c(NA, rep("POSIXct", 261)))
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, colClasses = c(NA, rep("POSIXct", 261)))

str(general)
summary(general)

str(e_survey)
summary(e_survey)

str(m_survey)
summary(m_survey)

str(in_time)
summary(in_time)

str(out_time)
summary(out_time)

#Check for if there are any duplicate employeed ids in the general dataset
length(unique(general$EmployeeID)) == nrow(general)
sum(is.na(general$EmployeeID))

#Similarly for in_time and out_time
length(unique(in_time$X)) == nrow(in_time)
sum(is.na(in_time$X))

length(unique(out_time$X)) == nrow(out_time)
sum(is.na(out_time$X))

#Checking if all 3 columns are equal
all.equal(general$EmployeeID, in_time$X, out_time$X)

in_nas <- which(is.na(in_time))
out_nas <- which(is.na(out_time))
all.equal(in_nas, out_nas)

# Since the position of nas in both the datasets are same, it can be concluded
# that the days on which both the in-time and out-time are nas, the employee 
# was on leave and the if the full day was NA, those columns can be taken as 
# company holiday

#Subtract intime from outime to get the hours in office
worktime <- out_time - in_time
worktime$X <- in_time$X
worktime1 <- as.data.frame(lapply(worktime, as.numeric))
worktime1 <- round(worktime1,1)

#For dates where all employees have na values, those must be company holidays
# and hence they need to be removed from the dataframe
allnas <- apply(worktime1[-1],2, function(x) all(is.na(x)))
nacols <- names(allnas[allnas == TRUE])

worktime1 <- worktime1[!colnames(worktime1) %in% nacols]

# Wherever the columns have nas will correspond to leaves taken by employees
# These can be counted to give the total leaves taken by employees 
worktime1$leaves <- rowSums(is.na(worktime1[-1]))
head(worktime1$leaves)

# Find total hours, average monthly hours and average daily hours
worktime1$Thours <- rowSums(worktime1[-1], na.rm = TRUE)
worktime1$Mhours <- worktime1$Thours/12
worktime1$Dhours <- worktime1$Thours/249

#Extracting the columns to be joined to general data
worktime2 <-worktime1[colnames(worktime1) %in% c("X","leaves","Thours","Mhours", "Dhours")]

#Combining all the datasets by empid 
#Check for uniqueness of employee id in the datasets and presence of any NAs 
#in employee id
length(unique(general$EmployeeID)) == nrow(general)
sum(is.na(general$EmployeeID))

length(unique(e_survey$EmployeeID)) == nrow(e_survey)
sum(is.na(e_survey$EmployeeID))

length(unique(m_survey$EmployeeID)) == nrow(m_survey)
sum(is.na(m_survey$EmployeeID))

length(unique(worktime2$X)) == nrow(worktime2)
sum(is.na(worktime2$X))

length(unique(general$EmployeeID)) == length(unique(e_survey$EmployeeID))
length(unique(general$EmployeeID)) == length(unique(m_survey$EmployeeID))  
length(unique(general$EmployeeID)) == length(unique(worktime2$X))

setdiff(general$EmployeeID, e_survey$EmployeeID)
setdiff(general$EmployeeID, m_survey$EmployeeID)
setdiff(general$EmployeeID, worktime2$X)

nrow(general) == nrow(e_survey)
nrow(general) == nrow(m_survey)
nrow(general) == nrow(worktime2)

#Combining all 4 datasets together
general <- general[order(general$EmployeeID),]
e_survey <- e_survey[order(e_survey$EmployeeID),]
m_survey <- m_survey[order(m_survey$EmployeeID),]
worktime2 <- worktime2[order(worktime2$X),]
hr_total <- general %>% inner_join(e_survey, by = "EmployeeID") %>%
                 inner_join(m_survey, by = "EmployeeID") %>%
                 inner_join(worktime2, by = c("EmployeeID" = "X"))


#General checks for merged dataset
nrow(hr_total)

#------------------Data Preparation & EDA ----------------------------
#1. Since EmployeeId is primary key, so it should be unique for each row
# Find unique values of Employee Id and if the same is equal to total number of rows

unique(hr_total$EmployeeID)
length(unique(hr_total$EmployeeID)) == nrow(hr_total)

# 2.Find NAs and missing values
colSums(is.na(hr_total))
table(is.na(hr_total))

narows <- which(rowSums(is.na(hr_total)) > 0)
length(narows)

table(hr_total$Attrition)
table(hr_total[narows,]$Attrition)

#Since 110 rows have nas and they form 110/4410 = 2.49%, a very small percentage
# of total points. Also the overall percentage of attrition in the dataset is 16.12%
# while in the rows containing na values, this percentage is 14.54% which is less
# than the overall percentage
# So, these rows can safely be removed from the dataset.

hr_total1 <-hr_total[-(narows),]
table(is.na(hr_total1))
sum(is.na(hr_total1))

str(hr_total1)
summary(hr_total1)

#Following data variables are really categories and should be converted to categorical
#variables: Education, JobLevel, StockOptionLevel, EnvironmentSatisfaction,
#JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating

categorical <- c("Education", "JobLevel", "StockOptionLevel", "EnvironmentSatisfaction",
                 "JobSatisfaction", "WorkLifeBalance", "JobInvolvement","PerformanceRating")

hr_total1[,categorical] <- lapply(hr_total1[,categorical], as.factor)
str(hr_total1)
summary(hr_total1)

# ----------------------Data visualization---------------------------------
# 1. Attrition
counts <- as.data.frame(dplyr::count(hr_total1, Attrition))
ggplot(counts, aes(x = Attrition, y = n, fill = Attrition)) + geom_bar(stat = "identity")

# 2. Plotting Attrition by BusinessTravel, Department, Education and EducationField
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(hr_total1, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(hr_total1, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

# 3. Plotting Attrition by Gender, JobLevel, JobRole and MaritalStatus
plot_grid(ggplot(hr_total1, aes(x=Gender,fill=Attrition))+ geom_bar(), 
          ggplot(hr_total1, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# 4. Plotting Attrition by StockOptionLevel, EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance
plot_grid(ggplot(hr_total1, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(), 
          ggplot(hr_total1, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_total1, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# 5. Plotting Attrition by JobInvolvement and PerformanceRating
plot_grid(ggplot(hr_total1, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(), 
          ggplot(hr_total1, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

#Bar plots reveal strong contrast for Job Level, Marital Status, EnvironmentSatisfaction, JobSatisfaction
# and moderate levels for EducationField, StockOptionLevel.

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# 1. Age
plot_grid(ggplot(hr_total1, aes(Age))+ geom_histogram( fill = "red", color = "black", binwidth = 5),
          ggplot(hr_total1, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#2. DistanceFromHome
plot_grid(ggplot(hr_total1, aes(DistanceFromHome))+ geom_histogram( fill = "orange", color = "black", binwidth = 3),
          ggplot(hr_total1, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#3. MonthlyIncome
plot_grid(ggplot(hr_total1, aes(MonthlyIncome))+ geom_histogram( fill = "green", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#4. NumCompaniesWorked
plot_grid(ggplot(hr_total1, aes(NumCompaniesWorked))+ geom_histogram( fill = "yellow", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#5. PercentSalaryHike
plot_grid(ggplot(hr_total1, aes(PercentSalaryHike))+ geom_histogram( fill = "grey", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#6. TotalWorkingYears
plot_grid(ggplot(hr_total1, aes(TotalWorkingYears))+ geom_histogram( fill = "purple", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#7. TrainingTimesLastYear
plot_grid(ggplot(hr_total1, aes(TrainingTimesLastYear))+ geom_histogram( fill = "black", color = "white", bins = 10),
          ggplot(hr_total1, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#8. YearsAtCompany
plot_grid(ggplot(hr_total1, aes(YearsAtCompany))+ geom_histogram( fill = "blue", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#9. YearsSinceLastPromotion
plot_grid(ggplot(hr_total1, aes(YearsSinceLastPromotion))+ geom_histogram( fill = "brown", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#10. YearsWithCurrManager
plot_grid(ggplot(hr_total1, aes(YearsWithCurrManager))+ geom_histogram( fill = "magenta", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#11. leaves
plot_grid(ggplot(hr_total1, aes(leaves))+ geom_histogram( fill = "pink", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#12. Thours
plot_grid(ggplot(hr_total1, aes(Thours))+ geom_histogram( fill = "violet", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=Thours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#13. Mhours
plot_grid(ggplot(hr_total1, aes(Mhours))+ geom_histogram( fill = "red", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=Mhours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#14. Dhours
plot_grid(ggplot(hr_total1, aes(Dhours))+ geom_histogram( fill = "orange", color = "black", bins = 10),
          ggplot(hr_total1, aes(x="",y=Dhours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Variables  MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, TrainingTimesLastYear,
# YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Thours, Mhours
# and Dhours have outliers.

# Boxplots of numeric variables relative to attrition
# 1. Age, DistanceFromHome, MonthlyIncome and NumCompaniesWorked
plot_grid(ggplot(hr_total1, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_total1, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Attrition seems to be more in lower age groups and more number of companies worked

# 2. PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear and YearsAtCompany
plot_grid(ggplot(hr_total1, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_total1, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Attrition seems to be more in lower TotalWorkingYears and YearsAtCompany

# 3. YearsSinceLastPromotion, YearsWithCurrManager, leaves and Thours
plot_grid(ggplot(hr_total1, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_total1, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=leaves, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=Thours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Attrition seems to be more in lower YearsWithCurrManager and higher Thours

# 4. Mhours and Dhours
plot_grid(ggplot(hr_total1, aes(x=Attrition,y=Mhours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_total1, aes(x=Attrition,y=Dhours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Attrition seems to be more in higher Mhours and higher Dhours

# Correlation between numeric variables
ggpairs(hr_total1[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked",
                      "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear",
                      "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager",
                      "leaves", "Thours", "Mhours", "Dhours")])

#Thours, Mhours and Dhours have high correlation,
#YearsAtCompany, YearsSinceLastPromotion and YearsWithCurrManager have high correlation,
#Age, YearsAtCompany and TotalWorkingYears have high correlation.

#-----------Data Preparation ------------------------------------

# Outliers treatment. Detect and Cap/floor any outliers of all numeric variables
sapply(hr_total1[,c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked", "PercentSalaryHike")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is big jump in monthly income from 90% to 91% and again from 91% to 92%.
#However, 10% of data cannot be termed outliers, hence they will be kept as such.
#There are no outliers in the remaining 3 variables

sapply(hr_total1[,c("TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany", "YearsSinceLastPromotion")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is a big jump in Total Working Years from 98% to 99% and then to 100%.
# Hence capping the data at 98%
hr_total1$TotalWorkingYears[which(hr_total1$TotalWorkingYears > 32)] <- 32

#There are some outliers in the boxplot of TrainingTimesLastYear but no big jumps
#in the data. Hence the training times can be retained as such.

#There is a big jump in the YearsAtCompany data from 97% to 98% and thereafter.
#Hence, capping the YearsAtCompany data at 97%
hr_total1$YearsAtCompany[which(hr_total1$YearsAtCompany > 22)] <- 22

#There is significant increase in YearsSinceLastPromotion at 97% and thereafter.
#Hence capping the Years since last promotion data to 97%ile
hr_total1$YearsSinceLastPromotion[which(hr_total1$YearsSinceLastPromotion > 11)] <- 11

sapply(hr_total1[,c("YearsWithCurrManager","leaves","Thours", "Mhours", "Dhours")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is a big jump in the YearsWithCurrManager data from 99% to 100%.
#Hence, capping the YearsWithCurrManager data at 99%
hr_total1$YearsWithCurrManager[which(hr_total1$YearsWithCurrManager > 14)] <- 14

#There are no jumps for leaves data. Hence, it can be left as such.

#There is significant increase in Thours data from 98% to 99% and thereafter. Hence,
#capping the data at 98%
hr_total1$Thours[which(hr_total1$Thours > 2596.5)] <- 2596.5

#There is significant increase in Mhours data from 98% to 99% and thereafter. Hence,
#capping the data at 98%
hr_total1$Mhours[which(hr_total1$Mhours > 216.3)] <- 216.3

#There is significant increase in Dhours data from 98% to 99% and thereafter. Hence,
#capping the data at 98%
hr_total1$Dhours[which(hr_total1$Dhours > 10.4)] <- 10.4

str(hr_total1)

#Following variables have either same values or are Nominal in nature and do not
#add value for statistical analysis. Hence, these variables should be removed from
#the dataset: EmployeeCount, EmployeeID, Over18, StandardHours

hr_total2 <- hr_total1[, !colnames(hr_total1) %in% c("EmployeeCount", "EmployeeID",
                                                    "Over18", "StandardHours")]
str(hr_total2)

#Normalizing continuous variables
continuous <- c("Age", "DistanceFromHome","MonthlyIncome", "NumCompaniesWorked",
                "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear",
                "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager",
                "leaves", "Thours", "Mhours", "Dhours")

hr_total2[,continuous] <- lapply(hr_total2[,continuous], scale)

str(hr_total2)
summary(hr_total2)

# converting target variable attrition from No/Yes character to numerical with levels 0/1 
hr_total2$Attrition <- ifelse(hr_total2$Attrition == "Yes",1,0)

# Checking attrition rate of employees
attr <- sum(hr_total2$Attrition)/nrow(hr_total2)
attr #attrition rate 16.16%

#Creating dummy variables for categorical variables
#Taking all categorical variables for dummy variable creation
cat_all <- c(categorical, "BusinessTravel", "Department", "EducationField", "Gender",
             "JobRole", "MaritalStatus")
cat_all 

#Creating dataset for categorical variables
hr_cat <- hr_total2[,colnames(hr_total2) %in% cat_all]

#Creating dataset for continuous variables
hr_cont <- hr_total2[,!colnames(hr_total2) %in% cat_all]

# creating dummy variables for categorical attributes
dummies<- data.frame(sapply(hr_cat, 
                            function(x) data.frame(model.matrix(~x-1,data = hr_cat))[,-1]))

str(dummies)

# Final dataset
hr_final<- cbind(hr_cont,dummies) 
str(hr_final)
View(hr_final) #4300 obs. of  59 variables

#-----------------------Model Building--------------------------------------
# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_final$Attrition, SplitRatio = 0.7)

hr_train = hr_final[indices,]

hr_test = hr_final[!(indices),]

# Logistic Regression: 
#Initial model
hrmodel_1 = glm(Attrition ~ ., data = hr_train, family = "binomial")
summary(hrmodel_1) #AIC 2105.6....59 coeff..nullDev 2661.4...resDev 1987.6

# Stepwise selection
library("MASS")
hrmodel_2<- stepAIC(hrmodel_1, direction="both")

summary(hrmodel_2)

# Removing multicollinearity through VIF check
library(car)
vif(hrmodel_2)

#EducationField.xLife.Sciences has high VIF value but also high significance
#Still removing the variable might reveal the significance of other variables

hrmodel_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + Dhours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + MaritalStatus.xMarried + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_3)
vif(hrmodel_3)

#BusinessTravel.xTravel_Rarely has high vif but lower significance
#Hence, removing BusinessTravel.xTravel_Rarely

hrmodel_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + Dhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + MaritalStatus.xMarried + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_4)
vif(hrmodel_4)

#MaritalStatus.xMarried has high vif and is also insignificant. Hence, removing
#MaritalStatus.xMarried from the model

hrmodel_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + Dhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_5)
vif(hrmodel_5)

#Dhours and Mhours have high vif but are also significant. They are also highly
#Correlated, hence removing Dhours will give a clearer picture

hrmodel_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_6)
vif(hrmodel_6)

#Now Mhours has low vif and is also highly significant
#WorkLifeBalance.x4 has high vif but is also significant. However it has lowest
#signficance among all worklife levels. Hence removing WorkLifeBalance.x4

hrmodel_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_7)
vif(hrmodel_7)

#TotalWorkingYears has high vif and even though high significance but this might
#be due to correlation with some other variable. Hence, removing TotalWorkingYears

hrmodel_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 + EducationField.xMarketing + 
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_8)
vif(hrmodel_8)

#Now vifs of all vriables are less than 2, hence now removing based on insignificance
#Removing EducationField.xMarketing as it is highly insignificant

hrmodel_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 +
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_9)
vif(hrmodel_9)

#Removing JobLevel.x5 as it is highly insignificant

hrmodel_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   Mhours + BusinessTravel.xTravel_Frequently + 
                   Education.x5 +
                   EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   JobRole.xSales.Representative + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hr_train)

summary(hrmodel_10)
vif(hrmodel_10)

#Removing EducationField.xMedical as it is highly insignificant

hrmodel_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    Education.x5 +
                    EducationField.xOther + EducationField.xTechnical.Degree + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_11)
vif(hrmodel_11)

#Removing EducationField.xOther as it is highly insignificant
hrmodel_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    Education.x5 +
                    EducationField.xTechnical.Degree + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_12)
vif(hrmodel_12)

#Removing EducationField.xTechnical.Degree as it is highly insignificant
hrmodel_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    Education.x5 +
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_13)
vif(hrmodel_13)

#Removing StockOptionLevel.x3 as it is highly insignificant
hrmodel_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    Education.x5 +
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_14)
vif(hrmodel_14)

#Removing JobRole.xManufacturing.Director as it is highly insignificant
hrmodel_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    Education.x5 +
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_15)
vif(hrmodel_15)

#Removing Education.x5 as it is insignificant
hrmodel_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle + StockOptionLevel.x1 +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_16)
vif(hrmodel_16)

#Removing StockOptionLevel.x1 as it is insignificant
hrmodel_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    JobRole.xSales.Representative + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_17)
vif(hrmodel_17)

#Removing JobRole.xSales.Representative as it is insignificant
hrmodel_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_18)
vif(hrmodel_18)

#Removing JobLevel.x2 as it is insignificant
hrmodel_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_19)
vif(hrmodel_19)

#Removing WorkLifeBalance.x2  as it is insignificant
hrmodel_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    JobInvolvement.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_20)
vif(hrmodel_20)

#Removing JobInvolvement.x3  as it is comparatively insignificant
hrmodel_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_21)
vif(hrmodel_21)

#Removing JobRole.xLaboratory.Technician  as it is comparatively insignificant
hrmodel_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_22)
vif(hrmodel_22)

#Removing JobRole.xResearch.Scientist  as it is comparatively insignificant
hrmodel_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xResearch.Director + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_23)
vif(hrmodel_23)

#Removing JobRole.xSales.Executive as it is comparatively insignificant
hrmodel_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    JobRole.xResearch.Director +
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_24)
vif(hrmodel_24)

#Removing JobRole.xResearch.Director as it is comparatively insignificant
hrmodel_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_25)
vif(hrmodel_25)

#Removing JobSatisfaction.x2 as it is comparatively insignificant
hrmodel_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_26)
vif(hrmodel_26)

#Removing JobSatisfaction.x3 as it is comparatively insignificant
hrmodel_27 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_27)
vif(hrmodel_27)

#Removing TrainingTimesLastYear as it is comparatively insignificant
hrmodel_28 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +
                    JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                  data = hr_train)

summary(hrmodel_28)
vif(hrmodel_28)

#Removing WorkLifeBalance.x3 as it is comparatively insignificant
hrmodel_29 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    Mhours + BusinessTravel.xTravel_Frequently + 
                    MaritalStatus.xSingle +
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +
                    JobSatisfaction.x4, family = "binomial", 
                  data = hr_train)

summary(hrmodel_29)
vif(hrmodel_29)

#Now, vif of all the variables is under 2 and all the variables are highly
# significant. Hence, this model can be taken as the final model

hr_finalmodel <- hrmodel_29

##------------------------Model interpretation------------------------------------

# Model variaables can interpreted as follows:-
#
# 1) Age: Coefficient of Age is negative meaning an increase in age results in the decrease in probability of attrition of employee.
#
# 2) Number of companies worked: Coefficient of Number of companies worked is positive meaning more the number of companies 
#   an employee has worked in, more the probability of him/her leaving.
#
# 3) Years Since Last Promotion: Coefficient of Years Since Last Promotion is positive meaning more the number of years since an employee was promoted, more is the probability of him/her leaving.
#
# 4) Years With Current Manager: Years With Current Manager has negative coefficient meaning more the number of years an employee has spent with his/her current manager, lesser the probability of him/her leaving.
#
# 5) Average Monthly Hours:  Average Monthly Hours has positive coefficient meaning more the average monthly hours of an employee, more is he/she likely to leave
#
# 6) BusinessTravel.xTravel_Frequently: This variable has positive coefficient. So, an employee who has to travel frequently for business has a higher probability of leaving than an employee who does not travel.
#
# 7) MaritalStatus.xSingle: This variable has positive coefficient. So, an employee who is single has a higher probability of leaving than a married employee.
#
# 8) Environment Satisfaction: This variable has negative coefficient for increasing levels of 2,3, and 4. So, an employee who has a medium, high or very high work environment satisfaction level is less likely to leave than an employee whose satisfaction level is low.
#
# 9) Job Satisfaction: This variable has negative coefficient for highest level 4 meaning an employee whose job satisfaction level is very high is less likely to leave.
#
#--------------------------Model Evaluation--------------------------------------
### Test Data ####
#predicted probabilities of attrition for test data

hr_test_pred = predict(hr_finalmodel, type = "response", 
                    newdata = hr_test[,!colnames(hr_test) %in% c("Attrition")])
# Check summary 
summary(hr_test_pred)

hr_test$prob <- hr_test_pred
View(hr_test)
# Let's use the probability cutoff of 50%.

test_pred_attn <- factor(ifelse(hr_test_pred >= 0.50, "Yes", "No"))
test_actual_attn <- factor(ifelse(hr_test$Attrition == 1,"Yes","No"))


table(test_actual_attn, test_pred_attn)

#######################################################################
test_pred_attn <- factor(ifelse(hr_test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

hr_test_conf <- confusionMatrix(test_pred_attn, test_actual_attn, positive = "Yes")
hr_test_conf

# Choosing the cutoff value. 
# Finding the optimal probalility cutoff 

hr_perform_fn <- function(cutoff) 
{
  predicted_attn <- factor(ifelse(hr_test_pred >= cutoff, "Yes", "No"))
  hr_conf <- confusionMatrix(predicted_attn, test_actual_attn, positive = "Yes")
  hr_acc <- hr_conf$overall[1]
  hr_sens <- hr_conf$byClass[1]
  hr_spec <- hr_conf$byClass[2]
  hr_out <- t(as.matrix(c(hr_sens, hr_spec, hr_acc))) 
  colnames(hr_out) <- c("sensitivity", "specificity", "accuracy")
  return(hr_out)
}

# Summary of test probability

summary(hr_test_pred)

s = seq(.01,.80,length=100)

HR_OUT = matrix(0,100,3)

for(i in 1:100)
{
  HR_OUT[i,] = hr_perform_fn(s[i])
} 

plot(s, HR_OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,HR_OUT[,2],col="darkgreen",lwd=2)
lines(s,HR_OUT[,3],col=4,lwd=2)
box()
legend(0.1,0.4,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"), cex = 0.5)

#Choosing a cutoff value
hr_cutoff <- s[which(abs(HR_OUT[,1]-HR_OUT[,2])<0.01)]

#Choosing a cutoff value of 0.1696 
test_cutoff_attn <- factor(ifelse(hr_test_pred >= 0.1696, "Yes", "No"))

hr_conf_final <- confusionMatrix(test_cutoff_attn, test_actual_attn, positive = "Yes")

hr_acc <- hr_conf_final$overall[1]

hr_sens <- hr_conf_final$byClass[1]

hr_spec <- hr_conf_final$byClass[2]

hr_acc

hr_sens

hr_spec

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attn <- ifelse(test_cutoff_attn=="Yes",1,0)
test_actual_attn <- ifelse(test_actual_attn=="Yes",1,0)


library(ROCR)
#on testing  data
hr_pred_object_test<- prediction(test_cutoff_attn, test_actual_attn)

hr_performance_measures_test<- performance(hr_pred_object_test, "tpr", "fpr")

hr_ks_table_test <- attr(hr_performance_measures_test, "y.values")[[1]] - 
  (attr(hr_performance_measures_test, "x.values")[[1]])

max(hr_ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attn, hr_test_pred, groups = 10)

ROCRlog = prediction(hr_test_pred, hr_test$Attrition)

# Performance function
ROCRtestlogperf = performance(ROCRlog, "tpr", "fpr")

# Plot ROC curve
plot(ROCRtestlogperf, colorize=TRUE)

as.numeric(performance(ROCRlog, "auc")@y.values)

#The AUC comes out to be 0.8001673 indicating high accuracy
