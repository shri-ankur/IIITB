
##------------Bank Marketing Analysis without "duration"---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------


#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#---------------------------------------------------------------------------
## Adding Prospect id and call cost variables
nrow(bank_data)
bank_data$p_id = paste0("P_",seq(1:nrow(bank_data)))

bank_data$call_cost = 0.033 * bank_data$duration + 0.8
#---------------------------------------------------------------------------

## Model Building   

##---------Logistic Regression----------#

# Required Packages

library(caret)
library(caTools)
library(dummies)

#---------------------------------------------------------    

# Removing binning variables, duration, prospect id and call cost during model
# buiding
removal <- c("duration", "binning.age", "p_id", "call_cost")
  
#Removing duration variable 
# bank_data1 <- bank_data[!colnames(bank_data) %in% c("duration")]

bank_data1 <- bank_data

#creating dummy variables

bank_data1$response <- as.integer(bank_data1$response)

k1 <- bank_data1

bank_data1 <- dummy.data.frame(bank_data1[, !colnames(bank_data) %in% removal])

#Adding back the removed variables
bank_data1$p_id <- k1$p_id
bank_data1$duration <- k1$duration
bank_data1$call_cost <- k1$call_cost

#binning variable is not added as it is not required further

bank_data1$response <- as.factor(ifelse(bank_data1$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data1$response, SplitRatio = 0.70)

train <- bank_data1[split_indices, ]

test <- bank_data1[!split_indices, ]

nrow(train)/nrow(bank_data1)

nrow(test)/nrow(bank_data1)

#Removing the prospect_id, duration and call cost variables from train dataset
#as it won't be required for buidling logistic regression model
train <- train[, !colnames(train) %in% removal]

#---------------------------------------------------------    

### Model 1: Logistic Regression


library(MASS)

library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train)



# checking vif for logistic_2 

vif(logistic_2)

summary(logistic_2)

#---------------------------------------------------------    

# removing "previousLess_than_3_times"since the variable is most insignificant 

logistic_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

summary(logistic_3)

vif(logistic_3)

# Removing "jobadmin" variable as it is most insignificant.

logistic_4 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

summary(logistic_4)

vif(logistic_4)

# Removing "maritaldivorced" variable as it is most insignificant.

logistic_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)


summary(logistic_5)

vif(logistic_5)

# Removing "day_of_weekfri" variable as it is most insignificant.

logistic_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)


summary(logistic_6)

vif(logistic_6)

# Removing "educationTertiary_Education" variable as it is most insignificant.
logistic_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

summary(logistic_7)

vif(logistic_7)

# Removing "jobtechnician " variable as it is most insignificant.
logistic_8 <-  glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + monthnov + monthoct + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

summary(logistic_8)

vif(logistic_8)

# Removing "monthoct" variable as it is most insignificant.

logistic_9 <- glm(formula = response ~ jobretired + jobstudent +
                    educationPrimary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)


summary(logistic_9)

vif(logistic_9)

# Removing "monthjul" variable as it is most insignificant and also has a high vif.

logistic_10 <- glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)


summary(logistic_10)

vif(logistic_10)


# Removing "monthapr" variable as it is most insignificant.

logistic_11 <- glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

summary(logistic_11)

vif(logistic_11)

# emp.var.rateand, nr.employed and cons.price.idx have high vif values and also
# the 3 are highly correlated as can be seen from the correlation values.
# But, among the 3, emp.var.rate is highly correlated with both nr.employed
# cons.price.idx.
cor(train$emp.var.rate, train$nr.employed)
# [1] 0.9075266
cor(train$emp.var.rate, train$cons.price.idx)
# [1] 0.7759761
cor(train$nr.employed, train$cons.price.idx)
# [1] 0.5240039

# Removing "emp.var.rate" variable as it is most correlated.

logistic_12 <- glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

summary(logistic_12)

vif(logistic_12)

#All the vif values are less than 2.
#cons.conf.idx is now most insignificant
# Removing "cons.conf.idx"

logistic_13 <- glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.price.idx +
                     nr.employed, family = "binomial", data = train)

summary(logistic_13)

vif(logistic_13)

# Removing "cons.price.idx" variable as it is most insignificant.

logistic_14 <- glm(formula = response ~ jobretired + jobstudent +
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + 
                     nr.employed, family = "binomial", data = train)

summary(logistic_14)

vif(logistic_14)

# Removing "jobstudent " variable as it is most insignificant.
logistic_15 <- glm(formula = response ~ jobretired +
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + 
                     nr.employed, family = "binomial", data = train)

summary(logistic_15)

vif(logistic_15)

#Now all the variables are highly significant and the vif values of each is less
#than 2. Hence logistic_15 can be taken as the final model.

logistic_final <- logistic_15
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.08)]


# Let's choose a cutoff value of 6.94% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.0694, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6682583 

sens
# Sensitivity 
# 0.7356322 

spec
# Specificity 
# 0.6597045 

#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Appending the probabilities and response variables to the test data

predictions_logit_vect <- as.vector(predictions_logit)

test$predicted_probs <- predictions_logit_vect

test$predicted_response <- predicted_response

#---------------------------------------------------------    

# Creating new dataframe "test_predictions"

test_predictions <- test[, c("p_id", "response", "predicted_response", "predicted_probs", "duration", "call_cost")]


summary(test_predictions$response)
summary(test_predictions$predicted_response)


response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

# sorting the probabilities in decreasing order 
test_predictions <- test_predictions[order(test_predictions$predicted_probs, decreasing = T), ]

#Downloading the data 
write.csv(test_predictions,"test_prediction.csv")

summary(test_predictions$response[1:6800])
summary(test_predictions$predicted_response[1:6800])

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
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

# Create a Table of cumulative gain and lift 

test_predictions$response <- as.factor(ifelse(test_predictions$response=="yes",1,0))

LG = lift(test_predictions$response, test_predictions$predicted_probs, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")


View(LG)
#As per the LG dataframe, to target the top 80% of the responders, we need to
#target the top 5 deciles or the top (1236+1236+1235+1236+1235) = 6178 of the 
#respondents

#Average call duration for the top 6178 responders
avg_call_top5 <- mean(test_predictions$duration[1:6178])

avg_call_top5
# [1] 263.2799

#Thus average call duration for targeting the top 80% prospects using this model
#is about 263.3 seconds

#Total marketing cost for a random model
total_mktg_random <- sum(test_predictions$call_cost)
total_mktg_random 
# [1] 112725
#marketing cost of targetting 80% of the customers in a random model
mktg_random_80 <- total_mktg_random * 0.8
mktg_random_80
# [1] 90180.04

#marketing cost using the logistic model
mktg_logistic_80 <- sum(test_predictions$call_cost[1:6178])
mktg_logistic_80
# [1] 58618.32

#Savings in marketing costs using the logistic model
savings = mktg_random_80 - mktg_logistic_80
savings

#Thus the logistic model would result in a savings of Rs 31561.72 over a 
#random model.
