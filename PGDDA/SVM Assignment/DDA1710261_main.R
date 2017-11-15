############################ SVM Letter Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to to develop a model using Support Vector Machine which should correctly 
# classify the handwritten digits based on the pixel values given as features

#####################################################################################

# 2. Data Understanding: 
# http://yann.lecun.com/exdb/mnist/index.html
# Number of Instances: 60,000 train, 10,000 test points
# Number of Attributes: 785 
# MNIST database of handwritten digits, available from this page, has a training set of 60,000 examples, 
# and a test set of 10,000 examples. The digits have been size-normalized and 
# centered in a fixed-size image.

# The data files train.csv and test.csv contain gray-scale images of hand-drawn digits, from zero through nine.
# 
# Each image is 28 pixels in height and 28 pixels in width, for a total of 784 pixels in total. 
# Each pixel has a single pixel-value associated with it, indicating the lightness or darkness of 
# that pixel, with higher numbers meaning darker. This pixel-value is an integer 
# between 0 and 255, inclusive.

#3. Data Preparation: 
#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(ggplot2)
library(gridExtra)
library(dplyr)


#Loading Data

mnist_data <- read.csv("mnist_train.csv", header = FALSE)

mnist_test <- read.csv("mnist_test.csv", header = FALSE)

#Understanding Dimensions

dim(mnist_data)

#Structure of the dataset

str(mnist_data)

#printing first few rows

head(mnist_data)

#Exploring the data

summary(mnist_data)

#check for NAs

sum(is.na(mnist_data))
#so, no NA values are there in the dataset

#Check for columns which have all 0s 
allzeros <- apply(mnist_data[-1],2, function(x) sum(x) == 0)
zerocols <- names(allzeros[allzeros == TRUE])
length(zerocols)

#Removing all zero columns as they won't be useful in model building and prediction
mnist_data1 <- mnist_data[,!colnames(mnist_data) %in% zerocols]
str(mnist_data1)

allzerorows <- apply(mnist_data[-1],1, function(x) sum(x) == 0)
zerorows <- names(allzerorows[allzerorows == TRUE])
length(zerorows)

#2. Check for Outlier values. 
#In this case only values less than 0 or greater than 255 will be outliers since
#they represent invalid pixel values

outliers_train <- apply(mnist_data,2, function(x) which(x < 0 | x > 255))

length(outliers_train)

#Similarly checking for outliers on test set

outliers_test <- apply(mnist_test,2, function(x) which(x < 0 | x > 255))

length(outliers_test)

#Since both vectors are empty, there are no outliers to be treated

#Making our target class to factor

mnist_data1$V1<-factor(mnist_data1$V1)

#Name the first column as 'digit'
names(mnist_data1)[names(mnist_data1) == "V1"] <- "digit"
summary(mnist_data1)

names(mnist_test)[names(mnist_test) == "V1"] <- "digit"
summary(mnist_test)

table(mnist_data1$digit)/nrow(mnist_data1) * 100

#Thus proportion of each outcome class is between 9 to 11 % and the outcome variable
# is not unbalanced for any class

#Set seed for sample extraction
set.seed(50)

sample.indices <- sample(1:nrow(mnist_data1), 0.2 * nrow(mnist_data1))
sample_mnist <- mnist_data1[sample.indices,]

#A very small sample of around 1000 records is created for tuning hyperparameters
#so that long running times on 
#Set seed for sample extraction main sample can be avoided
set.seed(50)

small.indices <- sample(1:nrow(mnist_data1), 1000)
small_mnist <- mnist_data1[small.indices,]

str(small_mnist)
summary(small_mnist)

table(small_mnist$digit)/nrow(small_mnist) * 100

#The proportion of each outcome class is roughly the same as the original dataset

#Constructing Model

#Using Linear Kernel
samp_linear <- ksvm(digit ~ ., data = sample_mnist, scale = FALSE, kernel = "vanilladot")
pred_linear<- predict(samp_linear, mnist_test[,-1])

#confusion matrix - Linear Kernel
confusionMatrix(pred_linear,mnist_test$digit)

small_linear <- ksvm(digit ~ ., data = small_mnist, scale = FALSE, kernel = "vanilladot")
small_pred_linear<- predict(small_linear, mnist_test[,-1])

#confusion matrix - Linear Kernel
confusionMatrix(small_pred_linear,mnist_test$digit)

############   Hyperparameter tuning and Cross Validation on Linear kernel#####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

# making a grid of C values. 
grid.linear1 <- expand.grid(C=c(1e-10,1e-7,1e-4, 1, 1e4, 1e7, 1e10))

# Performing 5-fold cross validation on small dataset to narrow down on C values which 
# can be used on the larger sample
set.seed(100)

fit.svm.linear1 <- train(digit ~ ., data = small_mnist, method="svmLinear", metric=metric,
                         tuneGrid=grid.linear1, trControl=trainControl)

print(fit.svm.linear1)
# Best tune at C=1e-07, 
# Accuracy - 0.8710961

# Plotting "fit.svm" results
plot(fit.svm.linear1)

#Narrowing the range of C parameter

grid.linear2 <- expand.grid(C=c(1e-06, 1e-07,2e-07, 3e-07, 4e-07, 5e-07))

set.seed(100)

fit.svm.linear2 <- train(digit ~ ., data = small_mnist, method="svmLinear", metric=metric, 
                         tuneGrid=grid.linear2, trControl=trainControl)

# Printing cross validation result
print(fit.svm.linear2)

# Plotting "fit.svm" results
plot(fit.svm.linear2)

grid.linear3 <- expand.grid(C=c(1e-06, 1e-07,3e-07, 5e-07))

set.seed(100)

fit.svm.linear3 <- train(digit ~ ., data = sample_mnist, method="svmLinear", metric=metric, 
                         tuneGrid=grid.linear3, trControl=trainControl)

print(fit.svm.linear3)

plot(fit.svm.linear3)

grid.linear4 <- expand.grid(C=c(1e-06, 5e-06,1e-05, 5e-05, 1e-04))

set.seed(100)
fit.svm.linear4 <- train(digit ~ ., data = sample_mnist, method="svmLinear", metric=metric, 
                         tuneGrid=grid.linear4, trControl=trainControl)

print(fit.svm.linear4)

plot(fit.svm.linear4)

#Using Linear Kernel and value of C chosen from cross validation
samp_linear1 <- ksvm(digit ~ ., data = sample_mnist, scale = FALSE, C = 1e-06, 
                     kernel = "vanilladot")
pred_linear1<- predict(samp_linear1, mnist_test[,-1])

#confusion matrix - Linear Kernel
confusionMatrix(pred_linear1,mnist_test$digit)

#####################-----RBF kernel----#########################################
#Using RBF Kernel on sample data
samp_RBF <- ksvm(digit ~ ., data = sample_mnist, scale = FALSE, kernel = "rbfdot")
pred_RBF<- predict(samp_RBF, mnist_test[,-1])
confusionMatrix(pred_RBF,mnist_test$digit)

#Since, model building and cross validation takes time even on sample data, the models
#will be first built and narrowed down on the small sample to avoid long running 
#times of frequent crss validation
small_RBF <- ksvm(digit ~ ., data = small_mnist, scale = FALSE, kernel = "rbfdot")
small_pred_RBF<- predict(small_RBF, mnist_test[,-1])

#confusion matrix - RBF Kernel
confusionMatrix(small_pred_RBF,mnist_test$digit)

############   Hyperparameter tuning and Cross Validation on RBF kernel#####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

rbf.grid1 <- expand.grid(.sigma=c(1e-10, 1e-07, 1e-04, 1, 1e04, 1e07, 1e10), .C=c(1e-04,1e-02,1,1e02,1e04) )

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

set.seed(100)

fit.svm.rbf1 <- train(digit ~ ., data = small_mnist, method="svmRadial", metric=metric, 
                      tuneGrid = rbf.grid1, trControl = trainControl)

print(fit.svm.rbf1)

plot(fit.svm.rbf1)

#Since Accuracy is increasing with increasing values of sigma and C, therefore,
#they will be increased again in the next grid
rbf.grid2 <- expand.grid(.sigma=c(1e-08, 1e-07, 5e-07, 1e-06), .C=c(0.1, 1, 10, 100, 1000, 10000))

set.seed(100)

fit.svm.rbf2 <- train(digit ~ ., data = small_mnist, method="svmRadial", metric=metric, 
                      tuneGrid = rbf.grid2, trControl = trainControl)

print(fit.svm.rbf2)

plot(fit.svm.rbf2)

# Using the previous grid which gave best results and narrowing the values further
# on sample data

rbf.grid3 <- expand.grid(.sigma=c(1e-07, 5e-07, 1e-06), .C=c( 1, 10, 100))

set.seed(100)

fit.svm.rbf3 <- train(digit ~ ., data = sample_mnist, method="svmRadial", metric=metric, 
                      tuneGrid = rbf.grid3, trControl = trainControl)

print(fit.svm.rbf3)

plot(fit.svm.rbf3)

#Using the values of sigma and C obtained by training radial kernel model
samp_RBF1 <- ksvm(digit ~ ., data = sample_mnist, scale = TRUE, C= 10,kpar=list(sigma = 5e-07),
                  kernel = "rbfdot")

pred_RBF1<- predict(samp_RBF1, mnist_test[,-1])

#confusion matrix - RBF Kernel
confusionMatrix(pred_RBF1,mnist_test$digit)

#With an accuracy of 0.9721, the model' performnce is quite good on test data.
