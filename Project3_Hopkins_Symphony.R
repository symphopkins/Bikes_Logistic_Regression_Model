#############################################
#                                           #
# Author:     Symphony Hopkins              #
# Date:       03/31/2023                    #
# Subject:    Project 3                     #
# Class:      DSCI 512                      #
# Section:    01W                           #         
# Instructor: Juan David Munoz              #
# File Name:  Project3_Hopkins_Symphony.R   #
#                                           #
#############################################

#PART 1----------------------------------------------

#1.Load the dataset mtcars.xlsx Download mtcars.xlsx
#  into memory and convert column am to a factor using 
#  factor() function.
#  Answer: See code.

#importing library
library(readxl)
mtcars <- read_csv("Documents/Maryville_University/DSCI_512/Week_3/mtcars.xlsx")
View(mtcars)

#converting am column to a factor
mtcars$am <- as.factor(mtcars$am)


#2.Split the data into training set and test set. The 
#  training set contains the first 35 observations, the
#  test set containing the remaining observations.
#  Answer: Since the subset function of the glm function
#  only accepts logical vectors, we need to create one
#  for the train and test sets doing the following:

#creating vector to equal length of total rows
mtcars$nid <- 1:nrow(mtcars)

#creating logical vector to find first 35 rows for train set
#this will be used in the glm function
mtcars_first_35 <- mtcars$nid<36

#performing train-test split
mtcars_train <- mtcars[mtcars_first_35, ]
mtcars_test <- mtcars[!mtcars_first_35, ]



#3.Build a logistic regression model with the response 
#  is am and the predictors are mpg, cyl, hp, and wt using 
#  glm() function.
#  Answer: We were able to create a logistic regression
#  model, however, it is important to note that we received
#  the following warning message: glm.fit: fitted probabilities 
#  numerically 0 or 1 occurred. It's also worth noting that
#  this is a not an error, so our model is still fit, but there
#  may be outliers causing this warning message to appear.
#  Source: https://www.statology.org/glm-fit-fitted-probabilities
#  -numerically-0-or-1-occurred/

#building logistic regression model
model_1 <- glm(am ~ mpg + cyl + hp + wt, data=mtcars, 
    subset=mtcars_first_35, family=binomial)
summary(model_1)



#4.Compute the test error on the test data set using a 
#  confusion matrix. Is it a good model based on test error?
#  Answer: The predictions were correct for 83% of the cars,
#  which means our model is decent. It would be interesting to
#  see how our model performs on a larger dataset. 

#creating confusion matrix
model_1_prob <- predict(model_1, newdata=mtcars_test, type="response")
model_1_pred <- rep('0',nrow(mtcars_test))
model_1_pred[model_1_prob > 0.5] = '1'
table(model_1_pred, mtcars_test$am)
mean(model_1_pred == mtcars_test$am)

#PART 2----------------------------------------------

#0.Importing bike.csv data set.
bike <- read.csv("~/Documents/Maryville_University/DSCI_512/Week_3/Bike.csv")


#1.Build a linear model to forecast number of total 
#  rentals (count) using potential predictors, season, 
#  holiday, workingday, weather, atemp, and registered.
#  Answer: See code. 

#building linear regression model
model_2 <- lm(count ~ season + holiday + workingday +
                weather + atemp + registered, data=bike)
summary(model_2)

#2.Perform best subset selection using bestglm() function 
#  based on BIC. What’s the best model based on BIC?
#  Answer: Based on the BIC, our best model includes
#  the following predictors: season, holiday, workingday,
#  weather, atemp, and registered.

#importing libraries
library(leaps)
library(bestglm)

#selecting columns with predictors and targets
Xy <- bike[, c('season','holiday','workingday','weather',
               'atemp','registered','count')]

#converting Xy to data frame
Xy <- as.data.frame(Xy)

#performing best subset selection based on BIC
model_3_bestglm_BIC <- bestglm(Xy, IC='BIC')
print(model_3_bestglm_BIC)

#3.Compute the test error of the best model based on BIC 
#  using LOOCV.
#  Answer: Since it is unclear on how to calculate the BIC
#  using LOOCV, we will evaluate the model using the metrics
#  provided by the function (RMSE, Rsquared, and MAE). From 
#  the metrics, we can conclude 

#importing library
library(caret)

#performing LOOCV
train_control <- trainControl(method='LOOCV')
model_4_LOOCV <- train(count ~ season + holiday + workingday +
                        weather + atemp + registered, data=bike,
                      trControl=train_control, method='lm')
print(model_4_LOOCV)


#4.Calculate the test error of the best model based on 
#  BIC using 10-fold CV.
#  Answer: a.	Since it is unclear on how to calculate the BIC 
#  using 10-fold CV, we will evaluate the model using the metrics 
#  provided by the function (RMSE, Rsquared, and MAE). From the 
#  metrics (specifically Rsquared), we can conclude that the relationship 
#  between the predictors and response variable is strong. We can even 
#  say that the model performed as well as the previous model being that 
#  the test errors are very similar.

#performing 10-fold CV
train_control_10_fold <- trainControl(method='CV', number=10)
model_5_10_CV <- train(count ~ season + holiday + workingday +
                         weather + atemp + registered, data=bike,
                       trControl=train_control_10_fold, method='lm')
print(model_5_10_CV)


#5.Perform best subset selection using bestglm() function 
#  based on CV. What’s the best model based on CV?
#  Answer: The best model based on CV includes the following predictors: 
#  season, holiday, workingday, weather, atemp, and registered

#performing best subset selection based on CV
model_6_bestglm_CV <- bestglm(Xy, IC='CV')
print(model_6_bestglm_CV)

#6.Perform the backward stepwise selection using stepAIC() 
#  function. What’s the best model?
#  Answer: The best model based includes the following predictors: 
#  season, holiday, workingday, weather, atemp, and registered.

#importing library
library(MASS)

#performing backward stepwise selection
model_7_stepAIC <- stepAIC(model_2, direction='backward')
print(model_7_stepAIC)

#End Assignment










