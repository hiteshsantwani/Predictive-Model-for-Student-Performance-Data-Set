#######################################################
# This code is solution to HW 1 question 4 Statistical data mining
#
# Hitesh santwani
# Created: 09/13/2018
#######################################################
#install.packages("caret")
#install.packages('e1071', dependencies=TRUE)
library(ElemStatLearn)
library(caret)
library(class)
rm(list = ls())

#Loading only the 2's and 3's from ziptrain data
ziptrain_filtered_data <- subset(zip.train, zip.train[,1]==2 | zip.train[,1]==3)
ziptrain_filtered_data <- data.frame(ziptrain_filtered_data)

#Loading only the 2's and 3's from ziptest data
ziptest_filtered_data <- subset(zip.test, zip.test[,1]==2 | zip.test[,1]==3)
ziptest_filtered_data <- data.frame(ziptest_filtered_data)

linear_model <- lm(X1 ~ ., data=ziptrain_filtered_data)

prediction_linear <- predict.lm(linear_model, ziptest_filtered_data)

# TO GET THE Residual standard error
summary(linear_model)
# Residual standard error: 0.1745

# Mean_square_error_train = mean(linear_model$residuals^2)
# Mean_square_error_test <- mean((ziptest_filtered_data$X1 - predict.lm(linear_model, ziptest_filtered_data)) ^ 2)

train_X1_Output <- factor(ziptrain_filtered_data$X1)
test_X1_Output <-factor(ziptest_filtered_data$X1)

# ziptrain_filtered_data$X1 <- factor(ifelse(ziptrain_filtered_data$X1==2, "TWO", "THREE"))
# trctrl <- trainControl(method = "repeatedcv", number = 17, repeats = 3)
# set.seed(3333)
# knn_fit <- train(ziptrain_filtered_data$X1 ~., data = ziptrain_filtered_data, method = "knn",
#                 trControl=trctrl,
#                 preProcess = c("center", "scale"),
#                 tuneLength = 10)


knn_N_1 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 1)
table(a = ziptest_filtered_data$X1,mod=knn_N_1)
## TEST ERROR - 355 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.025

knn_N_3 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 3)
table(a = ziptest_filtered_data$X1,mod=knn_N_3)
## TEST ERROR - 353 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.0302

knn_N_5 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 5)
table(a = ziptest_filtered_data$X1,mod=knn_N_5)
## TEST ERROR - 353 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.0302

knn_N_7 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 7)
table(a = ziptest_filtered_data$X1,mod=knn_N_7)
## TEST ERROR - 353 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.0302

knn_N_9 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 9)
table(a = ziptest_filtered_data$X1,mod=knn_N_9)
## TEST ERROR - 351 / 364 = 0.964 (ACCURACY) = 1 - (ACCURACY) 0.035

knn_N_11 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 11)
table(a = ziptest_filtered_data$X1,mod=knn_N_11)
## TEST ERROR - 351 / 364 = 0.964 (ACCURACY) = 1 - (ACCURACY) 0.035

knn_N_13 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 13)
table(a = ziptest_filtered_data$X1,mod=knn_N_13)
## TEST ERROR - 352 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.0329

knn_N_15 <- knn(ziptrain_filtered_data,ziptest_filtered_data,ziptrain_filtered_data$X1,k = 15)
table(a = ziptest_filtered_data$X1,mod=knn_N_15)
## TEST ERROR - 350 / 364 = 0.975 (ACCURACY) = 1 - (ACCURACY) 0.038

## From above information it is evident that best error we get is with KNN where k = 1
