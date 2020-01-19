install.packages("ISLR")
library(ISLR)

install.packages('class')
library(class) # Contains the "knn" function

##############################################
##############################################

#Create partitions in the Smarket data set (60% for training, 40% for testing/evaluation)
sm_sample <- sample(1:nrow(Smarket), size=nrow(Smarket)*0.6)
sm_train <- Smarket[sm_sample, ] #Select the 60% of rows
sm_test <- Smarket[-sm_sample, ] #Select the 40% of rows

##############################################
##############################################

#Here 2,3,4 in the vector represent Lag1, Lag2 and Lag3 features
train.X <- sm_train[,c(2,3,4)]
test.X <- sm_test[,c(2,3,4)]

#Seed must set in order to get reproducible result
set.seed(4985912356) #Set the seed for reproducibility

#First try to determine the right K-value 
smarket_acc <- numeric() #holding variable


highest_accuracy1 <- -99
highest_k_accuracy1 <- -1


for(i in 1:30){
  predict <- knn(train=train.X, test=test.X, cl=sm_train$Direction, k=i)
  smarket_acc <- c(smarket_acc, mean(predict==sm_test$Direction))
  if(highest_accuracy1 < smarket_acc[i]){ #To find the value which has the highest accuracy.
    highest_accuracy1 <- smarket_acc[i]
    highest_k_accuracy1 <- i
  }
}

#Plot error rates for k=1 to 30
plot(1-smarket_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Smarket with varying K")

#highest accuracy
highest_accuracy1

#k value has highest accuracy
highest_k_accuracy1

##############################################
##############################################

#Here 4,5,7 in the vector represent Lag3, Lag4 and Volume features
train.X <- sm_train[,c(4,5,7)]
test.X <- sm_test[,c(4,5,7)]

#Seed must set in order to get reproducible result

#First try to determine the right K-value 
smarket_acc <- numeric() #holding variable


highest_accuracy2 <- -99
highest_k_accuracy2 <- -1


for(i in 1:30){
  predict <- knn(train=train.X, test=test.X, cl=sm_train$Direction, k=i)
  smarket_acc <- c(smarket_acc, mean(predict==sm_test$Direction))
  if(highest_accuracy2 < smarket_acc[i]){ #To find the value which has the highest accuracy.
    highest_accuracy2 <- smarket_acc[i]
    highest_k_accuracy2 <- i
  }
}

#Plot error rates for k=1 to 30
plot(1-smarket_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Smarket with varying K")

#highest accuracy
highest_accuracy2

#k value has highest accuracy
highest_k_accuracy2

##############################################
##############################################

#Here 3,6,7 in the vector represent Lag2, Lag5 and Volume features
train.X <- sm_train[,c(3,6,7)]
test.X <- sm_test[,c(3,6,7)]

#Seed must set in order to get reproducible result

#First try to determine the right K-value 
smarket_acc <- numeric() #holding variable


highest_accuracy3 <- -99
highest_k_accuracy3 <- -1


for(i in 1:30){
  predict <- knn(train=train.X, test=test.X, cl=sm_train$Direction, k=i)
  smarket_acc <- c(smarket_acc, mean(predict==sm_test$Direction))
  if(highest_accuracy3 < smarket_acc[i]){ #To find the value which has the highest accuracy.
    highest_accuracy3 <- smarket_acc[i]
    highest_k_accuracy3 <- i
  }
}

#Plot error rates for k=1 to 30
plot(1-smarket_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Smarket with varying K")

#highest accuracy
highest_accuracy3

#k value has highest accuracy
highest_k_accuracy3

##############################################
max_value <- max(highest_k_accuracy1,highest_accuracy2,highest_k_accuracy3)

if(highest_k_accuracy1==max_value){
  cat(sprintf("The highest accuracy(%f) provided by the combinations that are Lag1, Lag2 and Lag3",highest_accuracy1))
} else if(highest_k_accuracy2==max_value){
  cat(sprintf("The highest accuracy(%f) provided by the combinations that are Lag3, Lag4 and Volume",highest_accuracy2))
} else {
  cat(sprintf("The highest accuracy(%f) provided by the combinations that are Lag2, Lag5 and Volume",highest_accuracy3))
}
  






