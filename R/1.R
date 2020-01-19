setwd('C:\\Users\\akifc\\Desktop')
options(max.print=999999)

library(tree) # Contains the "tree" function

wheat_types <- read.table("wheat_types.txt", header = TRUE, sep = ";", dec = ".")


#Set the seed for reproducibility
# Use 75% of samples for traning, the rest for testing
# the indices (row ids) are saved in the "sub" vector
set.seed(579642)
sub <- sample(1:nrow(wheat_types), size=nrow(wheat_types)*0.75)

wt.tr <- tree(type ~ ., data = wheat_types, subset = sub)
summary(wt.tr)

#################################
#Cross-validation version - Construct a new DT for different partitions of the samples - 100 times

dt_acc <- numeric()
set.seed(1815850)

highest_accuracy <- -99

for(i in 1:100){
  sub <-  sample(1:nrow(wheat_types), size=nrow(wheat_types)*0.75)
  fit2 <- tree(type ~ ., data = wheat_types, subset = sub)
  test_predict <- table(predict(fit2, wheat_types[-sub, ]), wheat_types[-sub, "type"])
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
  
  if(highest_accuracy < dt_acc[i]){ #DT which has the highest accuracy over 100 iterations.
    highest_accuracy <- dt_acc[i]
    highest_acc_tr <- fit2 
  }
}

#Report average accuracy of DT after 100 iterations.
mean(dt_acc)


#Draw a plot to show how accuracy changes over 100 iterations.
plot(1-dt_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Wheat Types With Different Subsets of Data")

#################################

#The value 'highest_acc_tr' filled above.
# Show the DT (with parent and internal node’s decision criteria) which has the highest accuracy over 100 iterations.
plot(highest_acc_tr,  type = "uniform")
text(highest_acc_tr)








