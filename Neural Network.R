#Kierra Young 
#Assignment 4 
#7/20/2021


#neuralnet package 
library("neuralnet")

#Reading file for WHAS1 and setting work directory.
setwd("C:/Users/kierr/Downloads/whas")
whas1<-read.csv(file="whas1.csv", head=TRUE, sep=",")

#Viewing rows
head(whas1)
View(whas1)

#viewing data in summary 
summary(whas1)

#view data frame
str(whas1)

# Scale of 14 variables
column[1:14]<-scale(column[1:14])

#Setting the train and test seed
set.seed(12345)
#Splitting the data into a train and test separate set
ind <- sample(2, nrow(whas1), replace = TRUE, prob = c(0.7, 0.3))
train.data <- whas1[ind == 1, ]
test.data <- whas1[ind == 2, ]
#Building neural network changed ce to sse due to only working with binary reponse 
nn<-neuralnet(formula =FSTAT~AGE+SEX+CPK+SHO+CHF+MIORD+MITYPE+YEAR+YRGRP+LENSTAY+DSTAT+LENFOL,data = train.data, hidden=2, err.fct="sse", linear.output = FALSE)
#displays the shown neural network properties for evaluation
names(nn)

#Commands for displaying the network properties for  nets 

nn$net.result[[1]][1:10] # first 10 predicted probabilities 
nn$result.matrix         # This command shows the amount of trainings steps, the error, and the weights 
plot(nn)                 # plot the network

#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict [1:9]

#Confusion matrix for the training set
table(mypredict, train.data$FSTAT, dnn =c("Predicted", "Actual"))
mean(mypredict==train.data$FSTAT)

#Confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:14])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$FSTAT, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$FSTAT)

#Barplot for amount of burned victims 

counts <- table(whas1$FSTAT, whas1$AGE)
barplot(counts, main="Age Vs Number of People Alive",
        xlab="Heart Attack Victims", col= 1:6, space = c(0, 2))
legend("topright",
       legend= rownames(counts),
       fill = 1:6, ncol = 2,
       cex = 0.25)

#Histogram for contraceptive method use data 
library(lattice)

histogram(~ FSTAT, data = whas1,
          main = "Worecester Heart Attack Study",
          xlab = "0=dead 1=Alive",
          ylab = "Total Percentage of Dead or Alive",
          col = c("chocolate2", "seagreen"),
          breaks = 20)

