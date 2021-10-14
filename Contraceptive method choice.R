#Kierra Young
#Assignment 3
#7/03/2021
#CMC Data


#1. installing library party
install.packages("party")
library("party")

#2.setting the working directory to cmcc.
setwd("C:/Users/kierr/Downloads/cmcc")

#3. reading credit data
cmc<-read.csv(file="cmc.csv", head=TRUE, sep=",", as.is=FALSE)
#viewing structure
str(cmc) 
#see summary
summary(cmc)
#viewing credit data
View(cmc)

#Creating a Bar plot for wife age 

counts <- table(cmc$ContraceptiveMethod, cmc$WifeAge)
barplot(counts, main="Wives Age",
        xlab="Contraceptive method", col= 1:6, space = c(0, 2))
legend("topright",
       legend= rownames(counts),
       fill = 1:6, ncol = 2,
       cex = 0.25)

#Histogram for contraceptive method use data 
library(lattice)

histogram(~ ContraceptiveMethod, data = cmc,
          main = "CMC Contraceptive Methods",
          xlab = "Method Type",
          ylab = "Total Method Use Percentage",
          col = c("chocolate2", "seagreen"),
          breaks = 20)

#Pre-processing data
#4. training and test data split
set.seed(1234)
ind <- sample(2, nrow(cmc), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cmc[ind == 1, ]
test.data <- cmc[ind == 2, ]

#6. training data method
myFormula<-ContraceptiveMethod~.
cmc_ctree <- ctree(myFormula, data = train.data)

#7. tree structure model
print(cmc_ctree) 

#8. visual of the tree
nodes(cmc_ctree,2)
plot(cmc_ctree)
plot(cmc_ctree, type="simple")

#9. building a confusion matrix
table(predict(cmc_ctree), train.data$ContraceptiveMethod)
prop.table(table(predict(cmc_ctree), train.data$ContraceptiveMethod))

#10. making a table for test data confusion matrix 
testPred <- predict(cmc_ctree, newdata = test.data)
table (testPred, test.data$ContraceptiveMethod)

#11. Trying a new confusion matrix method 

install.packages('caret')
# S3 method for class default- Failed 
confusionMatrix(cmc, test.data, positive = NULL, dnn = c('Prediction', 'Reference'))

#12. Another method for the confusion matrix- Failed  
confusionMatrix(cmc$ContraceptiveMethod, testPred)

#13.Trying ROC Method - Failed 
install.packages('ROSE')
confusionMatrix(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), ...)

table(factor(testPred, levels=min(test):max(test)), 
      factor(test, levels=min(test):max(test)))
