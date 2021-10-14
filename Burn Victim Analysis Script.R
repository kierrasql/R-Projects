#Assignment 2- Kierra Young
setwd("C:/Users/kierr/OneDrive/burn")
burn<-read.csv(file="burn.csv", head=TRUE, sep=",", as.is=FALSE)
#display data information 
head(burn)

#Stats for burn data 
str(burn)
summary(burn)

View(burn)

#Data Processing 

#preview the data
View(burn)

#setting seed
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(burn), replace = TRUE, prob = c(0.7, 0.3))
train.data <- burn [ind == 1, ]
test.data <- burn [ind == 2, ]

#creating model and store in a variable model
model<-glm(DEATH~., family=binomial, data=train.data)
#output the coeficients and Residual Deviance
print(model)
#output the coefficient, p value, and standard error for each independent variable and intercept
summary(model)
#output the coefficients and an intercept
exp(coef(model))

#first 10 estimated values
model$fitted.values[1:10]
#confusion matrix for the training set; need to round the estimated values
table(round(model$fitted.values), train.data$DEATH)
table(round(predict(model, train.data, type="response")), train.data$DEATH)

#display the first 10 estimated values for the test data
predict (model, test.data, type="response")[1:10]
#store the estimated values in a variable mypredictions; need to round the values
mypredictions<-round(predict (model, test.data, type="response"))
#confusion matrix for the test data
table (mypredictions, test.data$DEATH)

#plot the residuals
plot(predict(model),residuals(model), col=c("blue"))
lines(lowess(predict(model),residuals(model)), col=c("black"), lwd=2)
abline(h=0, col="grey")

#minimal adequate model
summary(step(model))


#Barplot for amount of burned victims 

counts <- table(burn$FLAME, burn$INH_INJ)
barplot(counts, main="Amount of Burn Victims from Flame Vs INH_INJ",
        xlab="Burn Victims", col= 1:6, space = c(0, 2))
legend("topright",
       legend= rownames(counts),
       fill = 1:6, ncol = 2,
       cex = 0.25)

