#Assignment 5- Data 630-9040
#Kierra Young 

# Load cluster package
library("cluster")

#user directory

setwd("C:/Users/kierr/Downloads/yeastt")


seeds <- read.csv("yeast.csv")
#Creating a Bar plot for Yeast  

str(seeds)
summary(seeds)

counts <- table(seeds$class, seeds$class)
barplot(counts, main="Class of Seeds by MCG",
        xlab="Class of Yeast", col= 1:6, space = c(0, 2))
legend("topright",
       legend= rownames(counts),
       fill = 1:6, ncol = 2,
       cex = 0.25)

#Histogram for mit on yeast 
library(lattice)

histogram(~ mit, data = myyeast,
          main = "Mit Yeast",
          xlab = "grams of yeast",
          ylab = "Total Yeast Percentage",
          col = c("chocolate2", "seagreen"),
          breaks = 20)

head(seeds)
table(seeds$class)

# Data Pre-processing
# removing the class
myyeast<-seeds
myyeast$class<-NULL
head(myyeast)
scale(myyeast)

# Run the method
set.seed(1234)
#storing kc values for 4 iterations 
kc<-kmeans(myyeast, 4)
#output the result
print(kc)
kc$iter

#cluster to class 
table(seeds$class, kc$cluster)

#cluster plot
clusplot(myyeast, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Anomaly for the yeast 
centers <- kc$centers[kc$cluster, ]
head(centers, 15)
distances <- sqrt(rowSums((myyeast - centers)^2))
distances

outliers <- order(distances, decreasing=T)[1:5]
outliers
seeds[outliers,]


# kc for k-values at 2
kc<-kmeans(myyeast, 2)
#output the result
print(kc)
kc$iter

#cluster to class
table(seeds$class, kc$cluster)

#cluster plot
clusplot(myyeast, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Anomaly for yeast
centers <- kc$centers[kc$cluster, ]
head(centers, 15)
distances <- sqrt(rowSums((myyeast - centers)^2))
distances

outliers <- order(distances, decreasing=T)[1:5]
outliers
seeds[outliers,]

