#This is my assignment 1 -Kierra Young
#setting work directory 
setwd("C:/Users/kierr/Downloads/nursey")
#reading file 
nursery<-read.csv(file="nursery.csv", head=TRUE, sep=",", as.is=FALSE)

#installing libraries 
library(arules)
library(arulesViz)

#preview of data
head(nursery)

#nursery data structure
str(nursery)

#stat analysis
summary(nursery)

#describes the datatype
sapply(nursery, class)

#first inspection of rules

rules<-apriori(nursery)
inspect(rules)

#pruning 
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

#remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#rules pruned summary
summary(rules.pruned)




#Histogram for number of Applications by Class Status
        
library(lattice)

histogram(~ class, data = nursery,
          main = "Nursery Applications",
          xlab = "Class Description",
          ylab = "Total Application Percentage",
          col = c("chocolate2", "seagreen"),
          breaks = 20)

# Stacked Bar Plot with Colors and Legend
counts <- table(nursery$children, nursery$class)
barplot(counts, main="Amount of Applications by Children and Class",
        xlab="Classes", col= 1:6, space = c(0, 2))
        legend("topright",
               legend= rownames(counts),
               fill = 1:6, ncol = 2,
               cex = 0.25)

#BoxPlot visual for applications of number children, parents category considering the class
FacetPlot1 = ggplot(nursery, aes(x=children, y=parents)) + geom_boxplot() + facet_grid(~class) 
FacetPlot1

#Apriori method 1
rules.pruned <- apriori(nursery, parameter= list(supp=0.1, conf=0.9))
inspect(rules.pruned)

#installing libraries 
library(ggplot2)
#BoxPlot visual for applications of number children, parents category considering the class
FacetPlot1 = ggplot(nursery, aes(x=children, y=finance)) + geom_boxplot() + facet_grid(~class) 
FacetPlot1

#Apriori method 2
rules.pruned <- apriori(nursery, parameter= list(supp=0.2, conf=0.8))
inspect(rules.pruned)

#Factor Function 

nursery$class<-factor(nursery$class)
summary(nursery$class)

#paracoord - rule property
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
#plot for rules pruned
plot(rules.pruned)
#plot for old rules pruned 
plot(rules)
#graph rules property
plot(rules.pruned, method="graph")


#additional metrics
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), nursery)




#grouping evaluation
plot(rules.pruned, method = "grouped")

#matrix evaulation
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))
