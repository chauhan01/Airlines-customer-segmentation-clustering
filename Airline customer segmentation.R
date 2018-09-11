#-------------------------------------k - means clustring project----------------------------




#--------------------------preparing enviroment---------------------
library(datasets)
library(ggplot2)
library(cluster)
library(caret)
library(fpc)


#-----------------------setting up working directory and importing data----------------------

path = "C:/Users/shubham/Downloads/data/IVY_K Means Clustering_Case Study/IVY_K Means Clustering_Case Study"
setwd(path)
getwd()

data <- read.csv("AirlinesCluster.csv")

dataBackup <- data #creating backup

#-----------------------------Basic exploration of data-----------------------------------

head(data)
summary(data)
str(data)

# checking for missing values
colSums(is.na(data)) # no missing values found


#--------------------------------------Normalizing data------------------------------------

prepoc<- preProcess(data)
NormData <- predict(prepoc, data)

summary(NormData)

#------------------------------------Hierarchical clustring-----------------------------------


#-calculating euclidean distance using dist function 

distance <- dist(NormData, method = "euclidean")


#Hierarchical clustring

Herclust <-  hclust(distance, method = "ward.D")

#ploting cluster dendrogram
plot(Herclust)


Groups <- cutree(Herclust, k = 5) #cut tree into 5 clusters 


table(Groups)

#ploting cluster
clusplot(NormData, Groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')


#computing the averege values in each of the variables for each of the five clusters

tapply(data$Balance, Groups, mean)

tapply(data$QualMiles, Groups, mean)

tapply(data$BonusMiles, Groups, mean)

tapply(data$BonusTrans, Groups, mean)

tapply(data$FlightMiles, Groups, mean)

tapply(data$FlightTrans, Groups, mean)

tapply(data$DaysSinceEnroll, Groups, mean)


#--------------K means clustering-----------------
set.seed(88)


#using elbow method to calculate optimal k
wssplot <- function(data, nc, seed = 88){
  wss<- (nrow(data)-1)* sum(apply(data, 2, var))
  for (i in 1:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}


wssplot(data, 10) #calling the function

# k- means clustring
AirCluster <- kmeans(NormData, 5 , nstart = 100, iter.max = 1000)
AirCluster
AirCluster$cluster <- as.factor(AirCluster$cluster)

#ploting clusters
clusplot(NormData, AirCluster$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



table(AirCluster$cluster)

#computing the averege values in each of the variables for each of the five clusters

tapply(data$Balance, AirCluster$cluster, mean)

tapply(data$QualMiles, AirCluster$cluster, mean)

tapply(data$BonusMiles, AirCluster$cluster, mean)

tapply(data$BonusTrans, AirCluster$cluster, mean)

tapply(data$FlightMiles, AirCluster$cluster, mean)

tapply(data$FlightTrans, AirCluster$cluster, mean)

tapply(data$DaysSinceEnroll, AirCluster$cluster, mean)



