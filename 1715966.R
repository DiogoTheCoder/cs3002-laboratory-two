setwd("/Users/diogocosta/Work/computer-science/cs3002/cs3002-laboratory-two")

#mydata = read.csv('spaeth_01.csv', sep=",")
#mydata = read.csv('spaeth_02.csv', sep=",")
#mydata = read.csv('spaeth_03.csv', sep=",")
#mydata = read.csv('spaeth_04.csv', sep=",")
#mydata = read.csv('spaeth_05.csv', sep=",")

# Iris
mydata = read.csv('iris.csv', sep=",")
irisreal = read.csv('iris_real.csv', sep=",")

plot(mydata)

# optional: Prepare Data
mydata = na.omit(mydata) # deletion of missing data
# DON'T NORMALISE THE DATA, WK DROPS FROM 0.98 -> 0.78
#mydata = scale(mydata) # standardise variables

matrix <- matrix('', 21, 2)
matrixHGroups <- matrix('', 21, 2)
matrixKGroups <- matrix('', 21, 2)

# WK takes in two clusterings and returns the Weighted Kappa between them
# so to calculate the WK value between the hierarchical and k-means methods use
source('WK_R.r')

# Use the Euclidean form of measuring distance,
# and generate a distance matrix
# other options include: euclidean, manhattan

distances <- c("euclidean", "manhattan")
for (distance in distances) {
  d <- dist(mydata, method = distance)
  for (numOfClusters in seq(from=2, to=22)) {
    # Hierarchical Clustering
    # other options include: average, complete and single
    # OBJECT NEEDS TO BE RE-CREATED
    fit <- hclust(d, method="average")
    
    Hgroups <- cutree(fit, k=numOfClusters)
    
    # Plot dendrogram
    #plot(fit)
    
    # Add cluster boxes
    #rect.hclust(fit, k=numOfClusters, border="red")
    
    #plot(mydata, col=Hgroups)
    
    # K-means clustering
    fit <- kmeans(mydata, numOfClusters)
    mean <- aggregate(mydata,by=list(fit$cluster),FUN=mean)
    Kgroups = fit$cluster
    #plot(mydata, col=Kgroups)
    
    # The is the agreement strength between two cluster arrangements
    # −1 <= WK <= 0: Very Poor
    # 0 < WK <= 0.2: Poor
    # 0.2 < WK <= 0.4: Fair
    # 0.4 < WK <= 0.6: Moderate
    # 0.6 < WK <= 0.8: Good
    # 0.8 < WK <= 1: Very Good
    
    # Single linkage method: getting 
    #wk = WK_R(Kgroups, Hgroups)
    wkH = WK_R(Hgroups, irisreal$X1)
    wkK = WK_R(Kgroups, irisreal$X1)
    
    #plot(mydata, col=Kgroups)
    #plot(mydata, col=Hgroups)
    
    #matrix[numOfClusters - 1,1] <- numOfClusters
    #matrix[numOfClusters - 1,2] <- wk
    
    matrixHGroups[numOfClusters - 1,1] <- numOfClusters
    matrixHGroups[numOfClusters - 1,2] <- wkH
    
    matrixKGroups[numOfClusters - 1,1] <- numOfClusters
    matrixKGroups[numOfClusters - 1,2] <- wkK
  }
  
  #plot(matrix, main = paste('H and K groups vs Iris real', distance), xlab = 'Num of Clusters', ylab = 'Weighted Kappa')
  plot(matrixHGroups, main = paste('Hierchical vs Iris real', distance), xlab = 'Num of Clusters', ylab = 'Weighted Kappa')
  plot(matrixKGroups, main = paste('K-Means vs Iris real', distance) , xlab = 'Num of Clusters', ylab = 'Weighted Kappa') 
}


#write.csv(Hgroups, file = 'Hgroups.csv')

#install.packages("arules")
#install.packages("arulesViz")

#library(arules)
#data('Groceries')
#inspect first 3 transactions
#inspect(head(Groceries, 3))

#Apriori Algorithm - generate rules
#grocery_rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))
#inspect first 3 sorted by confidence
#inspect(head(sort(grocery_rules, by = 'confidence'), 3))

#patientData = read.csv('patients.csv', sep=",", row.names = 1)
#top3 <- patientData[0:3]
#patientEuclideanD <- sum(dist(top3, method = "euclidean"))
#patientManhattanD <- dist(top3, method = "manhattan")
