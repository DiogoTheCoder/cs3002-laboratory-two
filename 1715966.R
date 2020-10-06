#mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//spaeth_01.csv', sep=",")
#mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//spaeth_02.csv', sep=",")
#mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//spaeth_03.csv', sep=",")
#mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//spaeth_04.csv', sep=",")
#mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//spaeth_05.csv', sep=",")

# Iris
mydata = read.csv('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//iris.csv', sep=",")

plot(mydata)

# optional: Prepare Data
mydata = na.omit(mydata) # deletion of missing data
# DON'T NORMALISE THE DATA, WK DROPS FROM 0.98 -> 0.78
#mydata = scale(mydata) # standardise variables

matrix <- matrix('', 21, 2)

for (numOfClusters in seq(from=2, to=22)) {
  # Use the Euclidean form of measuring distance,
  # and generate a distance matrix
  # other options include: euclidean, manhattan
  d <- dist(mydata, method = "manhattan")
  
  clusterAmount <- 3
  
  # Hierarchical Clustering
  # other options include: average, complete and single
  fit <- hclust(d, method="average")
  
  # Plot dendrogram
  #plot(fit)
  
  Hgroups <- cutree(fit, k=numOfClusters)
  rect.hclust(fit, k=numOfClusters, border="red")
  
  #plot(mydata, col=Hgroups)
  
  # K-means clustering
  fit <- kmeans(mydata, numOfClusters)
  mean <- aggregate(mydata,by=list(fit$cluster),FUN=mean)
  Kgroups = fit$cluster
  #plot(mydata, col=Kgroups)
  
  # WK takes in two clusterings and returns the Weighted Kappa between them
  # so to calculate the WK value between the hierarchical and k-means methods use
  source('//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//WK_R.r')
  
  
  # The is the agreement strength between two cluster arrangements
  # −1 <= WK <= 0: Very Poor
  # 0 < WK <= 0.2: Poor
  # 0.2 < WK <= 0.4: Fair
  # 0.4 < WK <= 0.6: Moderate
  # 0.6 < WK <= 0.8: Good
  # 0.8 < WK <= 1: Very Good
  
  # Single linkage method: getting 
  wk = WK_R(Kgroups, Hgroups)
  
  #plot(mydata, col=Kgroups)
  #plot(mydata, col=Hgroups)
  
  matrix[numOfClusters - 1,1] <- numOfClusters
  matrix[numOfClusters - 1,2] <- wk
}

plot(matrix, xlab = 'Num of Clusters', ylab = 'Weighted Kappa')

write.csv(Hgroups, file = '//Users//diogocosta//Work//computer-science//cs3002//cs3002-laboratory-two//Hgroups.csv')

install.packages("arules")
install.packages("arulesViz")