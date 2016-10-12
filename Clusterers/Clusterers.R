# © 2016 Microsoft Corporation. All rights reserved. 

# Wrapper around R hclust function
trainHierarchicalClusterer <- function(dataset,clusteringMethod,nClusters)
{
  d <- dist(dataset)
  clus <- hclust(d,method=clusteringMethod)
  # Cut tree into n clusters
  cIdx <- cutree(clus,nClusters)
  oDataset <- dataset
  oDataset["Cluster Assignments"] <- cIdx
  return(oDataset)
}

# wrapper around dbscan function
trainDBSCANClusterer <- function(dataset,dbscanEps)
{
  # Install dbscan package for AML
  AML <- TRUE
  if(AML)
  {
    # RRS optimization, check if it was installed previously in same process
    ip <- installed.packages(lib.loc=".",fields="Package")
    if(! "dbscan" %in% row.names(ip))
    {
      install.packages("src/dbscan_0.9-8.zip",lib=".",repos=NULL)
    }
    library(dbscan,lib.loc=".")
  }
  else
  {
    library(dbscan)
  }
  cls <- dbscan(dataset,eps=dbscanEps)
  cIdx <- cls$cluster
  oDataset <- dataset
  oDataset["Cluster Assignments"] <- cIdx
  return(oDataset)
}

# Use known clustering to do a k-NN lookup against new data
scoreClustererByKNN <- function(clustering,dataset,label,append)
{
  library(class)
  
  # Split off cluster label column
  clusteringLabels <- clustering[,label]
  clustering[label] <- NULL
  newClustering <- knn(clustering,dataset,cl=clusteringLabels)
  
  # Append results to input data vs return results only
  if(append)
  {
    oDataset <- dataset
    oDataset["Cluster Assignments"] <- newClustering
  }
  else
  {
    oDataset <- data.frame("Cluster Assignments"=newClustering,check.names = FALSE)
  }
  return(oDataset)
}