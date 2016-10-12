lf2 <- function (dataset2, valcol2, numfeats, skps){
  # Computes the lag features for specific column specified by valcol2 within dataset2
  # This creates total numfeats new features and it assumes dataset is already sorted by time
  # Args:
  #   dataset2: dataset to be used for adding new features.
  #   valcol2:  index of the column for which we need to create new features.
  #   numfeats: number of features to create. For example, if this is 3, then it will create 3 new lag features
  #   skps:     created nested lag features by using this array as skipping interval for each level
  # Returns:
  #   New dataset with additional features
  previous <- numfeats #total number of features to be generated
  
  n_rows <- dim(dataset2)[1] #number of rows in dataset
  n_cols <- dim(dataset2)[2] #number of columns in dataset
  
  base<-valcol2 #set column index to be featurized
  skips1<-strsplit(skps, " ", fixed=TRUE)[[1]] # create skip array
  
  skiparr <- strtoi(skips1) #convert to integer
  
  levels <- length(skiparr) #track # of levels
  
  for (j in 1:levels) {
    orig_names <- names(dataset2)
    movpos <- numfeats*(j-1)
    previous <- numfeats
    offset <- prod(skiparr[1:j])
    
    for (i in 1:previous) {
      
      
      #copy 1:n-1 rows for specific column to 2:n as part of lag
      dataset2[(i*offset+1):n_rows,n_cols+i+movpos] <- dataset2[1:(n_rows-i*offset),base] 
      # now adjust values for the first numfeat rows
      dataset2[1:(i*offset),n_cols+i+movpos] <- dataset2[1:(i*offset),
                                                         if ((i==1) & (j==1)) base else base+(n_cols-base)+i-1+movpos
                                                ]
    }
    #if (i==1)  base else 
    a <- -1:-previous
    
    new_names <- paste(colnames(dataset2)[valcol2],"lag",j," ",a, sep="") #create new column names
    
    
    names(dataset2) <- c(orig_names,new_names) #update dataset with new column names
  }
  
  
  
  
  return (dataset2)
}

GenerateNestedLagFeatures <- function (dataset1, valcol, numfeats, skps){
  # Computes the lag features for specific column specified by valcol2 within dataset2
  # This creates total numfeats new features and it assumes dataset is already sorted by time
  # Args:
  #   dataset1: dataset to be used for adding new features.
  #   valcol2:  array of string that has names' of all  columns in string format for which we need to create new features.
  #   numfeats: number of features to create. For example, if this is 3, then it will create 3 new lag features
  #   skps:     created nested lag features by using this array as skipping interval for each level
  # Returns:
  #   New dataset with additional features
  orig_names <- names(dataset1)
  bases <- which((orig_names %in% valcol)) #find columns that needs to be featurized
  
  if (length(bases) == 0)
  {
    # error - no column found with name that is matching in the list of names provided to the function
    print (paste("No columns matching with ", valcol, " found"))
    return (dataset1)
  }
  #generate features for each column
  for (k in 1:length(bases)) {
    
    base <- bases[k]
    
    
    dataset1<- lf2(dataset1, base, numfeats, skps)
  }
  
  
  return (dataset1)
}


#https://gist.github.com/nk773/a2ed7cd0ce8020647f5e7711f749b3b5#file-lag-r