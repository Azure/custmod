lf1 <- function (dataset2, valcol2, numfeats){
  previous <- numfeats
  orig_names <- names(dataset2)
  n_rows <- dim(dataset2)[1]
  n_cols <- dim(dataset2)[2]
  
  base<-valcol2
  
  
  
  for (i in 1:previous) {
  
     
    dataset2[(i+1):n_rows,n_cols+i] <- dataset2[1:(n_rows-i),base]
    dataset2[1:i,n_cols+i] <- dataset2[1:i,
                                       if (i==1)  base else base+(n_cols-base)+i-1
                                       ]
  }
  
  
  a <- -1:-previous
  
  new_names <- paste(colnames(dataset2)[valcol2],"lag ",a)
  
  
  names(dataset2) <- c(orig_names,new_names)
  
  return (dataset2)
}
GenerateLagFeatures <- function (dataset1, valcol, numfeats){
  orig_names <- names(dataset1)
  bases <- which((orig_names %in% valcol))
  
  if (length(bases) == 0)
  {
    print (paste("No columns matching with ", valcol, " found"))
    return (dataset1)
  }
  for (k in 1:length(bases)) {
    
    base <- bases[k]
    
    
    dataset1<- lf1(dataset1, base, numfeats)
  }
  
  
  return (dataset1)
}