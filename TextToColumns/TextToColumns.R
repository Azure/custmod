TextToCols<-function(dataset1, dataset2 = NA, selcol, splch, incorig) {
  if (length(selcol)>1)
  {
    
    scol<-selcol[1]
  }
  else
  {
    scol<-selcol
  }
  
  dataset3 <- data.frame( dataset1[[scol]])

  names(dataset3) <- c("V1")
 
  # delete the extra space at the end of the lines
  dataset3$V1 <- gsub(" +$","",dataset3$V1)
  
  # convert to character
  dataset3$V1 <- as.character(dataset3$V1)
 
  # parse the input into multiple columns and generate the data frame
  m <- sapply(dataset3$V1,strsplit, split=splch)
  
  if (is.na(dataset2) || is.null(dataset2)) {
    df <- data.frame(matrix(unlist(m), nrow=length(m), byrow=T), stringsAsFactors=FALSE)

  }
  else {
    colnames1 <- dataset2[1,]
    
    df <- data.frame(matrix(unlist(m), nrow=length(m), byrow=T,dimnames=list(c(),colnames1)), stringsAsFactors=FALSE)
    
    # generate column names for the data frame
    sz <- min(length(dataset2), length(m))
    
    names(df) <- colnames1
  }
  if ((incorig == "Include") | (incorig=="incl")){
    dataset <- cbind(dataset1, df)
  }
  else {
    dataset <- df
  }
    
  
  # genearte outpout data
  return(dataset)
}