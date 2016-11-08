# (C) 2016 Microsoft Corporation. All rights reserved.
　
moduleprefix ="AML_RECOMMEND_MODULES_FOR_MY_DATA:"
　
# Check if column has some missing values
checkmissing<- function(column) {
  result <- FALSE
  ratio <-  mean(is.na(column))
  if(ratio > 0 && ratio <= 0.5) {
    result <- TRUE
  }
  result
}
　
# Check if column has many missing values
checkmanymissing<- function(column) {
  result <- FALSE
  ratio <-  mean(is.na(column))
  if(ratio >0.5 ){
    result <- TRUE
  }
  result
}
　
# Check if column has NaNs
checknan <- function(column) {
  result <- FALSE
  n <- sum(is.nan(column))
  if(n > 0) {
    result <- TRUE
  }
  result
}
　
# Check if column has Infs
checkinf <- function(column) {
  result <- FALSE
  n <- sum(is.infinite(column))
  if(n > 0) {
    result <- TRUE
  }
  result
}
　
# Check if column has lots of unique values
checkunique<- function(column){
  result <- FALSE
  if(!is.numeric(column))
  {
    ratio <- length(unique(column))/length(column)
    if(ratio > 0.5) {
        result <- TRUE
    }
  }
  result
}
　
# Check if column has tokenized text
checktext <- function(column) {
  result <- FALSE
  if(!is.numeric(column))
  {
    tcount <- 0.0
    for(elem in column) {
      ntokens <- length(strsplit(elem,split=" ")[[1]])
      if(ntokens > 1) {
        tcount <- tcount + 1.0
      }
    }
    if(tcount/length(column) > 0.25) {
      result <- TRUE
    }
  }
  result
}
　
# Check is column is constant
checkconstant<- function(column){
  result <- FALSE
  if(length(unique(column))==1){
    result <- TRUE
  }
  result
}
　
　
checktypeconflicts <- function(column) {
  result <- FALSE
  if(!is.numeric(column)) {
    oldwarning = getOption("warn")
    options(warn = -1)
    n <- sum(is.na(as.double(as.character(column))))
    if(n/length(column)<0.25) {
    result <- TRUE
    }
    options(warn=oldwarning)
  }
  result
}
　
appendmessages <- function(messages,newmodule,newcolumns,newmessage)
{
  newcolumns <- paste(newcolumns,collapse=",")
  newrow <- data.frame("Recommended modules"=newmodule,"Columns to transform"=newcolumns,"Recommended action"=newmessage,check.names = FALSE,stringsAsFactors = FALSE)
  rbind(messages,newrow)
}
　
recommendmodules <- function(data)
{
  print(paste(moduleprefix,"Running Azure ML Dataset Validation"))
  result <- data.frame("Recommended modules"=c(),"Columns to transform"=c(),"Recommended action"=c(),check.names = FALSE,stringsAsFactors = FALSE)
  colnames <- names(data)
 
  # Check missing
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checkmissing(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Use Clean Missing Data module to replace missing values."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Clean Missing Data",
                            checkedcols,
                            message)
  }
  
  # Check many missing
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checkmanymissing(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Use Clean Missing Data to remove columns with mostly missing values."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Clean Missing Data",
                            checkedcols,
                            message)
  }
  
  # Check NaNs
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checknan(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Use Convert to Dataset module with Action ReplaceValues to replace Inf values."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Convert to Dataset",
                            checkedcols,
                            message)
  }
  
  # Check Infs
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checkinf(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Use Convert to Dataset module with Action ReplaceValues to replace NaN values."
    print(paste(moduleprefix,message))
    
    result <- appendmessages(result,"Convert to Dataset",
                            checkedcols,
                            message)
  } 
  
  # Check uniques
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checkunique(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Remove columns with large proportion of unique values as they may result in overfitting."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Select Columns in Dataset",
                            checkedcols,
                            message)
  } 
  
  # Check text
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checktext(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Apply Feature Hashing or Extract N-Gram Features from Text modules to text columns."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Feature Hashing, Extract N-Gram Features from Text",
                            checkedcols,
                            message)
  } 
  
  # Check constant
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checkconstant(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Remove columns with constant values as they have no predictive value."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Select Columns in Dataset",
                            checkedcols,
                            message)
  } 
  
  # Check typeconflict
  checkedcols = list()
  for(colname in colnames){
    column <- unlist(data[,colname])
    if(checktypeconflicts(column)) {checkedcols <- append(checkedcols,colname)}
  }
  if(length(checkedcols)>0) {
    message <- "Replace few non-numeric values in columns to make them all numeric."
    print(paste(moduleprefix,message))
    result <- appendmessages(result,"Execute Python Script, Execute R Script, Convert to Dataset",
                            checkedcols,
                            message)
  } 
  result
}
　
