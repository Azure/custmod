library(arules)
library(stringr)

ar <- function(dataframe, 
               dataObjType = "dataframe", 
               cols = NULL, 
               minSupport = 0.1, 
               minConfidence = 0.8,                               
               minlen = 2,
               maxlen = 5, 
               sortBy = "confidence", 
               lhs = NULL, 
               rhs = NULL, 
               pruneRedundancies = FALSE,
               target = "rules",
               maxReturnItems = 100) {  
  # initialization
  print ("initialization")
  trx = NULL
  class(minSupport) <- NULL
  class(minConfidence) <- NULL
  
  print ("get columns")
  # if no columns specified, take all columns
    if (is.null(cols)) {
      cols = colnames(dataframe)
    }
  
  # data object is a regular data frame
  if (dataObjType == 'dataframe') {        
    print ("for dataframe, convert all columns to factor")
    # convert all columns to factor
    df <- as.data.frame(lapply(dataframe[cols], as.factor))    
    trx <- as(df, "transactions")
  } 
  # data object is a single column where each row is a list of items separated by comma
  else if (dataObjType == "list") {        
    print ("for list, split items in the items column into list")	
    if (length(cols) != 1) stop("There can be only a single column.")    
    df <- dataframe[cols]    
    l <- lapply(apply(df, 1, mySplit), str_trim)    
    trx <- as(l, "transactions")    
  }
  # data object is a sparse matrix
  else if (dataObjType == "matrix") {
    print ("for matrix, just cast it to transactions")
    m <- as.matrix(dataframe[cols])
    trx <- as(m, "transactions")        
  }
  else {
    stop (paste("Unknown data object type:", dataObjType))
  }
  
  print ("construct parameters")
  # parameter list. set minlen = 2 to avoid empty rule sets
  p <- list(supp = minSupport, confidence = minConfidence, minlen = minlen, maxlen = maxlen, target = as.character(target))

  print ("construct appearance")
  # appearance list
  a <- list()
  if (!is.null(rhs)){ 
    a$default = "lhs"
    a$rhs = lapply(strsplit(rhs, ","), str_trim)[[1]]    
    }  
  if (!is.null(lhs)){ 
    a$default = "rhs"
    a$lhs = lapply(strsplit(lhs, ","), str_trim)[[1]]    
    }     
  if (!is.null(lhs) & !is.null(rhs)) { a$default <- "none" }
  
  print ("run rules")
  rules <- apriori(trx, parameter = p, appearance = a)  

  print ("calculate top N")
  maxReturnItems <- if (length(rules) < maxReturnItems) length(rules) else maxReturnItems
  print (maxReturnItems)
  
  str(rules)   
  # find left and right side labels
  
  # create empty charater arrays for left and right hand sides
  if (target == "rules") {
    left = character()
    right = character()
    
    print ("prune rules")
    if (length(rules) > 0) {
      # prune the rules if asked  
      if (length(rules) > 1 & pruneRedundancies) {
        sub <- is.subset(rules, rules)
        sub[lower.tri(sub, diag = T)] <- NA
        redundant <- colSums(sub, na.rm = T) >= 1
        rules <- rules[!redundant]
      }

      # sort the rules
      print ("sort rules")
      rules <- sort(rules, by = sortBy)
      print ("get left hand side")
      left = labels(lhs(rules))$elements
      print ("get right hand side")
      right = labels(rhs(rules))$elements
    }
    # print the rules
    print ("inspect rules")
    inspect(rules)   
    # create an array as row index
    print ("create row index arrary")
    ids <- if (length(rules) > 0) 1:length(rules) else integer()
    # assemble the rules dataset
    print ("assemble rules dataset")
    result <- data.frame(id = ids, lhs = left, rhs = right, rules@quality) 
  } 
  # return frequent item sets
  else { 
    # always sort by support
    if (length(rules) > 1) {
      print ("always sort by support")
      rules <- sort(rules, by = "support")
    }
    print ("inspect rules")
    inspect(rules) 
    # set up an array for row index
    print ("create row index arrary")
    ids <- if (length(rules) > 0) 1:length(rules) else integer()
    # set up an array for items
    print ("get items list")
    labels <- if (length(rules) == 0) character() else labels(items(rules))$elements
    # assemble the dataframe
    print ("assemble items dataset")
    result <- data.frame(id = ids, items = labels, rules@quality)
  }
  if (maxReturnItems > 1) {
    print ("take top N")
    result <- result[1:maxReturnItems,]
  }
  return (result)
}

mySplit <- function(s) strsplit(s, ",")[[1]]
