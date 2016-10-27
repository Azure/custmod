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
  trx = NULL
  class(minSupport) <- NULL
  class(minConfidence) <- NULL
  
  # if no columns specified, take all columns
    if (is.null(cols)) {
      cols = colnames(dataframe)
    }
  
  # data object is a regular data frame
  if (dataObjType == 'dataframe') {        
    # convert all columns to factor
    df <- as.data.frame(lapply(dataframe[cols], as.factor))    
    trx <- as(df, "transactions")
  } 
  # data object is a single column where each row is a list of items separated by comma
  else if (dataObjType == "list") {        
    if (length(cols) != 1) stop("There can be only a single column.")    
    df <- dataframe[cols]    
    l <- lapply(apply(df, 1, mySplit), str_trim)    
    trx <- as(l, "transactions")    
  }
  # data object is a sparse matrix
  else if (dataObjType == "matrix") {
    m <- as.matrix(dataframe)
    trx <- as(m, "transactions")        
  }
  else {
    stop (paste("Unknown data object type:", dataObjType))
  }
  
  # parameter list. set minlen = 2 to avoid empty rule sets
  p <- list(supp = minSupport, confidence = minConfidence, minlen = minlen, maxlen = maxlen, target = target)   
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
  
  rules <- apriori(trx, parameter = p, appearance = a)  
  maxReturnItems <- if (length(rules) < maxReturnItems) length(rules) else maxReturnItems
  
  str(rules)   
  # find left and right side labels
  
  # create empty charater arrays for left and right hand sides
  if (target == "rules") {
    left = character()
    right = character()
    
    # prune the rules if asked  
    if (length(rules) > 0) {
      if (length(rules) > 1 & pruneRedundancies) {
        sub <- is.subset(rules, rules)
        sub[lower.tri(sub, diag = T)] <- NA
        redundant <- colSums(sub, na.rm = T) >= 1
        rules <- rules[!redundant]
      }
      # sort the rules
      rules <- sort(rules, by = sortBy)
      rules <- rules[1:maxReturnItems]
      
      left = labels(lhs(rules))$elements
      right = labels(rhs(rules))$elements
    }
    # print the rules
    inspect(rules)   
    # create an array as row index
    ids <- if (length(rules) > 0) 1:length(rules) else integer()
    # assemble the rules dataset
    rulesDataset <- data.frame(id = ids, lhs = left, rhs = right, rules@quality) 
    return (rulesDataset)  
  } 
  # return frequent item sets
  else { 
    # always sort by support
    if (length(rules) > 1) {
      rules <- sort(rules, by = "support")
      rules <- rules[1:maxReturnItems]
    }
    inspect(rules) 
    # set up an array for row index
    ids <- if (length(rules) > 0) 1:length(rules) else integer()
    # set up an array for items
    labels <- if (length(rules) == 0) character() else labels(items(rules))$elements
    # assemble the dataframe
    itemsDataset <- data.frame(id = ids, items = labels, rules@quality)
    return (itemsDataset)
  }
}

mySplit <- function(s) strsplit(s, ",")[[1]]
