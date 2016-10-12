# © 2016 Microsoft Corporation. All rights reserved. 

# Order dataset by columns name. 
OrderByColumns <- function(dataset1, cols, desc) 
{
  return(dataset1[do.call('order', c(dataset1[cols], list(decreasing=desc))),])
} 