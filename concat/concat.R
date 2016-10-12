concatenate<-function(dataset1, colrng, sep=" ") {
  
  rng <- which(names(dataset1)%in%colrng)
  if (length(rng)<2) {
    print(paste("Insufficient columns for concatenation", length(rng), rng))
    return(dataset1)
  }
  df<-dataset1[,-rng]
  
  df3<-apply(dataset1[,rng],1, paste, collapse=sep)
  df3<-data.frame(df3)
  
  names(df3)<-c("merged")
  return (cbind(df,df3))
} 
