addMeanFeature<-function(dataset1, colrng, bycol=NA, windowsize){
  library(zoo)
  id<-bycol
  #print(paste("id =", id))
  rollingmean = c()
  rollingsd = c()
  rng <- which(names(dataset1)%in%colrng)
  a = paste("a",(1:length(rng)),sep="") # average
  sd =paste("sd",(1:length(rng)),sep="") # standard deviation
  
  if (!is.na(id))  {
    #print(length(id))
    if (length(id)>1){
      id<-id[1]
    }
    if (is.numeric(id)) {
      if (id>length(names(dataset1))){
        id <- 0
      }
    }
  }

  if((!is.na(id)) &(id!=0)) { 
    idxcol <- unique(dataset1[[id]])
    nid <- length(idxcol)
    for (i in seq(1:nid)) {
      sub_data = subset(dataset1[,rng], dataset1[[id]] == idxcol[i])
      n_row_subdata = nrow(sub_data)
      w=ifelse(windowsize < n_row_subdata,windowsize,n_row_subdata)
      
      # get the rolling mean for all sensors
      rollingmean = rbind(rollingmean,rollapply(sub_data,w,mean,align = "right",partial=1))
      
      # get the rolling sd for all sensors
      rollingsd_i = rollapply(sub_data,w,sd,align = "right",partial=1)
      rollingsd_i[is.na(rollingsd_i)]=0
      rollingsd = rbind(rollingsd,rollingsd_i)
    }
    
  }
  else {
    #print(rng)
    
    sub_data <- dataset1[,rng]
    #print(sub_data[1:3,])
    n_row_subdata = nrow(sub_data)
    #print(n_row_subdata)
    w=ifelse(windowsize < n_row_subdata,windowsize,n_row_subdata)
    #print (w)
    
    # get the rolling mean for all sensors
    rollingmean = rbind(rollingmean,rollapply(sub_data,w,mean,align = "right",partial=1))
    
    # get the rolling sd for all sensors
    rollingsd_i = rollapply(sub_data,w,sd,align = "right",partial=1)
    rollingsd_i[is.na(rollingsd_i)]=0
    rollingsd = rbind(rollingsd,rollingsd_i)
  }
  data_a = as.data.frame(rollingmean)
  data_sd = as.data.frame(rollingsd)
  
  names(data_a) = a
  names(data_sd) = sd
  
  
  
  df = cbind(data_a,data_sd)
  df2=cbind(dataset1,df)
  return (df2)
  
}