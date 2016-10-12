fsscore<-function(model, numPeriodsToForecast) {
  if (numPeriodsToForecast<=0)
  {
    print("ERROR: forecast doesn't have any unknow value as time attribute present in training data")
    return(data.frame(NA))
  }
  else
  {
   
    forecastedData <- forecast(model, h=numPeriodsToForecast)
    
    output <- data.frame(forecastperiod=seq(1:numPeriodsToForecast),forecast=as.numeric(forecastedData$mean)
                         
                         )
    #print(fcst80)
    fcstLo<-data.frame(forecastedData$lower)
    fcstUp<-data.frame(forecastedData$upper)
    names(fcstLo)<-forecastedData$level
    names(fcstUp)<-forecastedData$level
    
    output80 <- data.frame( 
                          lower80=as.numeric(fcstLo[["80"]]),
                          upper80=as.numeric(fcstUp[["80"]]))
    output95 <- data.frame( 
      lower95=as.numeric(fcstLo[["95"]]),
      upper95=as.numeric(fcstUp[["95"]]))
    
    #return(list(output,data.frame(model.frame())))
    return(list(output,output80,output95, saveModel(model)))
  }
}
fstrain<-function(dataset1, freq, valcol, fcst)
{

  orig_names <- names(dataset1)
  seasonality<-freq
  datacol <- which((orig_names %in% valcol))
  if (length(datacol)>=2)
  {
    print("ERROR: please use a single column for forecasting")
    return(data.frame(NA))
  }
  #labels <- as.numeric(dataset1[,which((orig_names %in% valcol))[1]])
  labels <- as.numeric(dataset1[,datacol])
  timeseries <- ts(labels,frequency=seasonality)
  
  
  if(fcst=="arima"){
    
    model <- auto.arima(timeseries)
    
  }
  else if (fcst == "stl") {
    model <- stl(timeseries, s.window="periodic")
  }
  else if (fcst == "stl+arima") {
    model <- stlf(train.ts, method = "arima", s.window = "periodic")
    
  }
  else {
    model <- ets(timeseries)
  }

  return(model)
  
  
}

saveModel<-function(model)
{
  m1<-data.frame(payload = as.integer(serialize(model, connection = NULL)))
  #m2<-model.frame(model)
  #print(m2)
  return(m1)
}

retrieveModel<-function(ml1)
{
  
  return(unserialize(as.raw(ml1$payload)))
}

fs<-function(dataset1, valcol, numPeriodsToForecast, freq, fcst){
  library(forecast)
  model<-fstrain(dataset1, freq, valcol, fcst)
  
  if (length(model)>1){
    #numPeriodsToForecast <- ceiling(max(dataset2$time)) - ceiling(max(dataset1$time))
    #numPeriodsToForecast <- max(numPeriodsToForecast, 0)
    #numPeriodsToForecast <- min(length(dataset2$time), numPeriodsToForecast)
    #dataset3 <- subset(dataset2$time, dataset2$time>max(dataset1$time))
    pred<-fsscore(model, numPeriodsToForecast)
    if (length(pred)<1){
      print("Error: Not Applicable predictions")
      return(pred)
    }
    return(pred)  
  } 
  else {
    print("Error: Not Applicable model")
    return(model)
  }
  
}
