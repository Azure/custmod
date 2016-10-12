# Map 1-based optional input ports to variables
library(forecast)
masefun <- function(observed, predicted){
  error = 0;
  if (length(observed) != length(predicted)) {
    return (NA);
  } else if (length(observed) == 0 || length(predicted) == 0) {
    return (NA);
  }
  else {
    denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
    error = sum((abs(observed-predicted)) / denom)/length(observed);
  }
  return (error);
}


smape <- function(observed, predicted){
  error = 0;
  if (length(observed) != length(predicted)) {
    return (NA);
  } else if (length(observed) == 0 || length(predicted) == 0) {
    return (NA);
  }
  else {
    error = sum((abs(observed-predicted)) / (observed+predicted))/length(observed);
    # denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
    #  error = sum((abs(observed-predicted)) / denom)/length(observed);
  }
  return (100.0*error);
}



evaluateTimeSeries<-function(dataset1, obsd, fcst, algo) {
  orig_names <- names(dataset1)
  
  dataidx <- which((orig_names %in% obsd))
  fcstidx <- which((orig_names %in% fcst))
  
  
  if (which(names(dataset1) %in% c("time"))>0) {
    time <- as.numeric(dataset1$time)
  }
  else {
    time <- seq(1:length(dataset1))
  }
  observed_data <- as.numeric(dataset1[,dataidx])
  forecast <- as.numeric(dataset1[,fcstidx])
  plot(time,observed_data,type="l",col="blue",xlab="Time",ylab="Data",lwd=1.5)
  lines(time,forecast,col="red",lwd=1.5)
  legend("topleft",legend = c("Original Data","Forecast"),bty=c("n","n"),lty=c(1,1),pch=16,col=c("blue","red"))
  
  forecast_data_testwindow <- as.numeric(forecast[(which(!is.na(forecast)))])
  actual_data_testwindow <- as.numeric(observed_data[(which(!is.na(forecast)))])
  mase <- masefun(actual_data_testwindow,forecast_data_testwindow)
  smape <- smape(actual_data_testwindow,forecast_data_testwindow)
  arima_acc <- data.frame(Method=as.character(algo),accuracy(forecast_data_testwindow,actual_data_testwindow),MASE=mase,sMAPE=smape)
  arima_acc$Method <- as.character(arima_acc$Method)
  data.set <- arima_acc
  
  lapply(data.set,class)
  return(data.set)

}