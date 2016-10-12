# © 2016 Microsoft Corporation. All rights reserved.
library(ggplot2)
library(gridExtra)

twovariableplots <- function(dataset, xcol, ycol, viztype) 
{
  graphics.off()
  # Create new plot with desired size
  png("myplot.png",width=1200,height=800) 
  # Get rid of default rViz file
  file.remove(Sys.glob("*rViz*png")) 

  print(paste("ScatterplotCustomModuleVisualizationType=",viztype,sep=""))
  
  # Scatterplot
  if(viztype=="scatterplotplain"){
    p <- ggplot(dataset,aes_q(x=as.name(xcol), y=as.name(ycol)))+geom_point(shape=1)
  }  
  
  # Scatterplot with linear regression
  if(viztype=="scatterplotwithregression"){
    p <- ggplot(dataset,aes_q(x=as.name(xcol), y=as.name(ycol)))+geom_point(shape=1)+geom_smooth(method=lm)
  }
  
  # Scatterplot with LOESS  
  if(viztype=="scatterplotwithloess"){
    p <- ggplot(dataset,aes_q(x=as.name(xcol), y=as.name(ycol)))+geom_point(shape=1)+geom_smooth(method=loess)
  }  

  # Scatterplot with contours   
  #if(viztype=="scatterplotwithcontours"){
  #  p <- ggplot(dataset,aes_q(x=as.name(xcol), y=as.name(ycol)))+geom_point(shape=1)+geom_density_2d()
  #}  
  
  print(p)
  dummy = data.frame("dummy"=c(0.0))
  return(dummy)
}
  
keyvalueplots <- function(dataset, xcol, ycol, groupviztype) 
{
  graphics.off()
  # Create new plot with desired size
  png("myplot.png",width=1200,height=800) 
  # Get rid of default rViz file
  file.remove(Sys.glob("*rViz*png")) 
  
  print(paste("GroupedDataVisualizationType=",groupviztype,sep=""))
  
  # Density plot
  if(groupviztype=="densitywithkey"){
      p <- ggplot(dataset,aes_q(as.name(ycol),fill=as.name(xcol),colour=as.name(xcol)))+geom_density(alpha=0.5)
    }
  
  # Bar chart
  if(groupviztype=="histogramwithkey"){
    p <- ggplot(dataset,aes_q(as.name(ycol),fill=as.name(xcol),colour=as.name(xcol)))+geom_histogram(alpha=0.5)
  }  
  
  # Bar chart
  if(groupviztype=="barwithkey"){
      p <- ggplot(dataset,aes_q(as.name(ycol),fill=as.name(xcol),colour=as.name(xcol)))+geom_bar(alpha=0.5)
    }
  print(p)
  dummy = data.frame("dummy"=c(0.0))
  return(dummy)
}

getregressionprediction <- function(dataset){
  tryCatch({
    return(get.label.column(dataset,label.type=RegressionScoresFeatureChannel))
  },
  error=function(err) {
    tryCatch({
      return(get.label.column(dataset,label.type=BayesianLinearRegressionScoresFeatureChannel))
    },
    error = function(err) {
      stop("No regression score column found. Use Score Model module to score the dataset.")
    })
  })
}


errorplots <- function(dataset,featurecol,errorviztype)
{
  graphics.off()
  # Create new plot with desired size
  png("myplot.png",width=1200,height=800) 
  # Get rid of default rViz file
  file.remove(Sys.glob("*rViz*png")) 
  
  print(paste("PredictionErrorVisualizationType=",errorviztype,sep=""))
  if(errorviztype == "regressionerror"){
    dataset["Predicted vs true score difference"] = getregressionprediction(dataset) - get.label.column(dataset,label.type=TrueLabelType)
    if(is.factor(dataset[,featurecol]) || is.character(dataset[,featurecol])){
      p <- ggplot(dataset,aes_q(as.name("Predicted vs true score difference"),fill=as.name(featurecol),colour=as.name(featurecol)))+geom_density(alpha=0.5)
    }
    else
    {
      p <- ggplot(dataset,aes_q(x=as.name(featurecol), y=as.name("Predicted vs true score difference")))+geom_point(shape=1)
    }
  }
  if(errorviztype == "classificationacc"){  
    predacc = get.label.column(dataset,label.type=ScoredLabelType) == get.label.column(dataset,label.type=TrueLabelType)
    if(is.factor(dataset[,featurecol]) || is.character(dataset[,featurecol])){
      dataset["Predicted and true label match"] = as.numeric(predacc)
      accuracy <- setNames(aggregate(dataset[,"Predicted and true label match"], by=list(dataset[,featurecol]),FUN=mean),c("Category","Average accuracy"))
      p <- ggplot(accuracy,aes_q(x=as.name("Category"),y=as.name("Average accuracy"),fill=as.name("Category")))+geom_bar(stat="identity")
    }
    else{
      dataset["Predicted and true label match"] = as.factor(predacc)
      p <- ggplot(dataset,aes_q(as.name(featurecol),fill=as.name("Predicted and true label match"),colour=as.name("Predicted and true label match")))+geom_density(alpha=0.5)
    }
  }
  print(p)
  dummy = data.frame("dummy"=c(0.0))
  return(dummy)  
}
