compareData<-function(dataset1, dataset2)
{
  
  d1<-dim(dataset1)
  d2<-dim(dataset2)
  
  if (d1[1]!=d2[1]|d1[2]!=d2[2]) {
    print("**** ERROR: dataset sizes don't match ****")
    paste("dataset1 size:",print(d1))
    paste("dataset2 size:",print(d2))
    print("using minimum size for comparison")
    
  }
  
  rowmin = min(d1[1], d2[1])
  colmin = min(d1[2], d2[2])
  res1<-(dataset1[1:rowmin, 1:colmin]==dataset2[1:rowmin, 1:colmin])
  
  sum1<-summary(res1)
  sum2<-colSums(res1)
  heatmap(1*res1,scale = "none", Rowv = NA, Colv = NA, col=c("red", "green"))
  bplt<-barplot(sum2,col = heat.colors(d1[2]), beside=TRUE, legend=rownames(sum2),xlab="Column Names", ylab="Number of datapoints that match")

  return(list(data.frame(res1),data.frame(sum1)))
  
}