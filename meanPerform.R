meanPerform<-function(perfAllFolds)
{
  # perfAllFolds: list of objects returned by testPerform function
  # example:
#   r1=NULL;
#   r1$perf=c(0.8,0.9,0.78,0.75);
#   r1$conMat=rbind(c(29,3),c(1,31));
#   r2=NULL;
#   r2$perf=c(0.78,0.82,0.88,0.76);
#   r2$conMat=rbind(c(25,5),c(2,31));
#   r3=NULL;
#   r3$perf=c(0.81,0.88,0.82,0.79);
#   r3$conMat=rbind(c(31,0),c(1,30));
#   perfAllFolds=NULL;
#   perfAllFolds[[1]]=r1;
#   perfAllFolds[[2]]=r2;
#   perfAllFolds[[3]]=r3;
#   mp=meanPerform(perfAllFolds);
  # Yifeng Li, Mar. 31, 2014
  
  source('sumCol.R');
  source('stdCol.R');
  ks=length(perfAllFolds); # number of runs or folds
  numCl=length(perfAllFolds[[1]]$perf) -2; # number of classes
  allAcc=matrix(0,ks,numCl+2); # accuracies of all runs
  meanConMat=matrix(0,numCl,numCl); # man confusion matrix
  rownames(meanConMat)=rownames(perfAllFolds[[1]]$conMat);
  colnames(meanConMat)=colnames(perfAllFolds[[1]]$conMat);
  k=1;
  while(k<=ks)
  {
    allAcc[k,]=perfAllFolds[[k]]$perf;
    meanConMat=meanConMat+perfAllFolds[[k]]$conMat;
    k=k+1;
  }
  
  perfMean=NULL;
  perf=round(sumCol(allAcc)/ks,4);
  names(perf)=names(perfAllFolds[[1]]$perf);
  perfMean$perf=perf;
  perfMean$std=stdCol(allAcc);
  meanConMat= round(meanConMat/ks,4); 
  perfMean$conMat=meanConMat;
  return(perfMean);
}