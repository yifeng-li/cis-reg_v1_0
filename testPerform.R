testPerform<-function(testClassPredicted,testClass,classLabels)
{
  # Analyze the performance of classification.
  # The Class lables are strings
  # Usage:
  # perf=testPerform(testClassPredicted,testClass,classLabels)
  # testClassPredicted: vector of strings, predicted class labels of testing samples.
  # testClass: vector of strings, the real class labels of testing.
  # classLabels: the class labels used in training.
  # perf: row vector,
  #      if cl==2: perf=[Spec.,Sen., Acc., and BAcc]
  #      if cl>2: perf=[Acc1,Acc1,Acc2,Acc3,...,Acc,Bacc].
  # for example,
  # testClass=c("c1","c1","c1","c2","c2","c2","c3","c3");
  # testClassPredicted=c("c1","c1","c3","c2","c1","c2","c3","c3");
  # classLabels=c("c1","c2","c3");
  # result=testPerform(testClassPredicted,testClass,classLabels)
  # Yifeng Li
  # Mar 27, 2014
  
  cl=length(classLabels); # number of classes
  perf=numeric(cl+2);

  
  n=length(testClass); # number of test samples
  
  conMat=matrix(0,cl,cl); # confusion matrix
  rownames(conMat)=classLabels;
  colnames(conMat)=classLabels;
  # fill out confusion matrix
  i=1;
  while(i<=n)
  {
    conMat[testClass[i],testClassPredicted[i]] <- conMat[testClass[i],testClassPredicted[i]] + 1;
    i=i+1;
  }
  # change the row and column names of the confusion matrix
  rownames(conMat)=paste(classLabels,"Actual",sep="_");
  colnames(conMat)=paste(classLabels,"Predicted",sep="_");
  
  # numerical measures
  perf[1:cl]=diag(conMat); # acc of each class
  perf[cl+1]=sum(perf[1:cl]); # acc
  perf[cl+2]=0; # BACC
  names(perf)=c(classLabels,"Accuracy","BACC");
  # get acc0,acc1,...
  ci=1;
  while(ci<=cl)
  {
    numci=sum(testClass==classLabels[ci]); # number of test samples with this class label
    perf[classLabels[ci]]=perf[classLabels[ci]]/numci;
    ci=ci+1;
  }
  perf[cl+1]=perf[cl+1]/n; #acc
  perf[cl+2]=mean(perf[1:cl],na.rm=TRUE); # geometricMean(perf(1:c)) # nanmean(perf(1:c)); # bacc/arithmetric mean
  result=NULL;
  perf=round(perf,4);
  names(perf)=c(classLabels,"Accuracy","BACC");
  result$perf=perf;
  result$std=NULL;
  result$conMat=conMat;
  result$classLabels=classLabels;
  return(result);
}