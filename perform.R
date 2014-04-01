perform<-function(testClassPredicted,testClass,cl)
{
  # Analyze the performance of classification
# For binary-class problem, the class labels should be 1 and 2.
# For m-class problem, the class labels should be 1,2,3,...,m.
# Usage:
  # perf=perform(testClassPredicted,testClass,c)
# testClassPredicted: vector, predicted class labels of testing samples.
# testClass: vector, the real class labels of testing.
# c: the number of classes.
# perf: row vector,
#      if cl==2: perf=[Spec.,Sen., Acc., and BAcc]
#      if cl>2: perf=[Acc1,Acc1,Acc2,Acc3,...,Acc,Bacc].
# for example,
# testClass=randi([0,2],100,1);
# testClassPredicted=randi([0,2],100,1);
# [perf,conMat]=perform(testClassPredicted,testClass,3)
  
  testClassPredicted=as.numeric(testClassPredicted);
  testClass=as.numeric(testClass);
perf=numeric(cl+2);
tfnum=is.numeric(testClass);
if(!tfnum)
return(0);

n=length(testClass); # number of test samples

conMat=matrix(0,cl,cl); # confusion matrix

# fill out confusion matrix
i=1;
while(i<=n)
{
  #if(!is.finite(testClassPredicted[i])||is.na(testClassPredicted[i]) )
  # {next;}
conMat[testClass[i],testClassPredicted[i]] <- conMat[testClass[i],testClassPredicted[i]] + 1;
i=i+1;
}


# numerical measures
perf[1:cl]=diag(conMat);
perf[cl+1]=sum(perf[1:cl]); # acc
# get acc0,acc1,...
ci=1;
while(ci<=cl)
{
numci=sum(testClass==ci);
perf[ci]=perf[ci]/numci;
ci=ci+1;
}
perf[cl+1]=perf[cl+1]/n; #acc
perf[cl+2]=mean(perf[1:cl],na.rm=TRUE); # geometricMean(perf(1:c)) # nanmean(perf(1:c)); # bacc/arithmetric mean
result=NULL;
result$perf=perf;
result$conMat=conMat;
return(result);
}