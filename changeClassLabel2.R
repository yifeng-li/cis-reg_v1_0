changeClassLabel2<-function( classes12, classLabels)
{# change class labels from 1,2,3,... to given classLabels
  # classes12: vector of integer class labels including 1,2,3,... (the standard class labels)
  # classLabels: vector given string or integer labels, should have the same length as unique(classes)
  # return class labels in given classLabels
  # example:
  # classes12=c(1,2,1,2,3);
  # classLabels=c("a","b","c");
  # c=changeClassLabel2(classes12,classLabels);
  # classLabels=c(4,5,3);
  # c=changeClassLabel2(classes12,classLabels);
  #
  # classes=c("c1","c2","c1","c2","c3");
  # result=changeClassLabel(classes);
  # cb=changeClassLabel2(result$classes12,result$classLabels);
  # testClass=c(2,3,2,2,3);
  # ct=changeClassLabel2(testClass,result$classLabels);
  # Yifeng Li
  # Feb. 13, 2014
  
  lenCl=length(classes12);
  #classUnik=unique(classes12);
  classNo=max(classes12); #max(classUnik);#length(classUnik);
  #if(classNo!=length(classLabels))
  #  return(0);
  
  num=1:classNo;
  #classes12=array(0,dim=c(lenCl,1));
  classesab=vector(mode=mode(classLabels),lenCl);
  i=1;
  while(i<=classNo)
  {
    classesab[classes12==i]=classLabels[i];
    i=i+1;
  }
  return(as.vector(classesab));
}