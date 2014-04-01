changeClassLabel<-function( classes )
{# change class labels to 1,2,3,...
  # classes: vector of string or integers
  # return class labels in {1,2,3,...}
  # Yifeng Li
  # Nov. 20, 2013
  # example:
  # classes=c("c1","c2","c1","c2","c3");
  # result=changeClassLabel(classes);
  # classes=c(4,4,5,5,3);
  # result=changeClassLabel(classes);
  
  lenCl=length(classes);
  classUnik=unique(classes);
  classNo=length(classUnik);
  num=1:classNo;
  #classes12=array(0,dim=c(lenCl,1));
  classes12=vector(mode="integer",lenCl);
  i=1;
  while(i<=lenCl)
  {
    classInd=(classUnik==classes[i]);
    classes12[i]=num[classInd];
    i=i+1;
  }
  result=NULL;
  result$classes12=as.vector(classes12);
  result$classLabels=classUnik;
 return(result);
}