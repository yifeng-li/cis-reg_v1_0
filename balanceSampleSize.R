balanceSampleSize<-function(data,classes,regions,size=100000,seed=100)
{
  # randomly get balanced sample sets for all classes.
  # Yifeng Li, Mar. 28, 2014.
  
  set.seed(seed);
  lenCl=length(classes);
  classUnik=unique(classes);
  classNo=length(classUnik);
  num=vector(mode="integer",classNo);
  # get sample size of each classes
  i=1;
  while(i<=classNo)
  {num[i]=sum(classes==classUnik[i]); 
   i=i+1;
  }
  
  # sample data
  dataNew=NULL;
  classesNew=NULL;
  regionsNew=NULL;
  minSize=min(num);
  if(size>minSize)
  {size=minSize;}
  
  # resample
  allInd=1:lenCl;
  i=1;
  while(i<=classNo)
  {indi=allInd[classes==classUnik[i]];
   indiSmp=indi[sample(1:num[i],size)];
   dataNew=rbind(dataNew,data[indiSmp,]);
   classesNew=c(classesNew,classes[indiSmp]);
   regionsNew=rbind(regionsNew,regions[indiSmp,]);
   i=i+1;
  }
  colnames(dataNew)<-colnames(data);
  result=NULL; # result=vector(mode="list",2);
  result$dataNew=dataNew;
  result$classesNew=classesNew;
  result$regionsNew=regionsNew;
  return(result);
}