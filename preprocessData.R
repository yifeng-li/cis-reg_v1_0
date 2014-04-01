preprocessData<-function(data,classes,features,positive=TRUE,logTrans=TRUE)
{
  # preprocess the data imported. set negative values to zero, and log-transform all values
  # Yifeng Li, Mar. 28, 2014.
  
  data<-as.matrix(data);
  features<-as.character(features[[1]]);
  colnames(data)<-features;
  classes<-as.character(classes[[1]]);
  
  # set data non-negative
  if(positive)
  {tfData<-data<0;
   data[tfData]<-0;
  }
  
  # log-transform
  if(logTrans)
  {data<-log2(data+1);}
  dataP=NULL;
  dataP$data=data;
  dataP$classes=classes;
  dataP$features=features;
  return(dataP);
}
