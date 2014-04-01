predictModel<-function(model,predictSet)
{
  # predict the class labels of the given data samples, using the trained model.
  # Yifeng Li, Mar. 28, 2014.
  
  outType="class"; # response
  classesPredicted=predict(model,predictSet,s=NULL,type=outType);
  if(outType=="response")
  {classesPredicted=as.vector(round(classesPredicted,4),mode="numeric");}
  return(classesPredicted);
}