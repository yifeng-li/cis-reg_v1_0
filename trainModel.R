trainModel<-function(trainSet,trainClasses,alpha=0.8)
{
  # train using all training data and using all lambdas
  # CV to find the best lambda
  # train using all training data and using the best lambda
  # return two models
  
  library("glmnet");
  normalize=TRUE;
  errorAUC="class";
  kfold=3;
  
  numCl=length(unique(trainClasses));
  # cross-validation
  if(numCl==2) 
  {twoMulti="binomial";}
  if(numCl>2)
  {twoMulti="multinomial";}
  
  # CV to select a lambda
  cvResult=cv.glmnet(trainSet, trainClasses, nfold=kfold, type.measure=errorAUC, alpha=alpha,  family=twoMulti, standardize=normalize);
  lambda1se=cvResult$lambda[cvResult$lambda==cvResult$lambda.1se];
  
  # train on whole data using the selected lambda
  model=glmnet(trainSet, trainClasses,alpha=alpha,family=twoMulti,standardize=normalize,lambda=lambda1se);
 return(model);
}