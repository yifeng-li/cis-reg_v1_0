rm(list=ls(all=TRUE));

# set work directory
wd<-"~/YifengLi/Research/reg_pred_v1_0";
setwd(wd);

#install.packages("glmnet");
#install.packages("cvTools");
library("glmnet");
library("cvTools");

source('balanceSampleSize.R');
source('testPerform.R');
source('changeClassLabel.R');
source('preprocessData.R');
source('useSomeClasses.R');
source('meanPerform.R');
source('trainModel.R');
source('predictModel.R');
source("writePerformance.R");

dataStrs=c("GM12878");
winSizes=c(1000);
for(dataStr in dataStrs)
    for(winSize in winSizes)
    {
      # select=c("EnhancerActive","EnhancerInactive","PromoterActive","PromoterInactive","Exon","Unknown");
      select=c("EnhancerActive","PromoterActive");
      dirStr="~/YifengLi/Research/reg_pred_v1_0/data/";
      balanced=TRUE;
      normalize=TRUE;
      alpha=0.8;
      errorAUC="class";
      kfold=3;
      reruns=3;

      # creat path and folder 
      vs=paste(select,collapse="_");
      numCl=length(unique(select));
      classNo=as.character(numCl);
      fileSavePath=file.path(wd,"/result/resultsMar312014/",vs,"/",dataStr,"_",classNo,"_",vs,"_",winSize,"bp",fsep="");
      dir.create(fileSavePath, showWarnings=FALSE,recursive=TRUE);
      
      # load training data
      data<-read.table(file.path(dirStr,dataStr,"_data_",as.character(winSize),"bp.txt",fsep="")); 
      features<-read.table(file.path(dirStr,dataStr,"_features_",as.character(winSize),"bp.txt",fsep="")); 
      classes<-read.table(file.path(dirStr,dataStr,"_classes_",as.character(winSize),"bp.txt",fsep="")); 
      regions<-read.table(file.path(dirStr,dataStr,"_regions_",as.character(winSize),"bp.txt",fsep=""));
      
      # preprocess data
      dataP=preprocessData(data,classes,features);
      
      data=dataP$data;
      features=dataP$features;
      classes=dataP$classes;
      
      # only use some of the classes
      dataP=useSomeClasses(data,classes,select,regions);
      #data=dataP$data;
      #classes=dataP$classes;
      #regions=dataP$regions;
      
      # run multiple times of cross-validation
      perfReruns=NULL;
      r=1;
      while(r<=reruns)
      {set.seed(r);
       
       data=dataP$data;
       classes=dataP$classes;
       regions=dataP$regions;
       
       # use balanced data
       if(balanced)
       { dataB=balanceSampleSize(data,classes,regions,10000,r);
         data=dataB$dataNew;
         classes=dataB$classesNew;
         regions=dataB$regionsNew;
       }
       
       # CV
       perfAllFolds=NULL;
       cv=cvFolds(length(classes), K = kfold, type = "random"); # use the cvTools package
       k=1;
       while(k<=kfold)
       {
         # obtain training and test subsets
         testInd=cv$subset[cv$which==k];
         trainSet=data[-testInd,];
         testSet=data[testInd,];
         trainClasses=classes[-testInd];
         testClasses=classes[testInd];
         # train a model
         model=trainModel(trainSet,trainClasses,alpha=alpha);
         # test/predict
         testClassesPredicted=predictModel(model,testSet);
         perfAllFolds[[k]]=testPerform(testClassesPredicted,testClasses,select);
         k=k+1;
       } # end of CV
         # average performance to file # accuracies and confusion matrix
       perfReruns[[r]]=meanPerform(perfAllFolds);
       r=r+1;
      } # end of rerun
      meanPerf=meanPerform(perfReruns); # mean performance of multiple runs
      fileName="performance.txt";
      saveFull=file.path(fileSavePath,fileName);
      writePerformance(meanPerf,saveFull); # write to file
    }