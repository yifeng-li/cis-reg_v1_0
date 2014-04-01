plotCoefHeatMap<-function(model,features,fileName="CoefHeatMap.pdf")
{
  require(grDevices); 
  require(graphics);
  library("gplots");
  beta1<-as.matrix(model$beta);
  lambda<-model$lambda;
  distDef<-function(x) dist(x, method="euclidean"); # euclidean binary
  hclustDef<-function(x) hclust(x, method="average");
  #fileName=file.path(dataStr,winSize,classNo,vs,"pos",positive,"balanced",balanced,"log",logTrans,"norm",normalize,"alpha",alpha,"heat.pdf",fsep="_");
  #saveFull=file.path(fileSavePath,fileName);
  pdf(fileName, width=10, height=10);
  heatmap.2(beta1, Rowv=FALSE, Colv=FALSE, scale="none", distfun=distDef, hclustfun=hclustDef, dendrogram="none", 
            key=TRUE, keysize=1,
            trace="none", col=cm.colors(64), margins=c(8,8), symbreaks=TRUE,symkey=TRUE,
            cexRow=0.3 + 1/log10(nrow(beta1)), cexCol=0.2 + 1/log10(ncol(beta1)),labRow=features, labCol=as.character(log(lambda)),
            main="Heatmap of The Coefficients of The Features As Lambda Change",xlab="Log(Lambda)",ylab="Coefficient");
  # color: rainbow, heat.colors, terrain.colors, topo.colors, cm.colors
  dev.off()
}
