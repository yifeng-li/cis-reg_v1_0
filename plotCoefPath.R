# Plot coefficient paths as lambda changes.
plotCoefPath<-function(model,features,fileName="CoefPath.pdf")
{
pdf(fileName, width=10, height=10);
plot(model,xvar="lambda",label=FALSE);
require(grDevices); require(graphics);
beta1<-as.matrix(model$beta);
lambda<-model$lambda;
nr<-nrow(beta1);
nc<-ncol(beta1);
text(array(log(lambda[nc]),nr,1),beta1[,nc],features, vfont=c(Hershey$serif,Hershey$plain ), font=1, cex=0.5);
text(array(log(lambda[as.integer(nc*(3/4))]),nr,1),beta1[,as.integer(nc*(3/4))],features, vfont=c(Hershey$serif,Hershey$plain ),font=1, cex=0.5);
text(array(log(lambda[as.integer(nc*(2/4))]),nr,1),beta1[,as.integer(nc*(2/4))],features, vfont=c(Hershey$serif,Hershey$plain ),font=1, cex=0.5);
text(array(log(lambda[as.integer(nc*(1/4))]),nr,1),beta1[,as.integer(nc*(1/4))],features, vfont=c(Hershey$serif,Hershey$plain ),font=1, cex=0.5);
#lines(c(log(cvResult$lambda.1se),log(cvResult$lambda.1se)),c(min(beta1),max(beta1)),lty="dotted",lwd=1);
#lines(c(log(cvResult$lambda.min),log(cvResult$lambda.min)),c(min(beta1),max(beta1)),lty="dotted",lwd=1);
title("Coefficients of The Features As Lambda Change");
dev.off()
}