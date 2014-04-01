sumCol<-function(M)
{
  # example:
#   M=cbind(c(1,2,3),c(5,2,8),c(10,15,13));
#   sumM=sumCol(M);
  
  nr=nrow(M);
  nc=ncol(M);
  sumM=vector(mode=mode(M),length=nr);
  c=1;
  while(c<=nc)
  {
    sumM[c]=sum(M[,c]);
    c=c+1;
  }
  return(sumM);
}