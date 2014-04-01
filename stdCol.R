stdCol<-function(M)
{
  # example:
  #   M=cbind(c(1,2,3),c(5,2,8),c(10,15,13));
  #   stdM=stdCol(M);
  
  nr=nrow(M);
  nc=ncol(M);
  stdM=vector(mode="numeric",length=nr);
  c=1;
  while(c<=nc)
  {
    stdM[c]=round(sd(M[,c]),4);
    c=c+1;
  }
  return(stdM);
}