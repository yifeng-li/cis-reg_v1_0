# log_2-transform
# example:
# M=matrix(7,3,4);
# logTransform(M,c(3,4));
logTransform<-function(data,exclude)
{
  nc=ncol(data);
  c=1;
  while(c<=nc)
  {
    if(any(exclude==c))
    {exclude=exclude[-which(exclude==c)];
     cat(exclude)
     c=c+1;
     next; 
    }
    data[,c]=log2(data[,c]+1);
    c=c+1;
  }
  return(data);
}