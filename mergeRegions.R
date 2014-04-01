mergeRegions<-function(regions)
{
  # regions: data.frame, or string matrix, at least have 4 columns, usually should have 9 columns
  # example:
#   regions=rbind(c("chr1","100","110","Enhancer"),
#                 c("chr1","110","120","Enhancer"),
#                 c("chr1","120","130","Enhancer"),
#                 c("chr1","130","140","Promoter"),
#                 c("chr1","140","150","Enhancer"),
#                 c("chr1","150","160","Promoter"),
#                 c("chr1","160","170","Promoter"));
#   M=mergeRegions(regions);
  # Yifeng Li, Mar. 27, 2014
  
  regM=as.matrix(regions);
  numReg=nrow(regions);
  inds=!logical(numReg); # initial indices, all true to keep
  r=1;
  while(r<numReg)
  {
    s=0; # step
    while(r+s+1<=numReg && regM[r+s,4]==regM[r+s+1,4] && regM[r+s,3]==regM[r+s+1,2])
    {
      inds[r+1+s]=FALSE; # the row need to remove
      regM[r,3]=regM[r+s+1,3]; # copy the end coordiante
      s=s+1;
    }
    r=r+1;
  }
  regM=regM[inds,];
  return(regM);
}
