# write the prediction to a bed/bedGraph file
writePrediction<-function(fileName="classesPredicted.R",classesPredicted,regions,format="bed",merge=FALSE)
{
  source("getColorforElements.R");
  source("mergeRegions.R");
  if(format=="bed")
  {
    # write head  
    cat(sprintf("browser position %s:%s-%s",regions$V1[1],as.character(regions$V2[1]),as.character(regions$V3[1])),file=fileName,append=FALSE);
    cat(sprintf("browser pack %s",fileName),file=fileName,append=TRUE);
    cat(sprintf("track name=\"%s\" description=\"%s\" visibility=3 itemRgb=\"On\" useScore=0 db=hg19",fileName,fileName),file=fileName,append=TRUE);
    
    # write body
    rgbCodes=c("255,215,0", #gold, EnhancerActive
             "184,134,11", # dark golden rod, EnhancerInactive
             "255,0,0", # red, PromoterActive
             "250,128,114", # salmon, PromoterInactive
             "0,128,0", # green, Exon
             "128,128,128"); # gray, Unknown
  classLabels=c("EnhancerActive","EnhancerInactive","PromoterActive","PromoterInactive","Exon","Unknown");
  names(rgbCodes)=classLabels;
  colors=getColorforElements(classesPredicted,rgbCodes);
  regions$V4=classesPredicted;
  regions$V5=rep(0,length(classesPredicted));
  regions$V6=rep(".",length(classesPredicted));
  regions$V7=regions$V2;
  regions$V8=regions$V3;
  regions$V9=colors;
    # if need to merge regions with the same label
    if(merge==TRUE)
    {
      regions=mergeRegions(regions);
    }
  
  } # end of if
  if(format=="bedGraph")
  {
    # write head  
    cat(sprintf("browser position %s:%s-%s",regions$V1[1],as.character(regions$V2[1]),as.character(regions$V3[1])),file=fileName,append=FALSE);
        cat(sprintf("browser full %s",fileName),file=fileName,append=TRUE);
    cat(sprintf("track type=bedGraph name=\"%s\" description=\"%s\"graphType=bar visibility=1 viewLimits=0:1 color=255,0,0 useScore=1",fileName,fileName),file=fileName,append=TRUE);
    
    regions=data.frame(regions$V1,regions$V2,regions$V3,classesPredicted);
  }
  write.table(regions, file = fileName, append = TRUE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE);
  return(regions);
}
