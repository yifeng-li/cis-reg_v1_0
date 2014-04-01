getColorforElements<-function(classes,rgbCodes)
{
  # example:
#   classes=c("EnhancerActive","EnhancerActive","EnhancerInactive","EnhancerInactive","PromoterActive","PromoterActive","PromoterInactive");
#   classLabels=c("EnhancerActive","EnhancerInactive","PromoterActive","PromoterInactive","Exon","Unknown");
#   rgbCodes=c("255,215,0", #gold, EnhancerActive
#   "184,134,11", # dark golden rod, EnhancerInactive
#   "255,0,0", # red, PromoterActive
#   "250,128,114", # salmon, PromoterInactive
#   "0,128,0", # green, Exon
#    "128,128,128"); # gray, Unknown
#   names(rgbCodes)=classLabels;
#   colors=getColorforElements(classes,rgbCodes);
  # Yifeng Li, Mar. 27, 2014
  
  num=length(classes);
  classUnik=unique(classes);
  numCl=length(classUnik);
  colors=classes; # initialize the vector of color codes
  c=1;
  while(c<=numCl)
  {
    colors[classes==classUnik[c]]=rgbCodes[classUnik[c]];
    c=c+1;
  }
  return(colors);
}