useSomeClasses<-function(data,classes,select=unique(classes),regions)
{
  select=unique(select);
  dataP=NULL;
  numSelCl=length(select);
  if(numSelCl<2)
  {return(dataP);} # return NULL
  
  c=1;
  ind=logical(length(classes)); # indices of selected samples
  while(c<=numSelCl)
  {
    ind=(ind|(classes==select[c]));
    c=c+1;
  }
  dataP$data=data[ind,];
  dataP$classes=classes[ind];
  dataP$regions=regions[ind,];
  return(dataP);
}
