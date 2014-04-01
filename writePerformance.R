writePerformance<-function(perf,fileName="performance.txt")
{
  # perf: returned by meanPerform function
  
  # Save Accuracies and Confusion Matrix 
  meanStd=round(cbind(perf$perf,perf$std),4);
  colnames(meanStd)=c("Mean_Accuracy", "STD");
  write.table(meanStd, file = fileName, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names =TRUE);
  write.table(round(perf$conMat,4), file = fileName, append = TRUE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names =TRUE);
}  


