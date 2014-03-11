lrc <-function(path,testpath)
{
  readTraining(path)
  fldr<-dir(testpath)
  for(i in 1:length(fldr))
  {
    sub<-fldr[i];
    test_path<-paste(testpath,sub,sep="/")
    images<-dir(test_path)
    for(j in 1:length(images))
    {
      filename<-paste(testpath,sub,images[j],sep="/")
      print(filename)
      readTesting(filename)
      recognition(X,y,Q)
    }
  }
}
