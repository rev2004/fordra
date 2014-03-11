readTesting <-
function(filename)
{ 
  y<-read.pnm(file = filename)
  plot(y)
  testimage<-getChannels(y)
  dim(testimage)<-c(1,height*width)
  Q<<-t(testimage)
}
