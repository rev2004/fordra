readTesting <-
function(filename)
{  
  y<-read.pnm(file = filename)
  plot(y)
  test<-getChannels(y)
  dim(test)<-c(1,height*width)
  t_test<-t(test)
  test_fv<<-t(G)%*%t_test[,1]
 
}
