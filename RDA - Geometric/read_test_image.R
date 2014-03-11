read_test_image<-function(filename)
{  
  library(pixmap)
  x<-read.pnm(file = filename)
  plot(x)
  test_image<<-getChannels(x)
  dim(test_image)<<-c(1,height*width)
  test_image_t<<-t(test_image)
  test_proj_feature<<-t(W_opt)%*%test_image_t[,1]
}