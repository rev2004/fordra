library(pixmap)
read_test_image<-function(filename)
{  
  x<-read.pnm(file = filename)
  plot(x)
  test_image<-getChannels(x)
  H<<-nrow(test_image)
  W<<-ncol(test_image)
  subimg_num <- 0
  N<-sqrt(N2)
  test_subimg<<-array(0, dim=c(H/N,W/N,N2))
  for(j in 1:N)
  {
    for(k in 1:N)
    {
      subimg_num <- subimg_num+1
      for(alpha in 1:((W/N)))
      {
        for(beta in 1:((H/N)))
        {
          test_subimg[beta,alpha,subimg_num]<<-test_image[((H/N)*(k-1)+beta),((W/N)*(j-1)+alpha)] 
        } 
      }
    }
  }
  whole_img<<-numeric()
  subimg_testmat<- array(0,dim=c(H/N,W/N))
  for(n in 1:N2)
  {
    subimg_testmat<- test_subimg[,,n]
    subimg_obj <- pixmapGrey(subimg_testmat)
    plot(subimg_obj)
    dim(subimg_testmat)<-c(dimension_subimg,1)
    whole_img<<-cbind(whole_img,subimg_testmat)  
  }
  test_proj_feature<<-numeric()
  for(j in 1:N2)
  {
   test_proj_feature_sub<<-t(W_opt)%*%whole_img[,j]
   test_proj_feature<<-cbind(test_proj_feature,test_proj_feature_sub)
  }
}

