library(pixmap)
sub_image<-function(path,N)
{
  folder<-dir(path)
  tot_sam<<-0
  img_label<<-numeric()
  X<<-numeric()
  y<<-numeric()
  N2<<-N*N
  for(sub_folder in 1:length(folder))
  {
    subject<-folder[sub_folder];
    message("Subject:",subject)
    image_path<-paste(path,subject,sep="/")
    images<-dir(image_path)
    for(img in 1:length(images))
    {
      print(images[img])
      img_label<<-cbind(img_label,subject)
      tot_sam<<-tot_sam+1
      filename<-paste(path,subject,images[img],sep="/")
      x<-read.pnm(file = filename)
      t<-getChannels(x)
      H<<-nrow(t)
      W<<-ncol(t)
      subimg_num <- 0
      subimg_mat = array(0, dim=c(H/N,W/N,N2))
      for(j in 1:N)
      {
        for(k in 1:N)
        {
          subimg_num <- subimg_num+1
          for(alpha in 1:((W/N)))
          {
            for(beta in 1:((H/N)))
            {
              subimg_mat[beta,alpha,subimg_num] <- t[((H/N)*(k-1)+beta),((W/N)*(j-1)+alpha)] 
            } 
          }
        }
      }
      for(n in 1:N2)
      {
        subimg<<- array(0,dim=c(H/N,W/N))
        subimg<<- subimg_mat[,,n]
        dimension_subimg<<-floor(W/N)*floor(H/N)
        dim(subimg)<<-c(dimension_subimg,1)
        X<<-cbind(X,subimg)
        y<<-cbind(y,subject)
     }
    }  
  } 
}