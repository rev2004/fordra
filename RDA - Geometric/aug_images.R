aug_images<-function(trainpath,X)
{
  library(pixmap)
  no_row<-nrow(X)
  folder<-dir(trainpath)
  m<-1
  for(i in 1:length(folder))
  {
    subject<-folder[i];
    message("Augmenting images in Class ",subject)
    image_path<-paste(trainpath,subject,sep="/")
    images<-dir(image_path)
    for(k in 1:length(images))
    {
      for(j in 1:8)
      {
        x_vec<<-as.vector(X[m,])
        x_mat<<-matrix(x_vec,height,width)
        random_pix<<-runif(2)
        random_row<<-ceiling(height*random_pix[1])
        random_col<<-ceiling(width*random_pix[2])
        x_mat[random_row,random_col]<<-r/255
        pix_img<<-pixmapGrey(x_mat)
        plot(pix_img)
        arg_filename<-paste("aug",k,"_",j,".pgm",sep="")
        full_filename<-paste(image_path,arg_filename,sep="/")
        write.pnm(pix_img,file=full_filename)
      }
      m<-m+1
    }  
  } 
}