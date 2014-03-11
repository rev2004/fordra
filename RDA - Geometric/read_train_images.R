read_train_images<-function(path)
{
  library(pixmap)
  folder<-dir(path)
  class<-1
  X<<-numeric()
  y<<-numeric()
  for(i in 1:length(folder))
  {
    subject<-folder[i];
    message("Subject: ",subject)
    image_path<-paste(path,subject,sep="/")
    images<-dir(image_path)
    for(j in 1:length(images))
    {
      print(images[j])
      filename<-paste(path,subject,images[j],sep="/")
      x<-read.pnm(file = filename)
      t<-getChannels(x)
      img_size<-dim(t)
      height<<-img_size[1]
      width<<-img_size[2]
      dim(t)<-c(1,height*width)
      X<<-rbind(X,t)
      y<<-cbind(y,subject)
    }
    class<-class+1
  }
}