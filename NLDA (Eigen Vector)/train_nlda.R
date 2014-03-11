train_nlda<-function(trainpath)
{
  start.time<-Sys.time()
  message("Reading Images...")
  read_train_images(trainpath)
  nlda(X,y)
  message("Training Completed.")
  end.time<-Sys.time()
  time.taken<-end.time-start.time
  print(time.taken)
}