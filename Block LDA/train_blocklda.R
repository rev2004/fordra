train_blocklda<-function(trainpath,N)
{
  start.time<-Sys.time()
  message("Reading Images...")
  sub_image(trainpath,N)
  message("Finding Scatter matrices Sw and Sb...")
  find_Sw_Sb(X,y)
  message("Finding Optimal projection matrix W_opt...")
  find_Wopt(Sb,Sw)
  message("Finding Projected feature vector of training images...")
  projected_featurevector(W_opt,X)
  message("Training Completed.")
  end.time<-Sys.time()
  time.taken<-end.time-start.time
  print(time.taken)
}