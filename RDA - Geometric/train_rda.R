train_rda<-function(path)
{
  start.time<-Sys.time()
  message("Reading Images...")
  read_train_images(path)
  message("Finding Scatter matrices Sw and Sb...")
  find_Sw_Sb(X,y)
  message("Augmenting training images and adding it to training dataset...")
  aug_images(trainpath,X)
  message("Finding the Regularization Parameter(lambda)...")
  id<<-diag(width*height)
  lambda_es<<-r*255
  lambda_es<<-lambda_es*lambda_es
  dim_img<<-height*width
  lambda_es<<-lambda_es/dim_img
  id<<-lambda_es*id
  message("Reading images from the training dataset including augmented images...")
  read_train_images(path)
  message("Finding New Scatter matrices Sw and Sb... ")
  find_new_Sw_Sb(X,y)
  Sw_new<<-Sw+id
  message("Finding Generalized eigen vectors of Sb and Sw...")
  eigen_val(Sb,Sw_new)
  message("Finding Optimal projection matrix W_opt...")
  find_Wopt(eig_vec,eig_val)
  message("Finding Projected feature vector of training images...")
  Projected_featurevector(W_opt,X)
  message("Training Completed.")
  end.time<-Sys.time()
  time.taken<-end.time-start.time
  print(time.taken)
}