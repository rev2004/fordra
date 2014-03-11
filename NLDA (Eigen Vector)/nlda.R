nlda<-function(X,y)
{
  library(psych)
  library(MASS)
  message("Finding Scatter matrices Sw and Sb...")
  vector_size<<-dim(X)
  n<-vector_size[1]
  d<-vector_size[2]
  label<<-table(y)
  labs<<-as.numeric(names(label))
  Cls<<-length(labs)
  Sw<<-matrix(0,d,d)
  Sb<<-matrix(0,d,d)
  mu<-colMeans(X)
  dim(mu)<-c(1,d)
  for(i in 1:Cls)
  {
    Xi<-X[which(y==i),]
    n<-nrow(Xi)
    mu_i=colMeans(Xi)
    dim(mu_i)=c(1,d)
    mu_i_new<-numeric()
    for(j in 1:n)
    {
      mu_i_new<-rbind(mu_i_new,mu_i)
    }
    Xi<-Xi-mu_i_new
    Xit=t(Xi)
    Sw<<-Sw+(Xit%*%Xi)
    Sb<<-Sb+(t(mu_i-mu)%*%(mu_i-mu))
  }
  message("Finding Eigen vectors of St...")
  St<<-Sb+Sw
  eigen_fn<-eigen(St)
  eig_val<<-eigen_fn$values
  p<<-eigen_fn$vectors
  message("Finding Sw`...")
  Sw_new<<-t(p)%*%Sw%*%p
  message("Finding null space of Sw`...")
  v<<-Null(Sw_new)
  message("Finding optimal projection matrix W_opt...")
  W_opt<<-p%*%v
  message("Finding Projected feature vector of training images...")
  Y<<-numeric()
  for(i in 1:nrow(X))
  {
    X_t<-t(X)
    proj_feature<<-t(W_opt)%*%X_t[,i]
    Y<<-cbind(Y,proj_feature)
  }
}
