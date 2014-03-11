eigen_val<-function(Sb,Sw)
{
  library(geigen)
  eigen_fn<-geigen(Sb,Sw)
  eig_val<<-eigen_fn$values
  eig_vec<<-eigen_fn$vectors
}