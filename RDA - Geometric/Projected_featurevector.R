Projected_featurevector<-function(Wopt,X)
{
  Y<<-numeric()
  for(i in 1:nrow(X))
  {
    X_t<-t(X)
    proj_feature<<-t(W_opt)%*%X_t[,i]
    Y<<-cbind(Y,proj_feature)
  }
}