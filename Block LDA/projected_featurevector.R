projected_featurevector<-function(W_opt,X)
{
  Y<<-numeric()
  proj_matrix<<-array(0,dim=c(dimension_subimg,N2,tot_sam))
  X_t<-t(X)
  inc<<-0
  for(i in 1:tot_sam)
  {
    for(j in 1:N2)
    {
      proj_feature<<-t(W_opt)%*%X[,inc+j]
      Y<<-cbind(Y,proj_feature)
    }
    proj_matrix[,,i]<<-Y
    inc<<-inc+N2
    Y<<-numeric()
  }
}