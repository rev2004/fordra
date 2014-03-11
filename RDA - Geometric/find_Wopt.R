find_Wopt<-function(eig_vec,eig_val)
{
  eig_val_sort<<-sort(eig_val,decreasing=TRUE)
  W_opt<<-numeric()
  for(i in 1:Cls)
  {
    position<<-match(eig_val_sort[i],eig_val)
    eig_vector<-eig_vec[,position]
    largest_eig_matrix<-matrix(eig_vector,width*height,1)
    W_opt<<-cbind(W_opt,largest_eig_matrix)
  }
}