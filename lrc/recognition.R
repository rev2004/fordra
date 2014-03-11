recognition <-
function(X,y,Q)
{
  vector_size<<-dim(X)
  n<-vector_size[1]
  d<-vector_size[2]
  label<<-table(y)
  distance<<-numeric()
  labs<<-as.numeric(names(label))
  Cls<<-length(labs)
  test1<<-as.vector(Q)  
  for(i in 1:Cls)
  {
    Xi<-X[which(y==i),]
    n<-nrow(Xi)
    t_Xi<-t(Xi)
    Bi<-solve(Xi%*%t_Xi)%*%Xi%*%Q
    yi<-t_Xi %*% Bi
    pred<-as.vector(yi)
    dist_i<-sqrt(sum((test1-pred)^2))
    distance<<-cbind(distance,dist_i)
  }
  testclass<<-ceiling(which.min(distance))
  print("Predicted Class:")
  print(testclass)
}
