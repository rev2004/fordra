nlda <-
function(X,y)
{
  start.time<-Sys.time()
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
  
  Ht<<-Sw+Sb
  temp<<-svd(Ht)
  U<<-temp$u;
  V<<-temp$v;
  S<<-diag(temp$d)
  Sw_new<<-t(U)%*%Sw%*%U
  Sb_new<<-t(U)%*%Sb%*%U
  W<<-Null(Sw_new)
  M<<-t(W)%*%Sb_new%*%W
  G<<-U%*%W%*%M
  Y<<-numeric()
  for(i in 1:nrow(X))
  {
    X_t<-t(X)
    proj_fv<<-t(G)%*%X_t[,i]
    Y<<-cbind(Y,proj_fv)
  }
  
  end.time<-Sys.time()
  time.taken<-end.time-start.time
  print(time.taken)
}
