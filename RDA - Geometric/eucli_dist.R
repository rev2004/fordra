eucli_dist<-function(test_proj_feature,Y)
{
 test<-as.vector(test_proj_feature)
 distance<<-numeric()
 for(i in 1:ncol(Y))
 {
   train<-as.vector(Y[,i])
   eud_dis<-sqrt(sum((test-train)^2))
   distance<<-cbind(distance,as.matrix(eud_dis))
 }
 message("Actual Class: ",y[which.min(distance)])
}