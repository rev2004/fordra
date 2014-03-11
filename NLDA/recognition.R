recognition <-
function(test_fv,Y)
{
 test<-as.vector(test_fv)
 distance<<-numeric()
 for(i in 1:ncol(Y))
 {
   train<-as.vector(Y[,i])
   eud_dis<-sqrt(sum((test-train)^2))
   distance<<-cbind(distance,as.matrix(eud_dis))
 }
 print("Predicted Class:")
 print(y[which.min(distance)])

}
