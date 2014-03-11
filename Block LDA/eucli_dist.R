eucli_dist<-function(test_proj_feature,proj_matrix)
{
 test<-as.vector(test_proj_feature)
 distance<<-numeric()
 for(i in 1:tot_sam)
 {
   train<-as.vector(proj_matrix[,,i])
   eud_dis<-sqrt(sum((test-train)^2))
   distance<<-cbind(distance,as.matrix(eud_dis))
 }
 message("Actual Class: ",img_label[which.min(distance)])
}