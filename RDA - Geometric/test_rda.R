test_rda<-function(testpath)
{
  test_folder<-dir(testpath)
  for(i in 1:length(test_folder))
  {
    test_subject<-test_folder[i];
    test_image_path<-paste(testpath,test_subject,sep="/")
    test_images<-dir(test_image_path)
    for(j in 1:length(test_images))
    {
      test_image<-paste(testpath,test_subject,test_images[j],sep="/")
      print(test_image)
      message("Expected Class: ",test_subject)
      read_test_image(test_image)
      eucli_dist(test_proj_feature,Y)
    }
  }
}