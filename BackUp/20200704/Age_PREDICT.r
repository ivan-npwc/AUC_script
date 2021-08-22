source("Modules/KMLwrite_function.r")
##################################################################################
Unet_Pred_Fun=function(
    labelInput,
    Species,                       
	modelAgePTH= paste0("System data/weights/",SSL_Age_pth),
	batch_size = 32,
	Age_Name= c("An","F", "J", "Sa")) {
					  	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	 
	 
     AgeFolder= paste0(labelInput,"\\Predict\\Age_predict")
	 images_dir1 = paste0(AgeFolder,"\\Big")
     images_dir2 = paste0(AgeFolder,"\\Small")
     
	 PthTblAgeRef=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"AgeRef.csv")
	 PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_AgeLatLon.csv")
	 Rookery_polygonPTH=paste0(labelInput,"\\Polygons\\Rookery\\Rookery.shp")
     kmlPathSave=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_AgeLatLon.kml")
	 ProbAge_PTH=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_ProbAge.csv")
	 
	ImgsCount=length(list.files(images_dir1))
    steps=round(length(list.files(images_dir1))/batch_size)
	    if ((batch_size * steps) < ImgsCount) {steps=1+steps}
		
     	if(exists("model_age_read")==F) {model_age_read <<- load_model_hdf5(modelAgePTH)}
		 model_age_read %>% compile(
            optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
            loss = "categorical_crossentropy",
            metrics = c("accuracy"))
 ###################################################### 
   cl <- makePSOCKcluster(detectCores ()-2) 
    clusterEvalQ(cl, {
      library(magick)     
      library(abind)     
      library(reticulate)
      library(EBImage)
     library(keras)
 
   
imagesRead <- function(image_file1,
                       image_file2) {
  img1 <- image_read(image_file1);img1 <- image_scale(img1, "256x256!")
  img2 <- image_read(image_file2);img2 <- image_scale(img2, "150x150!")
  list(img1 = img1, img2 = img2)
}

img2arr <- function(image, 
                    target_width,
                    target_height) {
  result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
  dim(result) <- c(1, target_width, target_height, 3)
  return(result)
}
    })
    registerDoParallel(cl)
#######################################################################################
  Batch_generator <- function(images_dir1,
                              images_dir2, 
                              batch_size) {
  images_iter1 <- list.files(images_dir1,  
                            full.names = T)# for current epoch
  images_all1 <- list.files(images_dir1, 
                           full.names = T) # for next epoch
						   ##
  images_iter2 <- list.files(images_dir2, 
                           full.names = T) # for current epoch
  images_all2 <- list.files(images_dir2, 
                          full.names = T) # for next epoch
  function() {
    # start new epoch
    if (length(images_iter1) < batch_size) {
      images_iter1 <<- images_all1
      images_iter2 <<- images_all2
	  batch_ind <- c(1:length(images_iter1))
    } else   { batch_ind <- c(1:batch_size)}
 
    batch_images_list1 <- images_iter1[batch_ind]          # select images acording ID batch
    images_iter1 <<- images_iter1[-batch_ind]              # remove from all imgs list ID batch
    batch_images_list2 <- images_iter2[batch_ind]          #
    images_iter2 <<- images_iter2[-batch_ind]
	
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file1 = batch_images_list1[i],
                             image_file2 = batch_images_list2[i])
      # without augmentation	  
      # return as arrays
      x_y_arr <- list(x = img2arr(x_y_imgs$img1,256,256),
                        y = img2arr(x_y_imgs$img2,150,150))
    }
      x_y_batch <- purrr::transpose(x_y_batch)
       x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
       y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
	   
    result <-  list(keras_array(x_batch), 
                    keras_array(y_batch))
    return(result)
  }
}
    
    predict_iterator <- py_iterator(Batch_generator(images_dir1=images_dir1,
	                                                  images_dir2=images_dir2, 
                                                      batch_size=batch_size))	
	#iter_next(predict_iterator)
    preds <- predict_generator(model_age_read, predict_iterator,steps)
	
	preds3=NULL
    colnames(preds)=Age_Name
	 for (k in 1: ImgsCount) {
	 row1= preds[k,]
	 cellMax= row1[row1==max(row1)]
	 Age=   row.names(data.frame(cellMax))
	 Prob= data.frame(cellMax)$cellMax
	 preds2= data.frame(name= Age,preds=Prob)
	 preds3=rbind(preds3,preds2)
	 }
	 preds3$imgPth=list.files(images_dir1,full.names=T)
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))
	  
      preds3$link=basename(as.character(preds3$imgPth))
	  
      TableGeoAge <<- merge(x=preds3,y=TblAgeRef,by="link",all=T)
      TableGeoAge1 <<- data.frame(lon=TableGeoAge$lon,lat=TableGeoAge$lat,age= TableGeoAge$name)
	  
	  write.csv(TableGeoAge, ProbAge_PTH)  
      write.csv(TableGeoAge1,PTH_TableGeoAge)
      KMLwrite(Img3=TableGeoAge1,kmlPathSave)
	 
	 
stopCluster(cl)
}
##############################################################################################################################################
Unet_Pred_Fun(labelInput=labelInput,
              Species= Species)