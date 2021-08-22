##################################################################################
Unet_Pred_Fun=function(sex,predict_dir,Model_pth1) {
  resultBlob=NULL
  batch_size_global<<-1000
  batch_size<<-50
  ################################################# 
  date1=basename(labelInput)
  dice_coef <- function(y_true, y_pred, smooth = 1) {
    y_true_f <- k_flatten(y_true)
    y_pred_f <- k_flatten(y_pred)
    intersection <- k_sum(y_true_f * y_pred_f)
    (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  }
  dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
  attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"
  attr(dice_coef, "py_function_name") <- "dice_coef"
  
  if(exists("model")==F) {
    model <<- load_model_hdf5(Model_pth1  , custom_objects = c(dice_coef = dice_coef,
                                                               dice_coef_loss=dice_coef_loss)) }
  
  layer_output <- get_layer(model, index=1)$output
  shape=layer_output$shape								 
  dimModel<<-c(paste0(shape[1]),paste0(shape[2]))								 
  
  pth_resultBlob <<- paste0(labelInput,"\\Predict\\",sex,"_BlobTable_", date1, ".csv") 
  pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",sex, "_BlobTable_", date1, "_tmp.csv")
  listImage_glob <<-list.files(predict_dir, full.names = T,  recursive = T, include.dirs = F,pattern=".png")
  global_steps <<- round(length(listImage_glob)/batch_size_global)
  if(length(listImage_glob) > (global_steps*batch_size_global)) {global_steps=global_steps+1}
 
 
 
 
 ###################################################### 
   cl <- makePSOCKcluster(4) 
    clusterEvalQ(cl, {
      library(magick)     
      library(abind)     
      library(reticulate)
      library(EBImage)
     library(keras)
 
      imageRead <- function(image_file,
	                        dimModel1
                            ) {
							
		dimModel2<<-dimModel1					
        img <- image_read(image_file)
        img <- image_scale(img, paste0(dimModel2[1], "x", dimModel2[2], "!"))
      }
      
      img2arr <- function(image, 
	                      dimModel1) {
						  
		dimModel2<<-dimModel1				  
        result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
        dim(result) <- c(1, dimModel2[1], dimModel2[2], 3)
        return(result)
      }
      
  #    arr2img <- function(arr,
  #                        target_width = 1024,
  #                        target_height = 1024) {
  #      img <- image_read(arr)
  #      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
  #    }						  
    })
    registerDoParallel(cl)
  #################################################################################
  for (e in 1:global_steps) {
    batch_ind_global <- c(1:length(listImage_glob))[1:batch_size_global]
    listImage <- listImage_glob[batch_ind_global]
    listImage=listImage[is.na(listImage)==F]
    if (length(listImage_glob) > length(listImage)) {
      listImage_glob <<- listImage_glob[-batch_ind_global]
    }
    listImage=listImage
    listImageBl=listImage
    listImgName<- basename(listImage) 
    batch_size=batch_size
    steps= round(length(listImage)/batch_size)
    if(length(listImage) > (steps*batch_size)) {steps=steps+1} 
    ########################################## 
   
    #######################################################################################
    test_generator <- function(listImage,batch_size,dimModel) {			  
      function()   {     
     
        batch_ind <- c(1:length(listImage))[1:batch_size]
        batch_images_list <- listImage[batch_ind]
        batch_images_list=batch_images_list[is.na(batch_images_list)==F]
        if (length(listImage) > length(batch_images_list)) {
          listImage <<- listImage[-batch_ind]
        }
        
        dimModel0=dimModel
        x_batch <- foreach(y = 1:length(batch_images_list)) %dopar% {
          img <- imageRead(image_file = batch_images_list[y],dimModel1=dimModel0)
          arr <- img2arr(img,dimModel1=dimModel0)
        }
        x_batch <- do.call(abind, c(x_batch, list(along = 1)))
        result <- list(keras_array(x_batch))
      }
    }
    
    test_iterator <- py_iterator(test_generator(listImage,batch_size,dimModel=dimModel))	
	#iter_next(test_iterator)
    preds <- predict_generator(model, test_iterator, steps = steps)
    ############
	predsDir=paste0(labelInput,"\\Predict\\Preds")
	if(dir.exists(predsDir)==F){dir.create(predsDir)}
	pthSavePreds=paste0(predsDir,"\\Preds_",sex,"_",e)
	Preds1=list(preds=preds,DimPreds=dim(preds),dimModel=dimModel,listImageBl=listImageBl)
	saveRDS(Preds1,pthSavePreds)
		
	
}
stopCluster(cl)
rm(model)
}

##############################################################################################################################################
if (Species== "NFS_Adult") {
  Unet_Pred_Fun(sex="NFS_Adult",
                predict_dir=paste0(labelInput,"\\Predict\\Haulout"),
                Model_pth1=NFS_Adult_Model_pth)
}				
if (Species== "SSL_Adult") {     
  Unet_Pred_Fun(sexsex="SSL_Adult",
                predict_dir=paste0(labelInput,"\\Predict\\Haulout"),
                Model_pth1=SSL_Adult_Model_pth) }	
if (Species== "NFS_Pup") {     
  Unet_Pred_Fun(sex="NFS_Pup",
                predict_dir=paste0(labelInput,"\\Predict\\Haulout"),
                Model_pth1=NFS_Pup_Model_pth) }
if (Species== "SSL_Pup") {     
  Unet_Pred_Fun(sex="SSL_Pup",
                predict_dir=paste0(labelInput,"\\Predict\\Haulout"),
                Model_pth1=SSL_Pup_Model_pth) }












