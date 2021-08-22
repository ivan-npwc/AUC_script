if (is.null(Rookery_polygon) == F) {test_dir<<-paste0(labelInput,"\\Predict\\Rookery")
 }  else {
test_dir <<- paste0(labelInput,"\\Predict\\Garem")}

##################################
Unet_garem=function () {
  UnetPredict (
    test_dir <<- test_dir,
    sex<<-"Garem", 
    batch_size_global<<-1000,
    batch_size<<-30
  )
}
##################################################################################
UnetPredict=function(test_dir,sex, batch_size_global,batch_size) { 
  
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
    model <<- load_model_hdf5(Model_pth  , custom_objects = c(dice_coef = dice_coef,
                                                              dice_coef_loss=dice_coef_loss)) 
    showNotification("Unet successful loaded")}
  
  pth_resultBlob <<- paste0(labelInput,"\\Predict\\",sex,"_BlobTable_", date1, ".csv") 
  pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",sex, "_BlobTable_", date1, "_tmp.csv")
  listImage_glob <<-list.files(test_dir, full.names = T,  recursive = T, include.dirs = F,pattern=".png")
  global_steps <<- round(length(listImage_glob)/batch_size_global)
  if(length(listImage_glob) > (global_steps*batch_size_global)) {global_steps=global_steps+1}
  withProgress(message = 'Unet predict', value = 0, {
    #################################################################################
    for (e in 1:global_steps) {
      e <<- e
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
      cl <- makePSOCKcluster(4) 
      clusterEvalQ(cl, {
        library(magick)     
        library(abind)     
        library(reticulate)
        library(EBImage)
        
        imageRead <- function(image_file,
                              target_width = 256, 
                              target_height = 256) {
          img <- image_read(image_file)
          img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
        }
        
        img2arr <- function(image, 
                            target_width = 256,
                            target_height = 256) {
          result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
          dim(result) <- c(1, target_width, target_height, 3)
          return(result)
        }
        
        arr2img <- function(arr,
                            target_width = 1024,
                            target_height = 1024) {
          img <- image_read(arr)
          img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
        }						  
        
      })
      registerDoParallel(cl)
      #######################################################################################
      test_generator <- function(listImage,batch_size) {			  
        function()   {     
          
          batch_ind <- c(1:length(listImage))[1:batch_size]
          batch_images_list <- listImage[batch_ind]
          batch_images_list=batch_images_list[is.na(batch_images_list)==F]
          if (length(listImage) > length(batch_images_list)) {
            listImage <<- listImage[-batch_ind]
          }
          
          
          x_batch <- foreach(y = 1:length(batch_images_list)) %dopar% {
            img <- imageRead(image_file = batch_images_list[y])
            arr <- img2arr(img)
          }
          x_batch <- do.call(abind, c(x_batch, list(along = 1)))
          result <- list(keras_array(x_batch))
        }
      }
      
      test_iterator <- py_iterator(test_generator(listImage,batch_size))	
      preds <- predict_generator(model, test_iterator, steps = steps)
      ############
      resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
        
        name=basename(listImageBl[i])
        img_pth=listImageBl[i]
        #############
        mask=preds[i, , , ]
        img <- t(mask)
        dim(img) <- c(256, 256, 1)
        img = getFrame(img, 1)
        nmask = thresh(img, 18, 18, 0.009)  
        nmask <- fillHull(nmask)
        nmask = opening(nmask, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
        nmask = fillHull(nmask)
        nmask = bwlabel(nmask)
        
        if (max(nmask)!=0) {   
          fts = computeFeatures.moment(nmask)  # coordinat
          shapeFeatures <- computeFeatures.shape(nmask) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
          BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=img_pth)  


#     if (checkUnet!=0) {
#   if (sample(1:checkUnet)==1) {
#          		pthSave= paste0(labelInput,"\\Predict\\Check")
#				if (dir.exists(pthSave)==F) {dir.create(pthSave)}
#              img1=image_read(img_pth)
#              blob=image_read(img0)
#			  blob=image_scale(blob,"1024x1024!")
#              Check=image_composite(blob,img1,operator = "blend", compose_args="70")
#             save1=paste0(pthSave,"\\",name)
#             image_write(Check,save1)
#		  }}

		  
          
        }
      }
      
      
      if (exists("resultBlob")== F) {
        resultBlob=resultBlob_tmp } else {
          resultBlob=rbind(resultBlob,resultBlob_tmp)}
      incProgress(1/global_steps, detail = paste("Doing part", e))
      Sys.sleep(0.1)
      write.csv(resultBlob,pth_resultBlob_tmp,row.names = F)
	  stopCluster(cl)
    }
  })  
  write.csv(resultBlob,pth_resultBlob,row.names = F)
  }
Unet_garem()