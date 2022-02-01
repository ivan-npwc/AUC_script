#UnetTrain256=function (
#                      trainDir1=trainDir,
#                       weight1=Weight,
#                       Model_base1=Model_base, #"D:\\CIC\\SHINY_NFS_AUTOCOUNT\\20190505\\data\\TRAIN\\Val_0.43_epoch_16_NFS_256_Points.h5",
#					   TrainIndex=0.85,  # how many data use for train and validate (1-TrainIndex)
#					   BatchIntens= BatchIntens1,  # all train set will be procesing in one epoch 3 times
#					   Split=Split1,	        # if split for train and validate
#					   Smooth=1,       # smoth for accuracy model estimate  
#					   batch_size=batch_size1,
#					   epochs=epochs1,
#					   dfrmn=F
#					   ) {
		

                      trainDir1=trainDir
                       weight1=Weight
                       Model_base1=Model_base
					   TrainIndex=0.9
					   BatchIntens= BatchIntens1
					   Split= T  #Split1
					   Smooth=1 
					   batch_size=batch_size1
					   epochs=epochs1
					   dfrmn=F
                      TypeTrain

			 

		
library(magick)
library(abind)
library(reticulate)
library(parallel)
library(doParallel)
library(foreach)
#library(tensorflow)
library(keras)
library(tools)

#use_condaenv("r-tensorflow")
#sess = tf$Session()
#hello <- tf$constant('Hello, TensorFlow!')
#sess$run(hello)

 source("Modules/TrainLog.r")

Species=basename(trainDir1)
dateTrain=format(Sys.time(),  "%Y%m%d") 
checkpoint_dir=paste0(trainDir1,"\\Checkpoints"); if (dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}

images_dir= paste0(trainDir1, "\\Image")
masks_dir= paste0(trainDir1, "\\Mask")

     listImgS <<- list.files(images_dir)

	 listImgSNoExt=file_path_sans_ext(listImgS)
	 
	                   if (length(listImgS)==0) {stop ("No Images found")}
	  
     ListMskS<<- list.files(masks_dir)
	
	 MskNoExt=file_path_sans_ext(ListMskS)

     deleteListImgs<<- listImgSNoExt[!(listImgSNoExt %in% MskNoExt)] # here is IMGS DELETE without msk 
     deleteListMsks<<- MskNoExt[!(MskNoExt %in% listImgSNoExt)]
   
     deleteListImgs1=paste0(images_dir,"\\",deleteListImgs,".jpg")
     deleteListMsk1=paste0(masks_dir,"\\",deleteListMsks,".png")
	 unlink(deleteListImgs1)
	 unlink(deleteListMsk1)



       train_samples <- length(list.files(images_dir))
       train_index <- sample(1:train_samples)[1:round(train_samples * TrainIndex)] 
       val_index0 <-  c(1:train_samples)
       val_index=val_index0[!(val_index0 %in% train_index)]
	   
	  print(paste0("Found  ", length(train_index)," Images for TRAIN ",Species)) 
	  print(paste0("Found  ", length(val_index)," Images for VALIDATE ",Species)) 
	  
       steps_per_epoch= round(length(train_index)/batch_size*BatchIntens)
	     if (Split==FALSE) {
		  train_index <- sample(1:train_samples)[1:round(train_samples)] 
           val_index=sample(1:train_samples)[1:round(train_samples)] 
            steps_per_epoch= round(train_samples/batch_size*BatchIntens)
		 }
#####################################################################
K <- backend()
dice_coef <- function(y_true, y_pred, smooth = Smooth) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
}
attr(dice_coef, "py_function_name") <- "dice_coef"

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"

####
bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) +
    (1 - dice_coef(y_true, y_pred))
  return(result)
}
########
if ( exists("unet1")==F){source("Modules/UnetVGG16Create.r")}
if(TypeTrain=="Retrain"){setWEight=readRDS(weight1); set_weights(unet1,setWEight)}


														
														
unet1 <- unet1 %>%
        compile(
        optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),     #optimizer_nadam              
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
    )														
														
														
unet1
###########
early_stopping <- callback_early_stopping(patience = 4)
#filepath <<- file.path(checkpoint_dir, "Val_{val_dice_coef:.2f}_epoch_{epoch:02d}_256.h5")
BaseName <<- basename(file.path(checkpoint_dir, "Val_{val_dice_coef:.2f}_epoch_{epoch:02d}_256.h5"))
filepath <<- paste0(checkpoint_dir,"\\",Species,"_",dateTrain,"_",BaseName)

cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 1,
  #save_weights_only = TRUE,
  verbose = 1)
#########################################################################
#########################################################################
imagesRead <- function(image_file,
                       mask_file,
                       target_width = 256, 
                       target_height = 256) {
  img <- image_read(image_file)
  img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
  
  mask <- image_read(mask_file)
  mask <- image_scale(mask, paste0(target_width, "x", target_height, "!"))
  list(img = img, mask = mask)
}
#################################################################
randomBSH <- function(img,
                      u = 0,
                      brightness_shift_lim = c(90, 115), # percentage
                      saturation_shift_lim = c(90, 115), # of current value
                      hue_shift_lim = c(100, 100)) {
  
  if (rnorm(1) < u) return(img)
  
  brightness_shift <- runif(1, 
                            brightness_shift_lim[1], 
                            brightness_shift_lim[2])
  saturation_shift <- runif(1, 
                            saturation_shift_lim[1], 
                            saturation_shift_lim[2])
  hue_shift <- runif(1, 
                     hue_shift_lim[1], 
                     hue_shift_lim[2])
  
  img <- image_modulate(img, 
                        brightness = brightness_shift, 
                        saturation =  saturation_shift, 
                        hue = hue_shift)
  img
}
####################################################################
randomHorizontalFlipDEf <- function(img, mask,dfrmn=F) {
  w=sample(1:4)[1]
  if (w == 1) {a=(list(img = img, mask = mask))}
  if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask))) }
  if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask)))}
  if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask))))}
  defIndex= sample(seq(from=-0.4,to=0.4,by=0.1))[1]
  if(dfrmn==F){
  a= list(img=image_implode(a$img, factor = defIndex),mask= image_implode(a$mask, factor = defIndex))}
  return(a)
  
}
#######################################################
img2arr <- function(image, 
                    target_width = 256,
                    target_height = 256) {
  result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
  dim(result) <- c(1, target_width, target_height, 3)
  return(result)
}
#########################################################
mask2arr <- function(mask,
                     target_width = 256,
                     target_height = 256) {
  result <- t(as.numeric(mask[[1]])[, , 1]) # transpose
  dim(result) <- c(1, target_width, target_height, 1)
  return(result)
}
######################################
val_generator <- function(images_dir, 
                          samples_index,
                          masks_dir, 
                          batch_size) {
  images_iter <- list.files(images_dir, 
                        #    pattern = ".jpg", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                        #   pattern = ".jpg",
                           full.names = TRUE)[samples_index]  # for next epoch
  masks_iter <- list.files(masks_dir, 
                        #   pattern = ".png",
                           full.names = TRUE)[samples_index] # for current epoch
  masks_all <- list.files(masks_dir, 
                        #  pattern = ".png",
                          full.names = TRUE)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch
    if (length(images_iter) < batch_size) {
      images_iter <<- images_all
      masks_iter <<- masks_all
    }
    
    batch_ind <- sample(1:length(images_iter), batch_size)
    
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
    batch_masks_list <- masks_iter[batch_ind]
    masks_iter <<- masks_iter[-batch_ind]
    
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i],
                             mask_file = batch_masks_list[i])
      # without augmentation
      
      # return as arrays
      x_y_arr <- list(x = img2arr(x_y_imgs$img),
                      y = mask2arr(x_y_imgs$mask))
    }
    
    x_y_batch <- purrr::transpose(x_y_batch)
    
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
    
    result <- list(keras_array(x_batch), 
                   keras_array(y_batch))
    return(result)
  }
}
###########################################
train_generator <- function(images_dir, 
                            samples_index,
                            masks_dir, 
                            batch_size,
							dfrmn) {
  images_iter <- list.files(images_dir, 
                        #    pattern = ".jpg", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                        #   pattern = ".jpg",
                           full.names = TRUE)[samples_index]  # for next epoch
  masks_iter <- list.files(masks_dir, 
                        #   pattern = ".png",
                           full.names = TRUE)[samples_index] # for current epoch
  masks_all <- list.files(masks_dir, 
                        #  pattern = ".png",
                          full.names = TRUE)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch
    if (length(images_iter) < batch_size) {
      images_iter <<- images_all
      masks_iter <<- masks_all
    }
    
    batch_ind <- sample(1:length(images_iter), batch_size)
    
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
    batch_masks_list <- masks_iter[batch_ind]
    masks_iter <<- masks_iter[-batch_ind]
    
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i],
                             mask_file = batch_masks_list[i])
      # augmentation
      x_y_imgs$img <- randomBSH(x_y_imgs$img)
      x_y_imgs <- randomHorizontalFlipDEf(x_y_imgs$img,  #
                                       x_y_imgs$mask,
									   dfrmn=F)
      # return as arrays
      x_y_arr <- list(x = img2arr(x_y_imgs$img),
                      y = mask2arr(x_y_imgs$mask))
    }
    
    x_y_batch <- purrr::transpose(x_y_batch)
    
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
    
    result <- list(keras_array(x_batch), 
                   keras_array(y_batch))
    return(result)
  }
}
############
cl <- makePSOCKcluster(detectCores (logical = FALSE))
clusterEvalQ(cl, {
  library(magick)     
  library(abind)     
  library(reticulate)
  
  imagesRead <- function(image_file,
                         mask_file,
                         target_width = 256, 
                         target_height = 256) {
    img <- image_read(image_file)
    img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
    
    mask <- image_read(mask_file)
    mask <- image_scale(mask, paste0(target_width, "x", target_height, "!"))
    return(list(img = img, mask = mask))
  }
  
  randomBSH <- function(img,
                        u = 0,
                        brightness_shift_lim = c(90, 115), # percentage
                        saturation_shift_lim = c(90, 115), # of current value
                        hue_shift_lim = c(100, 100)) {
    
    if (rnorm(1) < u) return(img)     #
    
    brightness_shift <- runif(1, 
                              brightness_shift_lim[1], 
                              brightness_shift_lim[2])
    saturation_shift <- runif(1, 
                              saturation_shift_lim[1], 
                              saturation_shift_lim[2])
    hue_shift <- runif(1, 
                       hue_shift_lim[1], 
                       hue_shift_lim[2])
    
    img <- image_modulate(img, 
                          brightness = brightness_shift, 
                          saturation =  saturation_shift, 
                          hue = hue_shift)
    img
  }
  ############################################################
randomHorizontalFlipDEf <- function(img, mask,dfrmn=F) {
  w=sample(1:4)[1]
  if (w == 1) {a=(list(img = img, mask = mask))}
  if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask))) }
  if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask)))}
  if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask))))}
  defIndex= sample(seq(from=-0.4,to=0.4,by=0.1))[1]
   if(dfrmn==F){
 a= list(img=image_implode(a$img, factor = defIndex),mask= image_implode(a$mask, factor = defIndex))}
  return(a)
  
}
  #########################################################################
  
  img2arr <- function(image, 
                      target_width = 256,
                      target_height = 256) {
    result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
    dim(result) <- c(1, target_width, target_height, 3)
    return(result)
  }
  
  mask2arr <- function(mask,
                       target_width = 256,
                       target_height = 256) {
    result <- t(as.numeric(mask[[1]])[, , 1]) # transpose
    dim(result) <- c(1, target_width, target_height, 1)
    return(result)
  }
})

registerDoParallel(cl)
##########################################################
train_iterator <- py_iterator(train_generator(images_dir = images_dir,
                                              masks_dir = masks_dir,
                                              samples_index = train_index,
                                              batch_size = batch_size,
											  dfrmn=dfrmn))

val_iterator <- py_iterator(val_generator(images_dir = images_dir,
                                          masks_dir = masks_dir,
                                          samples_index = val_index,
                                          batch_size = batch_size))

#a=iter_next(train_iterator)
#iter_next(val_iterator)			
#######################################################################
 #         keras:::fit.keras.engine.training.Model(unet1,
 #                                                train_iterator,
 #                                             
 #                                                epochs = epochs)
############################################################################
unet1 %>% fit_generator(
  train_iterator,
  steps_per_epoch =  steps_per_epoch,
  epochs = epochs, 
  validation_data =  val_iterator,
  validation_steps = steps_per_epoch,
  verbose = 1,
  callbacks = list(early_stopping,cp_callback)
)
a1=get_weights(unet1)
filepathRDS=paste0(checkpoint_dir,"\\",Species,"_",dateTrain)
saveRDS(a1,filepathRDS)
###########################################################################
stopCluster(cl)
#}

#if (tensorflow::tf_version() <= "2.0") {UnetTrain256()}
#if (tensorflow::tf_version() > "2.0") {source("Modules/unet_train_model_3.r")}
