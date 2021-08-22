train256_NFS_Dir = "D:\\CIC\\SHINY_NFS_AUTOCOUNT\\20190505\\data\\TRAIN"

ValIndex=0.2

trainDir=  train256_NFS_Dir
Model_base=    "C:\\Users\\Admin\\Documents\\Unet_4_512_3_16_layer_separable_conv_2d.h5"  # paste0(trainDir,"\\Model_base_NFS_Points5")
checkpoint_dir=paste0(trainDir,"\\Checkpoints")

images_dir= paste0(trainDir, "\\Image")
masks_dir= paste0(trainDir, "\\Mask")
points_dir=paste0(trainDir, "\\Points_predict")

     listImgS<<- list.files(images_dir)
     ListMskS<<- list.files(masks_dir)
	 
     deleteList<<- listImgS[!(listImgS %in% ListMskS)]
     RemoveList <<- paste0(images_dir,"\\",deleteList)
     file.remove(RemoveList)
	 ###
	 deleteList1<<- ListMskS[!(ListMskS %in% listImgS)]
     RemoveList1 <<- paste0(masks_dir,"\\",deleteList1)
     file.remove(RemoveList1)

images_dir_tmp=paste0(trainDir, "\\Image_tmp")
mask_dir_tmp=paste0(trainDir, "\\Mask_tmp")

validate_dir=paste0(trainDir, "\\Validate")
#unlink(validate_dir, recursive=T)
#unlink(images_dir_tmp, recursive=T)
#unlink(mask_dir_tmp, recursive=T)

val_img_dir_tmp=paste0(trainDir, "\\Validate\\Image")
val_msk_dir_tmp=paste0(trainDir, "\\Validate\\Mask")
dir.create(validate_dir)
dir.create(val_img_dir_tmp)
dir.create(val_msk_dir_tmp)
dir.create(images_dir_tmp)
dir.create(mask_dir_tmp)

ImgList=list.files(images_dir)
index=sample(1:length(ImgList))[1:round(ValIndex * length(ImgList))]
ValImgList=ImgList[index]
TrainImgList=ImgList[!(ImgList %in% ValImgList)]


ValImgListFrom=paste0(images_dir,"\\",ValImgList)
#file.copy(ValImgListFrom,val_img_dir_tmp)
ValMskListFrom=paste0(masks_dir,"\\",ValImgList)
#file.copy(ValMskListFrom,val_msk_dir_tmp)
TrainImgListFrom=paste0(images_dir,"\\",TrainImgList)
#file.copy(TrainImgListFrom,images_dir_tmp)
TrainMskListFrom=paste0(masks_dir,"\\",TrainImgList)
#file.copy(TrainMskListFrom,mask_dir_tmp)
#######################################################
val_images_dir=  val_img_dir_tmp
val_masks_dir=   val_msk_dir_tmp
images_dir=images_dir_tmp
masks_dir= mask_dir_tmp 

##########################################################################
library(magick)
library(abind)
library(reticulate)
library(parallel)
library(doParallel)
library(foreach)
#################################################################################
library(tensorflow)
#install_tensorflow(gpu = TRUE)
use_condaenv("r-tensorflow")
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
#############
library(keras)
#############################################################################
input_size <- 256  
epochs <- 50
steps_per_epoch=300
batch_size <- 15 
orig_width <- 1024  #
orig_height <- 1024
train_samples <- length(list.files(images_dir))
val_train_samples=  length(list.files(val_images_dir))

train_index <- sample(1:train_samples, round(train_samples * 1)) 
val_index <- sample(1:val_train_samples, round(val_train_samples * 1))
 
#####################################################################
	imagesRead <- function(image_file,
						   mask_file,
						   points_file,
						   target_width = 256, 
						   target_height = 256) {
	  img <- image_read(image_file)
	  img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
	  
	  points_pred <- image_read(points_file)
	  points_pred <- image_scale(points_pred, paste0(target_width, "x", target_height, "!"))
	  
	  mask <- image_read(mask_file)
	  mask <- image_scale(mask, paste0(target_width, "x", target_height, "!"))
	  list(img = img, mask = mask,points_pred=points_pred)
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
randomHorizontalFlip <- function(img, mask,points_pred) {
  w=sample(1:4)[1]
  if (w == 1) {a=(list(img = img, mask = mask,points_pred=points_pred))}
  if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask),points_pred = image_flop(points_pred))) }
  if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask),points_pred = image_flip(points_pred)))}
  if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask)),points_pred = image_flop(image_flip(points_pred))))}
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
                          points_dir,
                          samples_index,
                          masks_dir, 
                          batch_size) {
  images_iter <- list.files(images_dir, 
                            pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index]  # for next epoch
  
      points_iter <- list.files(points_dir, 
                            pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
      points_all <- list.files(points_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index] 
  masks_iter <- list.files(masks_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index] # for current epoch
  masks_all <- list.files(masks_dir, 
                          pattern = ".png",
                          full.names = TRUE)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch
    if (length(images_iter) < batch_size) {
      images_iter <<- images_all
      points_iter <<- points_all
	  masks_iter <<- masks_all
    }
    
    batch_ind <- sample(1:length(images_iter), batch_size)
    
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
	
	 batch_points_list <- points_iter[batch_ind]
    points_iter <<- points_iter[-batch_ind]
	
    batch_masks_list <- masks_iter[batch_ind]
    masks_iter <<- masks_iter[-batch_ind]
    ######
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i],
                             mask_file = batch_masks_list[i],
							 points_file=batch_points_list[i])
     
                             
      
      x_y_arr <- list(x = abind(img2arr(x_y_imgs$img),mask2arr(x_y_imgs$points_pred)),
                      y = mask2arr(x_y_imgs$mask))
    }
    ########################
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
                            points_dir,
                            samples_index,
                            masks_dir, 
                            batch_size) {
  images_iter <- list.files(images_dir, 
                            pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index]  # for next epoch
						   
	  points_iter <- list.files(points_dir, 
                            pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
      points_all <- list.files(points_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index] 					   						   										   
  masks_iter <- list.files(masks_dir, 
                           pattern = ".png",
                           full.names = TRUE)[samples_index] # for current epoch
  masks_all <- list.files(masks_dir, 
                          pattern = ".png",
                          full.names = TRUE)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch
    if (length(images_iter) < batch_size) {
      images_iter <<- images_all
      points_iter <<- points_all
	  masks_iter <<- masks_all
    }
    
    batch_ind <- sample(1:length(images_iter), batch_size)
    
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
	
	 batch_points_list <- points_iter[batch_ind]
    points_iter <<- points_iter[-batch_ind]
	
    batch_masks_list <- masks_iter[batch_ind]
    masks_iter <<- masks_iter[-batch_ind]
    ######
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i],
                             mask_file = batch_masks_list[i],
							 points_file=batch_points_list[i])
     
                                                          #  x_y_imgs$img <- randomBSH(x_y_imgs$img)
                                                          #   x_y_imgs <- randomHorizontalFlip(x_y_imgs$img,  
                                                             #                                   x_y_imgs$mask)
      
      x_y_arr <- list(x = abind(img2arr(x_y_imgs$img),mask2arr(x_y_imgs$points_pred)),
                      y = mask2arr(x_y_imgs$mask))
    }
    ########################
    x_y_batch <- purrr::transpose(x_y_batch)
    
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
    
    result <- list(keras_array(x_batch), 
                   keras_array(y_batch))
    return(result)
  }
}
############
cl <- makePSOCKcluster(4)
clusterEvalQ(cl, {
  library(magick)     
  library(abind)     
  library(reticulate)
 ######################################################## 
imagesRead <- function(image_file,
                       mask_file,
					   points_file,
                       target_width = 256, 
                       target_height = 256) {
  img <- image_read(image_file)
  img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
  
  points_pred <- image_read(points_file)
  points_pred <- image_scale(points_pred, paste0(target_width, "x", target_height, "!"))
  
  mask <- image_read(mask_file)
  mask <- image_scale(mask, paste0(target_width, "x", target_height, "!"))
  list(img = img, mask = mask,points_pred=points_pred)
}
##################################################### 
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
  randomHorizontalFlip <- function(img, mask) {
    w=sample(1:4)[1]
    if (w == 1) {a=(list(img = img, mask = mask))}
    if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask))) }
    if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask)))}
    if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask))))}
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
											  points_dir=points_dir,
                                              samples_index = train_index,
                                              batch_size = batch_size))

val_iterator <- py_iterator(val_generator(images_dir = val_images_dir,
                                          masks_dir = val_masks_dir,
										  points_dir=points_dir,
                                          samples_index = val_index,
                                          batch_size = batch_size))

iter_next(train_iterator)
iter_next(val_iterator)			
#######################################################################

K <- backend()

dice_coef <- function(y_true, y_pred, smooth = 1) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
}
attr(dice_coef, "py_function_name") <- "dice_coef"

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"

##########################################
bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) +
    (1 - dice_coef(y_true, y_pred))
  return(result)
}
#####################################################################################
model <- load_model_hdf5(Model_base, custom_objects = c(dice_coef = dice_coef,
                                                        dice_coef_loss=dice_coef_loss))
#model
#############################################################################################################
early_stopping <- callback_early_stopping(patience = 4)
filepath <- file.path(checkpoint_dir, "Val_{val_dice_coef:.2f}_epoch_{epoch:02d}_NFS_256.h5")
cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 3,
  #save_weights_only = TRUE,
  verbose = 1)


##########
############################################################################
model %>% fit_generator(
  train_iterator,
  steps_per_epoch =steps_per_epoch,
  epochs = epochs, 
  validation_data = val_iterator,
  validation_steps = steps_per_epoch,
  verbose = 1,
  callbacks = list(early_stopping,cp_callback)
)
###########################################################################
stopCluster(cl)

