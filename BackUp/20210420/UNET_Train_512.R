
trainDir=  train256_NFS_Dir
ValIndex=0.3
if (file.exists(Model_base)==F) { Model_base=   "D:\\CIC\\SHINY_NFS_AUTOCOUNT\\20190505\\data\\TRAIN\\Val_0.43_epoch_16_NFS_256_Points.h5"}      
checkpoint_dir=paste0(trainDir,"\\Checkpoints")
if (dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}

images_dir= paste0(trainDir, "\\Image")
masks_dir= paste0(trainDir, "\\Mask")



 listImgS<<- list.files(images_dir)
	 listImgS=gsub("jpg","png",listImgS)
     ListMskS<<- list.files(masks_dir)
     deleteList<<- listImgS[!(listImgS %in% ListMskS)]
	 deleteList=gsub("png","jpg",deleteList)
     RemoveList <<- paste0(images_dir,"\\",deleteList)
     file.remove(RemoveList)
	 ###
	 deleteList1<<- ListMskS[!(ListMskS %in% listImgS)]
     RemoveList1 <<- paste0(masks_dir,"\\",deleteList1)
     file.remove(RemoveList1)

images_dir_tmp=paste0(trainDir, "\\Image_tmp")
mask_dir_tmp=paste0(trainDir, "\\Mask_tmp")

validate_dir=paste0(trainDir, "\\Validate")
if(NewTrain==T) {
  unlink(validate_dir, recursive=T)
  unlink(images_dir_tmp, recursive=T)
  unlink(mask_dir_tmp, recursive=T)
}
###############################################################
val_img_dir_tmp=paste0(trainDir, "\\Validate\\Image")
val_msk_dir_tmp=paste0(trainDir, "\\Validate\\Mask")
dir.create(validate_dir)
dir.create(val_img_dir_tmp)
dir.create(val_msk_dir_tmp)
dir.create(images_dir_tmp)
dir.create(mask_dir_tmp)

ImgList=list.files(images_dir)
MskList=list.files(masks_dir)
index=sample(1:length(ImgList))[1:round(ValIndex * length(ImgList))]
ValImgList=ImgList[index]
ValMskList=MskList[index]
TrainImgList=ImgList[!(ImgList %in% ValImgList)]
ValImgListFrom=paste0(images_dir,"\\",ValImgList)
TrainImgListFrom=paste0(images_dir,"\\",TrainImgList)
ValMskListFrom=paste0(masks_dir,"\\",ValMskList)
TrainMskList=MskList[!(MskList %in% ValMskList)]
TrainMskListFrom=paste0(masks_dir,"\\",TrainMskList)

#######################################################
if(NewTrain==T) {
  file.copy(ValImgListFrom,val_img_dir_tmp)
  file.copy(ValMskListFrom,val_msk_dir_tmp)
  file.copy(TrainImgListFrom,images_dir_tmp)
  file.copy(TrainMskListFrom,mask_dir_tmp)
}
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
input_size <- 512  
epochs <- 3
batch_size <- 2 
orig_width <- 1024  #
orig_height <- 1024
train_samples <- length(list.files(images_dir))
val_train_samples=  length(list.files(val_images_dir))
steps_per_epoch=round(train_samples/batch_size*2)

train_index <- sample(1:train_samples, round(train_samples * 1)) 
val_index <- sample(1:val_train_samples, round(val_train_samples * 1)) 
#####################################################################
imagesRead <- function(image_file,
                       mask_file,
                       target_width = 512, 
                       target_height = 512) {
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
randomHorizontalFlip <- function(img, mask) {
  w=sample(1:4)[1]
  if (w == 1) {a=(list(img = img, mask = mask))}
  if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask))) }
  if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask)))}
  if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask))))}
  return(a)
  
}
#######################################################
img2arr <- function(image, 
                    target_width = 512,
                    target_height = 512) {
  result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
  dim(result) <- c(1, target_width, target_height, 3)
  return(result)
}
#########################################################
mask2arr <- function(mask,
                     target_width = 512,
                     target_height = 512) {
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
                         #   pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                         #  pattern = ".png",
                           full.names = TRUE)[samples_index]  # for next epoch
  masks_iter <- list.files(masks_dir, 
                          # pattern = ".png",
                           full.names = TRUE)[samples_index] # for current epoch
  masks_all <- list.files(masks_dir, 
                         # pattern = ".png",
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
                            batch_size) {
  images_iter <- list.files(images_dir, 
                          #  pattern = ".png", 
                            full.names = TRUE)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                          # pattern = ".png",
                           full.names = TRUE)[samples_index]  # for next epoch
  masks_iter <- list.files(masks_dir, 
                         #  pattern = ".png",
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
      x_y_imgs <- randomHorizontalFlip(x_y_imgs$img,  #
                                       x_y_imgs$mask)
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
cl <- makePSOCKcluster(4)
clusterEvalQ(cl, {
  library(magick)     
  library(abind)     
  library(reticulate)
  
  imagesRead <- function(image_file,
                         mask_file,
                         target_width = 512, 
                         target_height = 512) {
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
                      target_width = 512,
                      target_height = 512) {
    result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
    dim(result) <- c(1, target_width, target_height, 3)
    return(result)
  }
  
  mask2arr <- function(mask,
                       target_width = 512,
                       target_height = 512) {
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
                                              batch_size = batch_size))

val_iterator <- py_iterator(val_generator(images_dir = val_images_dir,
                                          masks_dir = val_masks_dir,
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
														
model
if (file.exists(Weight)==T) {
  weightRDS=readRDS(Weight)
  set_weights(model,	weightRDS)}	

#############################################################################################################
early_stopping <- callback_early_stopping(patience = 4)
filepath <- file.path(checkpoint_dir, "Val_{val_dice_coef:.2f}_epoch_{epoch:02d}_512.h5")
cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 1,
  #save_weights_only = TRUE,
  verbose = 1)


##########
############################################################################
model %>% fit_generator(
  train_iterator,
  steps_per_epoch =steps_per_epoch,
  epochs = epochs, 
  validation_data = val_iterator,
  validation_steps = 500,
  verbose = 1,
  callbacks = list(early_stopping,cp_callback)
)
###########################################################################
stopCluster(cl)

