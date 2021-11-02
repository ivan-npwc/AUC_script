
library(magick)
library(abind)
library(reticulate)
library(parallel)
library(doParallel)
library(foreach)
library(keras)

             trainDir= "C:\\SSL_DB\\TRAIN\\LRG_Measurements"
             epochs=200
			 batch_size=32
			 BaseModel_pth= ""
             TrainIndex=1  # every img 4 times for 1 epoch
             NewModelCreate=T
             trgt_size=256
        
		
             dateTrain=format(Sys.time(),  "%b%d %Y") 
		     checkpoint_dir=paste0(trainDir,"\\Checkpoints");if(dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}
			 images_dir =  paste0(trainDir,"\\Image")
			 
##########################################################################
if (NewModelCreate==T) {
		  conv_base <- application_vgg16(
		   weights = "imagenet",
		   include_top = FALSE,
		   input_shape = c(trgt_size, trgt_size, 3))
		   
		   model_regresion <<- keras_model_sequential() %>%
											   conv_base %>%
							  layer_dropout(rate = 0.5) %>% 
										  layer_flatten() %>%  
	layer_dense(units = 64, activation = "relu",name = "fc4") %>%   
    layer_batch_normalization() %>%    	
	layer_dense(units = 1,name = "predictions")
	 
	freeze_weights(conv_base)
	

	model_regresion %>% compile(optimizer = "adam",
                               loss = "mse",
                               metrics = c("mae"))					
						
						model_regresion					
	} else {model_regresion=model_regresion(BaseModel_pth)}	
#########################################################################
       train_samples <- length(list.files(images_dir))
      # val_train_samples=  length(list.files(val_images_dir1,full.names=T, recursive=T, include.dirs=F))
       steps_per_epoch= round(train_samples/batch_size*1)
       train_index <- sample(1:train_samples, round(train_samples * TrainIndex)) # we can rondomly select some parts, using not 100% data
    #   val_index <- sample(1:val_train_samples, round(val_train_samples * DataUsingIndex)) 
#####################################################################
imagesRead <- function(image_file) {
  img <- image_read(image_file);img <- image_scale(img, "256x256!")
  label=as.numeric(strsplit(basename(image_file),"_")[[1]][1]) # I shud to rename images with the first two letters corresponding to category
  list(img = img,label=label)
}
#################################################################
randomBSH <- function(img,label,
                      u = 0,
                      brightness_shift_lim = c(90, 115), # percentage
                      saturation_shift_lim = c(90, 115), # of current value
                      hue_shift_lim = c(100, 100)) {
  if (rnorm(1) < u) return( list(img = img, label=label))
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
  										
  list(img = img,label=label)
}
####################################################################
randomHorizontalFlip <- function(img,label) {
  w=sample(1:4)[1]
  if (w == 1) {a=list(img = img,label=label)}
  if (w == 2) {a=list(img = image_flop(img),label=label) }
  if (w == 3) {a=list(img = image_flip(img),label=label)}
  if (w == 4) {a=list(img = image_flop(image_flip(img)),label=label)}
  return(a) 
}
#######################################################
img2arr <- function(image, 
                    target_width,
                    target_height) {
  result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
  dim(result) <- c(1, target_width, target_height, 3)
  return(result)
}
#############################################################
val_generator <- function(val_images_dir, 
                          samples_index,
                          batch_size) {
  images_iter <- list.files(val_images_dir,  
                            full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all1 <- list.files(val_images_dir, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index]  # for next epoch

  function() {
    # start new epoch
    if (length(images_iter) < batch_size) {
      images_iter <<- images_all
   
    }
    batch_ind <- sample(1:length(images_iter), batch_size)
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
    
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i])
      # without augmentation	  
      # return as arrays
      x_y_arr <- list(x = img2arr(x_y_imgs$img,256,256), y= x_y_imgs$label)
    }
    x_y_batch <- purrr::transpose(x_y_batch)
       x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
       y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
	  
    result <-      list(keras_array(x_batch), 
                        keras_array(y_batch))
				        
    return(result)
  }
}
###########################################
train_generator <- function(images_dir, 
                            samples_index, 
                            batch_size) {
  images_iter <- list.files(images_dir,  
                            full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all <- list.files(images_dir, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index]  # for next epoch
  
  function() { 
    # start new epoch  images_iter
    if (length(images_iter) <  batch_size) {
      images_iter <<- images_all

    }
    batch_ind <- sample(1:length(images_iter), batch_size)
    batch_images_list <- images_iter[batch_ind]
    images_iter <<- images_iter[-batch_ind]
   
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file = batch_images_list[i])
      # augmentation
      x_y_imgs <- randomBSH(x_y_imgs$img,x_y_imgs$label)
      x_y_imgs <- randomHorizontalFlip(x_y_imgs$img,x_y_imgs$label)
      # return as arrays
      x_y_arr <- list(x = img2arr(x_y_imgs$img,256,256), 
					  y=x_y_imgs$label)
    }
    x_y_batch <- purrr::transpose(x_y_batch)
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))

    result <-      list(keras_array(x_batch), 
                        keras_array(y_batch))
				    
    return(result)
  }
}
############
cl <- makePSOCKcluster(detectCores(logical=F))
clusterEvalQ(cl, {
  library(magick)     
  library(abind)     
  library(reticulate)
  #####################################################################
imagesRead <- function(image_file) {
  img <- image_read(image_file);img <- image_scale(img, "256x256!")
  label=as.numeric(strsplit(basename(image_file),"_")[[1]][1]) # I shud to rename images with the first two letters corresponding to category
  list(img = img,label=label)
}
#################################################################
randomBSH <- function(img,label,
                      u = 0,
                      brightness_shift_lim = c(90, 115), # percentage
                      saturation_shift_lim = c(90, 115), # of current value
                      hue_shift_lim = c(100, 100)) {
  if (rnorm(1) < u) return( list(img = img, label=label))
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
  										
  list(img = img,label=label)
}
####################################################################
randomHorizontalFlip <- function(img,label) {
  w=sample(1:4)[1]
  if (w == 1) {a=list(img = img,label=label)}
  if (w == 2) {a=list(img = image_flop(img),label=label) }
  if (w == 3) {a=list(img = image_flip(img),label=label)}
  if (w == 4) {a=list(img = image_flop(image_flip(img)),label=label)}
  return(a) 
}
#######################################################
img2arr <- function(image, 
                    target_width,
                    target_height) {
  result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
  dim(result) <- c(1, target_width, target_height, 3)
  return(result)
}

#############################################################
})
registerDoParallel(cl)
##########################################################
train_iterator <- py_iterator(train_generator(images_dir = images_dir,  
                                              samples_index = train_index,
                                              batch_size = batch_size))

#val_iterator <- py_iterator(val_generator(val_images_dir1 = val_images_dir1,
#                                          val_images_dir2 = val_images_dir2,
#                                          samples_index = val_index,
#                                          batch_size = batch_size))
iter_next(train_iterator)
#iter_next(val_iterator)			
#############################################################################################################
#early_stopping <- callback_early_stopping(patience = 4)
#filepath <- file.path(checkpoint_dir, "Val_{val_acc:.2f}_epoch_{epoch:02d}_AgeSSL.h5")
#cp_callback <- callback_model_checkpoint( 
#  filepath = filepath,
#  period = 1,
#  verbose = 1)
############################################################################
model_regresion %>% fit_generator (
  train_iterator,
  steps_per_epoch =  steps_per_epoch,
  epochs =  epochs)
  
  				
#  validation_data = val_iterator,
#  validation_steps = steps_per_epoch,
#  verbose = 1,
#  callbacks = list(early_stopping,cp_callback)
#)
####