image_file1=  "F:\\Google Drive (ivan@npwc.us)\\AUC\\20191222\\data\\TRAIN\\Sex\\1\\An\\An_An1_20190610_d013_f11323.png"
image_file2=  "F:\\Google Drive (ivan@npwc.us)\\AUC\\20191222\\data\\TRAIN\\Sex\\1\\An\\An_An1_20190610_d013_f11323.png"
trainDir="F:\\Google Drive (ivan@npwc.us)\\AUC\\20191222\\data\\TRAIN\\Sex\\Train"

images_dir1= paste0(trainDir, "\\Big")
images_dir2= paste0(trainDir, "\\Small")
checkpoint_dir=paste0(trainDir,"\\Checkpoints"); if (dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}


val_images_dir1=  images_dir1
val_images_dir2=   images_dir2


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
epochs <- 10
batch_size <- 16 

train_samples <- length(list.files(images_dir1,full.names=T, recursive=T, include.dirs=F))
val_train_samples=  length(list.files(val_images_dir1,full.names=T, recursive=T, include.dirs=F))
steps_per_epoch= round(train_samples/batch_size*2)

train_index <- sample(1:train_samples, round(train_samples * 1)) # we can rondomly select some parts, using not 100% data
val_index <- sample(1:val_train_samples, round(val_train_samples * 1)) 
#####################################################################
imagesRead <- function(image_file1,
                       image_file2) {
  img1 <- image_read(image_file1);img1 <- image_scale(img1, "256x256!")
  img2 <- image_read(image_file2);img2 <- image_scale(img2, "150x150!")
  label=substr(basename(image_file1),1,2) # I shud to rename images with the first two letters corresponding to category
  list(img1 = img1, img2 = img2,label=label)
}
#################################################################
randomBSH <- function(img1,img2,label,
                      u = 0,
                      brightness_shift_lim = c(90, 115), # percentage
                      saturation_shift_lim = c(90, 115), # of current value
                      hue_shift_lim = c(100, 100)) {
  
  if (rnorm(1) < u) return( list(img1 = img1, img2 = img2,label=label))
  
  brightness_shift <- runif(1, 
                            brightness_shift_lim[1], 
                            brightness_shift_lim[2])
  saturation_shift <- runif(1, 
                            saturation_shift_lim[1], 
                            saturation_shift_lim[2])
  hue_shift <- runif(1, 
                     hue_shift_lim[1], 
                     hue_shift_lim[2])
  
  img1 <- image_modulate(img1, 
                        brightness = brightness_shift, 
                        saturation =  saturation_shift, 
                        hue = hue_shift)
  img2 <- image_modulate(img2, 
                        brightness = brightness_shift, 
                        saturation =  saturation_shift, 
                        hue = hue_shift)					
						
						
  list(img1 = img1, img2 = img2,label=label)
}
####################################################################
randomHorizontalFlip <- function(img1,img2,label) {
  w=sample(1:4)[1]
  if (w == 1) {a=list(img1 = img1,img2 = img2,label=label)}
  if (w == 2) {a=list(img1 = image_flop(img1),img2 = image_flop(img2),label=label) }
  if (w == 3) {a=list(img1 = image_flip(img1),img2 = image_flip(img2),label=label)}
  if (w == 4) {a=list(img1 = image_flop(image_flip(img1)),img2 = image_flop(image_flip(img2)),label=label)}
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
#########################################################
 label_to_matrix <- function(label) {
      if (label== "An"){labelMatrix=matrix(c(1,0,0,0),1,4)}
	   if (label== "F_"){labelMatrix=matrix(c(0,1,0,0),1,4)}
	    if (label== "J_"){labelMatrix=matrix(c(0,0,1,0),1,4)}
		 if (label== "Sa"){labelMatrix=matrix(c(0,0,0,1),1,4)}
labelMatrix		 
}
#############################################################
val_generator <- function(val_images_dir1,val_images_dir2, 
                          samples_index,
                          batch_size) {
  images_iter1 <- list.files(val_images_dir1,  
                            full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all1 <- list.files(val_images_dir1, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index]  # for next epoch
						   ##
  images_iter2 <- list.files(val_images_dir2, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all2 <- list.files(val_images_dir2, 
                          full.names = T, recursive=T, include.dirs=F)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch
    if (length(images_iter1) < batch_size) {
      images_iter1 <<- images_all1
      images_iter2 <<- images_all2
    }
    
    batch_ind <- sample(1:length(images_iter1), batch_size)
    
    batch_images_list1 <- images_iter1[batch_ind]
    images_iter1 <<- images_iter1[-batch_ind]
	
    batch_images_list2 <- images_iter2[batch_ind]
    images_iter2 <<- images_iter2[-batch_ind]
    
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file1 = batch_images_list1[i],
                             image_file2 = batch_images_list2[i])
      # without augmentation	  
      # return as arrays
      x_y_z_arr <- list(x = img2arr(x_y_imgs$img1,256,256),
                      y = img2arr(x_y_imgs$img2,150,150),
					  z=label_to_matrix(x_y_imgs$label))
    }
    
    x_y_batch <- purrr::transpose(x_y_batch)
    
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
	
	z_batch <- do.call(abind, c(x_y_batch$z, list(along = 1)))
    
    result <- list(list(keras_array(x_batch), 
                        keras_array(y_batch)),
				        keras_array(z_batch))
    return(result)
  }
}
###########################################
train_generator <- function(images_dir1,images_dir2, 
                            samples_index, 
                            batch_size) {
  images_iter1 <- list.files(images_dir1,  
                            full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all1 <- list.files(images_dir1, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index]  # for next epoch
  images_iter2 <- list.files(images_dir2, 
                           full.names = T, recursive=T, include.dirs=F)[samples_index] # for current epoch
  images_all2 <- list.files(images_dir2, 
                          full.names = T, recursive=T, include.dirs=F)[samples_index] # for next epoch
  
  function() {
    
    # start new epoch  images_iter
    if (length(images_iter1) <  batch_size) {
      images_iter1 <<- images_all1
      images_iter2 <<- images_all2
    }
    
    batch_ind <- sample(1:length(images_iter1), batch_size)
    
    batch_images_list1 <- images_iter1[batch_ind]
    images_iter1 <<- images_iter1[-batch_ind]
	
    batch_images_list2 <- images_iter2[batch_ind]
    images_iter2 <<- images_iter2[-batch_ind]
    
    x_y_batch <- foreach(i = 1:batch_size) %dopar% {
      x_y_imgs <- imagesRead(image_file1 = batch_images_list1[i],
                             image_file2 = batch_images_list2[i])
      # augmentation
      x_y_imgs <- randomBSH(x_y_imgs$img1,x_y_imgs$img2,x_y_imgs$label)
      x_y_imgs <- randomHorizontalFlip(x_y_imgs$img1,x_y_imgs$img2,x_y_imgs$label)
      # return as arrays
      x_y_z_arr <- list(x = img2arr(x_y_imgs$img1,256,256),
                        y = img2arr(x_y_imgs$img2,150,150),
					   z=label_to_matrix(x_y_imgs$label))
    }
	

    
    x_y_batch <- purrr::transpose(x_y_batch)
    
    x_batch <- do.call(abind, c(x_y_batch$x, list(along = 1)))
    
    y_batch <- do.call(abind, c(x_y_batch$y, list(along = 1)))
    
    z_batch <- do.call(abind, c(x_y_batch$z, list(along = 1)))
    
    result <- list(list(keras_array(x_batch), 
                        keras_array(y_batch)),
				        keras_array(z_batch))



    return(result)
  }
}
############
cl <- makePSOCKcluster(4)
clusterEvalQ(cl, {
  library(magick)     
  library(abind)     
  library(reticulate)
  #####################################################################
imagesRead <- function(image_file1,
                       image_file2) {
  img1 <- image_read(image_file1);img1 <- image_scale(img1, "256x256!")
  img2 <- image_read(image_file2);img2 <- image_scale(img2, "150x150!")
  label=substr(basename(image_file1),1,2) # I shud to rename images with the first two letters corresponding to category
  list(img1 = img1, img2 = img2,label=label)
}
#################################################################
randomBSH <- function(img1,img2,label,
                      u = 0,
                      brightness_shift_lim = c(90, 115), # percentage
                      saturation_shift_lim = c(90, 115), # of current value
                      hue_shift_lim = c(100, 100)) {
  
  if (rnorm(1) < u) return( list(img1 = img1, img2 = img2,label=label))
  
  brightness_shift <- runif(1, 
                            brightness_shift_lim[1], 
                            brightness_shift_lim[2])
  saturation_shift <- runif(1, 
                            saturation_shift_lim[1], 
                            saturation_shift_lim[2])
  hue_shift <- runif(1, 
                     hue_shift_lim[1], 
                     hue_shift_lim[2])
  
  img1 <- image_modulate(img1, 
                        brightness = brightness_shift, 
                        saturation =  saturation_shift, 
                        hue = hue_shift)
  img2 <- image_modulate(img2, 
                        brightness = brightness_shift, 
                        saturation =  saturation_shift, 
                        hue = hue_shift)					
						
						
  list(img1 = img1, img2 = img2,label=label)
}
####################################################################
randomHorizontalFlip <- function(img1,img2,label) {
  w=sample(1:4)[1]
  if (w == 1) {a=list(img1 = img1,img2 = img2,label=label)}
  if (w == 2) {a=list(img1 = image_flop(img1),img2 = image_flop(img2),label=label) }
  if (w == 3) {a=list(img1 = image_flip(img1),img2 = image_flip(img2),label=label)}
  if (w == 4) {a=list(img1 = image_flop(image_flip(img1)),img2 = image_flop(image_flip(img2)),label=label)}
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
#########################################################
  label_to_matrix <- function(label) {
      if (label== "An"){labelMatrix=matrix(c(1,0,0,0),1,4)}
	   if (label== "F_"){labelMatrix=matrix(c(0,1,0,0),1,4)}
	    if (label== "J_"){labelMatrix=matrix(c(0,0,1,0),1,4)}
		 if (label== "Sa"){labelMatrix=matrix(c(0,0,0,1),1,4)}
labelMatrix		 
}
#############################################################
})

registerDoParallel(cl)
##########################################################
train_iterator <- py_iterator(train_generator(images_dir1 = images_dir1,
                                              images_dir2 = images_dir2,
                                              samples_index = train_index,
                                              batch_size = batch_size))

val_iterator <- py_iterator(val_generator(val_images_dir1 = val_images_dir1,
                                          val_images_dir2 = val_images_dir2,
                                          samples_index = val_index,
                                          batch_size = batch_size))

iter_next(train_iterator)
iter_next(val_iterator)			
#######################################################################
K <- backend()




#####################################################################################
#####################################################
conv_base1 <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(256, 256, 3))
  conv_base1$name="Left_vgg16"
########################################################################
conv_base2 <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150, 150, 3)
  )
   conv_base2$name="Right_vgg16"
############################################
left_input <- layer_input(shape = c(256, 256, 3))
right_input <- layer_input(shape = c(150, 150, 3))

 
left_features <- left_input %>% conv_base1() %>% layer_flatten(name = "L_branch1")
right_features <- right_input %>% conv_base2() %>% layer_flatten(name = "L_branch2")



predictions <- layer_concatenate(list(left_features, right_features)) %>%
              layer_dense(units = 512, activation = "relu",name = "fc3")  %>%
		     #layer_batch_normalization() %>%
			  layer_dense(units = 256, activation = "relu",name = "fc4") %>%
			#  layer_dropout(rate=0.2) %>%	
              layer_dense(units = 4, activation = "softmax",name = "predictions")

model_age_read <- keras_model(list(left_input, right_input), predictions)
###############################################################
freeze_weights(conv_base1)
freeze_weights(conv_base2)
###########################################
model_age_read %>% compile(
  optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
#############################################################################################################
early_stopping <- callback_early_stopping(patience = 4)
filepath <- file.path(checkpoint_dir, "Val_{val_acc:.2f}_epoch_{epoch:02d}_AgeSSL.h5")
cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 1,
  #save_weights_only = TRUE,
  verbose = 1)

############################################################################
model_age_read %>% fit_generator(
  train_iterator,
  steps_per_epoch =   2, #steps_per_epoch,
  epochs =    2,           # epochs, 
  validation_data = val_iterator,
  validation_steps =  2,  #steps_per_epoch,
  verbose = 1,
  callbacks = list(early_stopping,cp_callback)
)
###########################################################################
stopCluster(cl)
