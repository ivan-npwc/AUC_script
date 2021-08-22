#https://towardsdatascience.com/understanding-semantic-segmentation-with-unet-6be4f42d4b47
#https://keras.rstudio.com/articles/examples/unet.html
#################################################################
#this code removes fit_generator
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
# Parameters -----------------------------------------------------
    TypeTrain = "Retrain" # "TrainEmptyModel"
	trainDir ="D:\\SSL_DB\\TRAIN\\WLRS"
	Weight ="D:\\SSL_DB\\TRAIN\\WLRS\\Checkpoints\\WLRS_20200227"
	epochs =5
	batch_size=16L	
	vision_dimensions=256
	ValTrainSplit=0.9 # use 90% for train and 10 for validate
	
	
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	images_dir=paste0(trainDir,"\\Image")
	masks_dir=paste0(trainDir,"\\Mask")
	shape=c(vision_dimensions,vision_dimensions,3)
############
     listImgS <<- list.files(images_dir)
	 ImgsExten=extension(listImgS)[1]
	 listImgSNoExt=substr(listImgS,1,nchar(listImgS)-nchar(ImgsExten))
	                   if (length(listImgS)==0) {stop ("No Images found")}
     ListMskS<<- list.files(masks_dir)
	 MskExten=extension(ListMskS)[1]
	 MskNoExt=substr(ListMskS,1,nchar(ListMskS)-nchar(MskExten))
     deleteListImgs<<- listImgSNoExt[!(listImgSNoExt %in% MskNoExt)] # here is IMGS DELETE without msk 
     deleteListMsks<<- MskNoExt[!(MskNoExt %in% listImgSNoExt)]
     deleteListImgs1=paste0(images_dir,"\\",deleteListImgs,ImgsExten)
     deleteListMsk1=paste0(masks_dir,"\\",deleteListMsks,MskExten)
	 unlink(deleteListImgs1)
	 unlink(deleteListMsk1)
#################
######################################
if (exists("unet1")==F){source("D:\\MachineLearning\\AUC_script\\Modules\\UnetVGG16Create.r")}#source("Modules/UnetVGG16Create.r")
if (TypeTrain == "Retrain"){
     SWeight=readRDS(Weight)
	 set_weights(unet1,SWeight)}
	 
	 
unet1 <- unet1 %>%
    compile(
        optimizer = optimizer_adam(lr= lr = 2e-6),# optimizer_adam(lr= 0.0001, decay = 1e-6 )  optimizer_rmsprop(lr = 2e-5)
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
   )
########################################	
	  data <- tibble::tibble(
	  img = list.files(images_dir, full.names = TRUE),
	  mask = list.files(masks_dir, full.names = TRUE))
	  
	  data <- rsample::initial_split(data, prop = ValTrainSplit)	
##########################################################################
	random_bsh <- function(img) {                                           
	img %>% 
	tf$image$random_brightness(max_delta = 0.3) %>% 
	tf$image$random_contrast(lower = 0.5, upper = 0.7) %>% 
	tf$image$random_saturation(lower = 0.5, upper = 0.7) %>% 
	tf$clip_by_value(0, 1) # clip the values into [0,1] range.
	}
############
left_right  <- function(img) {img %>% tf$image$flip_left_right()}
up_down <- function(img) {img %>% tf$image$flip_up_down()}

############	
#universal function	
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_png(tf$io$read_file(.x$img)),
		  mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float64)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
	  
	  
		dataset1 <- dataset %>% 
		  dataset_map(~.x %>% list_modify(
			img = random_bsh(.x$img)
		  )) 
		  
	if ( rnorm(1)>0) {	  
	  dataset1 <- dataset1 %>% dataset_map(~.x %>% list_modify(img = left_right(.x$img))) 
	  dataset1 <- dataset1 %>% dataset_map(~.x %>% list_modify(mask = left_right(.x$mask))) 
	 } 
	  
	if ( rnorm(1)>0) {	  
	  dataset1 <- dataset1 %>% dataset_map(~.x %>% list_modify(img = up_down(.x$img))) 
	  dataset1 <- dataset1 %>% dataset_map(~.x %>% list_modify(mask = up_down(.x$mask))) 
	 }


		dataset2 <- dataset1 %>% 
		  dataset_shuffle(buffer_size = batch_size*vision_dimensions)
	 
	  
	  dataset3 <- dataset2 %>% 
		dataset_batch(batch_size)
	  
	  
	  
	  dataset3 %>% 
		dataset_map(unname) # Keras needs an unnamed output.
		
	
	}
	
	training_dataset <- create_dataset(training(data), train = TRUE, batch_size=batch_size, vision_dimensions=vision_dimensions)
	validation_dataset <- create_dataset(testing(data), train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
# Training -----------------------------------------------------
checkpoint_dir=paste0(trainDir,"\\Checkpoints");dir.create(checkpoint_dir,showWarnings = F)
BaseName <<- basename(file.path(checkpoint_dir, "Val_{val_dice_coef:.2f}_epoch_{epoch:02d}_256.h5"))
filepath <<- paste0(checkpoint_dir,"\\",Species,"_",dateTrain,"_",BaseName)


callbacks_list <- list(
 # callback_tensorboard(paste0(checkpoint_dir,output_name,"_",vision_dimensions[1])),
  callback_early_stopping(monitor = "val_dice_coef",
                          min_delta = 1e-4,
                          patience = 4,
                          verbose = 1,
                          mode = "max"),
  callback_reduce_lr_on_plateau(monitor = "val_dice_coef",
                                factor = 0.1,
                                patience = 2,
                                verbose = 1,
                                mode = "max"),
  callback_model_checkpoint(filepath = filepath,
                            monitor = "val_dice_coef",
							 period = 1)
                           )

unet1 %>%  keras:::fit.keras.engine.training.Model(
  training_dataset,  
  epochs = epochs, 
  validation_data = validation_dataset,  
  callbacks = callbacks_list)




