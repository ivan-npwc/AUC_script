	#TrainBrandRead= function (
							 train_Gen_dir= "C:\\SSL_DB\\SSL_age\\HAULOUT"
							  
							  NewModelCreate=T
							  BaseModel_pth =  "C:\\SSL_DB\\SSL_age\\HAULOUT\\Checkpoints\\SSLAdult_Feb 19  2021_Val_0.65_epoch_01.h5"
							  Species="SSLAdult"
							  bth_size =64
							  trgt_size =256
							  epochs=5
							  validation_split=0.02
							  TrainIndex=3  # every img 4 times for 1 epoch
							  ValIndex=1
							  rotation_range = 0.01
							  width_shift_range = 0.01
							  height_shift_range = 0.01
							  shear_range = 0.01
							  zoom_range = 0.01
							  brightness_range=c(1,3)
							  fill_mode = "nearest"
							  
							  #) {

		  library(keras)
		  library(magick)

				 dateTrain=format(Sys.time(),  "%b %d  %Y") 
				 checkpoint_dir=paste0(train_Gen_dir,"\\Checkpoints");if(dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}
		   
				 train_dir =  paste0(train_Gen_dir,"\\All_norm")
				 pth_save_weight=paste0(train_Gen_dir,"\\Checkpoints\\",Species,"_",dateTrain)
				 Pth_save_name=paste0(train_Gen_dir,"\\",Species,"_",dateTrain,"_Name.csv")
				 nameBrand=list.files(train_dir)
				 write.csv(nameBrand,Pth_save_name,row.names = F)
				 
				 AllImg=list.files(train_dir,recursive=T,include.dir=F)
				 train_step=round(length(AllImg)/bth_size*TrainIndex* (1-validation_split))
				 val_step=round(length(AllImg)/bth_size*ValIndex* validation_split)
	####################################	 
		   early_stopping <<- callback_early_stopping(patience = 3)   # 5 epoch for check if regress exists
			cp_callback <- callback_model_checkpoint( 
				filepath = paste0(checkpoint_dir, "\\",Species, "_",dateTrain,"_", basename(file.path(checkpoint_dir, "Val_{val_acc:.2f}_epoch_{epoch:02d}.h5"))),
				period = 1,
				verbose = 1)  
	############################################
	train_datagen = image_data_generator(
	  rescale = 1/255,
	  horizontal_flip = T, vertical_flip = T,
	  rotation_range = rotation_range,
	  width_shift_range = width_shift_range,
	  height_shift_range = height_shift_range,
	  shear_range = shear_range,
		zoom_range = zoom_range,
	   brightness_range=brightness_range,
	  fill_mode = fill_mode,
	  validation_split=validation_split
	)
	val_datagen=image_data_generator(rescale = 1/255,validation_split=validation_split)
	##########################################################
	train_generator <- flow_images_from_directory(
	  train_dir,
	  train_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	 subset="training")
	#############################################################
	validation_generator <- flow_images_from_directory(
	  train_dir,
	  val_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	  subset="validation"
	)
	##############################################################################################
	if (NewModelCreate==T){
		  conv_base <- application_vgg16(
		   weights = "imagenet",
		   include_top = FALSE,
		   input_shape = c(trgt_size, trgt_size, 3))
		   
		   model_brand_read <<- keras_model_sequential() %>%
											   conv_base %>%
							 #  layer_dropout(rate = 0.5) %>% 
										  layer_flatten() %>%  
#	layer_dense(units = 512, activation = "relu",name = "fc3") %>%  
	layer_dense(units = 256, activation = "relu",name = "fc4") %>%   
    layer_batch_normalization() %>%    	
	layer_dense(units = train_generator$num_classes, activation = "softmax",name = "predictions")
	 
	freeze_weights(conv_base)
	
	model_brand_read %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))
						model_brand_read
	###########################
	 model_brand_read %>% fit_generator(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	   callbacks = list(early_stopping,cp_callback))

	#################################
	unfreeze_weights(conv_base, from = "block3_conv1")
	model_brand_read
	model_brand_read %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))					
###############################################
							   epochs=3
							   TrainIndex=6
	train_step=round(length(AllImg)/bth_size*TrainIndex* (1-validation_split))
##########################################################								
	 model_brand_read %>% fit_generator(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))
	  }
if (NewModelCreate==F){

model_brand_read=load_model_hdf5(BaseModel_pth)
	  conv_base=get_layer(model_brand_read,"vgg16")
	  unfreeze_weights(conv_base, from = "block3_conv1")
	 
	  model_brand_read %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))
	 model_brand_read %>% fit_generator(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))

	  }
