            
			library(keras)
		    library(magick)





							 train_Gen_dir= "D:\\PL_DB\\2021_3101_OPP\\20210725_084310\\Predict\\LRG_Measurements"
							 ImgToFold=F
							  NewModelCreate=T
							  BaseModel_pth =  ""
							  Species="LRG"
							  bth_size =16
							  trgt_size =256
							  epochs=50
							  validation_split=0.1
							  TrainIndex=1  # every img 4 times for 1 epoch
							  ValIndex=1
							  rotation_range = 0.10
							  width_shift_range = 0.01
							  height_shift_range = 0.01
							#  shear_range = 0.01
							#  zoom_range = 0.01
							  brightness_range=c(1,3)
							  fill_mode = "nearest"
					
						
		     

				 dateTrain=format(Sys.time(),  "%b%d %Y") 
				 checkpoint_dir=paste0(train_Gen_dir,"\\Checkpoints");if(dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}
				 train_dir =  paste0(train_Gen_dir,"\\Train")
	
		
				

	
				 
				 AllImg=list.files(train_dir,recursive=T,include.dir=F)
				 train_step=round(length(AllImg)/bth_size*TrainIndex* (1-validation_split))
				 val_step=round(length(AllImg)/bth_size*ValIndex* validation_split)	
	############################################
	if (ImgToFold==T) {
TableImgs=read.csv(pthtblB)
category1=unique(TableImgs$lngth)
for (i in 1:length(category1)) {
category2=category1[i]
CategoryFold=paste0(dirSave,"\\",category2)

listImgsSub=TableImgs$NewImgPth[TableImgs$lngth==category2]
 for(u in 1:length(listImgsSub)) {
 
 if (file.exists(listImgsSub[u])){
  dir.create(CategoryFold,showWarnings = F)
file.copy(listImgsSub[u],CategoryFold)
}
}
}
}
	##########################################
	train_datagen = image_data_generator(
	  rescale = 1/255,
	  horizontal_flip = T, vertical_flip = T,
	  rotation_range = rotation_range,
	  width_shift_range = width_shift_range,
	 height_shift_range = height_shift_range,
	 # shear_range = shear_range,
	#	zoom_range = zoom_range,
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
	

	model_regresion %>% compile(
                              optimizer = "adam",
                              loss = "mse",
                              metrics = c("mae")
                               )					
						
						model_regresion
						
	} else {model_regresion=model_regresion(BaseModel_pth)}					
	###########################
	 model_regresion %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step)
	  
	
	#   callbacks = list(early_stopping,cp_callback))

	


