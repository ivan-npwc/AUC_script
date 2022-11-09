          library(keras)
		  library(magick)
							 train_Gen_dir=  dirToF="C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS-SSL" 
							  ImgCountNormise=F
							  NewModelCreate=F
							  BaseModel_pth =  "C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS-SSL\\Checkpoints\\SSL_adult_P_NFS_crop__20220920_loss_0.24_epoch_03.h5"
							  Species="NFS-SSl_split"
							  bth_size =64
							  trgt_size =256
							  epochs=3
							  validation_split=0.2
							
						
							  
							  dfrmn=T
						
		
		  
          VN=list.files(paste0(train_Gen_dir,"\\All_norm"))
		    VN2=NULL
			for (i in 1: length(VN)){
			VN1=VN[i]
			VN2=paste0(VN1,"_",VN2)
			}
			
		Species=VN2

				 dateTrain=format(Sys.time(),  "%Y%m%d") 
				 checkpoint_dir=paste0(train_Gen_dir,"\\Checkpoints");if(dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}
		   
				 train_dir =  paste0(train_Gen_dir,"\\All_norm")
				 pth_save_weight=paste0(train_Gen_dir,"\\Checkpoints\\",Species,"_",dateTrain)
				 Pth_save_name=paste0(train_Gen_dir,"\\",Species,"_",dateTrain,"_Name.csv")
				 nameBrand=list.files(train_dir)
				# write.csv(nameBrand,Pth_save_name,row.names = F)
				 
				 AllImg=list.files(train_dir,recursive=T,include.dir=F)
				 train_step=round(length(AllImg)/bth_size*1* (1-validation_split))
				 val_step=round(length(AllImg)/bth_size*1* validation_split)
	####################################
	if (ImgCountNormise== T) {
		   AllDir=paste0(train_Gen_dir,"\\All") 
		   All_normDir=paste0(train_Gen_dir,"\\All_norm")
		       unlink(All_normDir,recursive=T);dir.create(All_normDir)
		   listFolderCopy=list.files(AllDir,full.names=T)
           file.copy(listFolderCopy, All_normDir,  recursive = T)
           listFolder=list.files(All_normDir,full.names=T)
             TableCountImg=NULL
          for (i in 1:length(listFolder)) {
              pth.fold=paste0(listFolder[i])
              countImg=length(list.files(pth.fold))
              row1=data.frame(countImg=countImg,folder=basename(pth.fold))
              TableCountImg= rbind(TableCountImg,row1)}
                 meanCountImg=   median(TableCountImg$countImg)
                 minimum=  7  #round(meanCountImg*0.2)
                 maximum= round(meanCountImg * 2)
  for (i in 1:length(listFolder)) {
    pth.fold=paste0(listFolder[i])
    listImg=list.files(pth.fold,full.names=T)
    countImg=length(listImg)
    if (countImg<minimum) {unlink(pth.fold,recursive = T)} else {
      if (countImg > maximum) {
        diffDel= countImg-maximum
        deleteIndex=sample(1:countImg)[1:diffDel]
        deleteList=listImg[deleteIndex]
        unlink(deleteList)
      }}}
	listFolder=list.files(All_normDir,full.names=T) 
  for (i in 1:length(listFolder)) {	
    pth.fold=paste0(listFolder[i])
    listImg=list.files(pth.fold,full.names=T)
    countImg=length(listImg)
   # if (countImg<meanCountImg) {
      difference= round(maximum-countImg)
      for (f in 1: difference) {
        index=sample(1:length(listImg))[1]
        randomImgPth<<-paste0(listImg[index])
        RandomImg=image_read(randomImgPth)
		   defIndex= sample(c(-0.35,-0.3,-0.25,-0.2,-0.15,-0.1,0.1,0.15,0.2,0.25,0.3,0.35))[1]
            if(dfrmn==T){ RandomImg= image_implode(RandomImg, factor = defIndex)}
        newPth<<-paste0(pth.fold,"\\",f,"_",basename(randomImgPth))
        image_write(RandomImg,newPth)
      }
	#  }
	  }
 train_dir=  paste0(train_Gen_dir,"\\All_norm")
}
#####################################	


		   early_stopping <<- callback_early_stopping(patience = 5)   # 5 epoch for check if regress exists
			cp_callback <- callback_model_checkpoint( 
				filepath = paste0(checkpoint_dir, "\\",Species, "_",dateTrain,"_", basename(file.path(checkpoint_dir, "loss_{val_loss:.2f}_epoch_{epoch:02d}.h5"))),
			#	period = 1,
				verbose = 1)  
	############################################
	train_datagen = image_data_generator(
	  rescale = 1/255,
	  horizontal_flip = T, vertical_flip = T,
	 # rotation_range = 45,
	 # width_shift_range = width_shift_range,
	 # height_shift_range = height_shift_range,
	 # shear_range = shear_range,
	#	zoom_range = zoom_range,
#  brightness_range=c(1,3),
	  fill_mode = "constant",
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
		   
		   modelTrain <<- keras_model_sequential() %>%
											   conv_base %>%
							         layer_dropout(rate = 0.5) %>% 
										  layer_flatten() %>%  
	layer_dense(units = 512, activation = "relu",name = "fc3") %>% #512
    layer_batch_normalization() %>% 
	layer_dense(units = 256, activation = "relu",name = "fc4") %>%   #256
    layer_batch_normalization() %>%    	
	layer_dense(units = train_generator$num_classes, activation = "softmax",name = "predictions")
	 
	freeze_weights(conv_base)
	
	modelTrain %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))
						modelTrain
	###########################
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	   callbacks = list(cp_callback)) #early_stopping

	#################################
	unfreeze_weights(conv_base, from = "block3_conv1")
	modelTrain
	modelTrain %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))					
###############################################

	train_step=round(length(AllImg)/bth_size*1* (1-validation_split))
##########################################################								
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch =   train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))
	  }
	  #############################
	  						 
#############################
if (NewModelCreate==F){
                      
							
							   TrainIndex=1
                               train_step=round(length(AllImg)/bth_size*1* (1-validation_split))
								
modelTrain=load_model_hdf5(BaseModel_pth)
	  conv_base=get_layer(modelTrain,"vgg16")
	  unfreeze_weights(conv_base, from = "block3_conv1")
	 modelTrain
	  modelTrain %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))
						
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))

	  }
