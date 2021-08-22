library(keras)
library(magick)
ValIndex=0.3
bth_size=32
trgt_size=256
#epochs=30
#####################################################################################################
train_Gen_dir=  "C:\\SSL_DB\\SSL_AUC\\20191218\\data\\TRAIN\\Age_SSL"


checkpoint_dir=paste0(train_Gen_dir,"\\Checkpoints")
if(dir.exists(checkpoint_dir)==F) {dir.create(checkpoint_dir)}
train_dir= paste0(train_Gen_dir,"\\Train")   
val_dir= paste0(train_Gen_dir,"\\Validate") 
###########################################################################################################
early_stopping <- callback_early_stopping(patience = 10)   # 5 epoch for check if regress exists
filepath <- file.path(checkpoint_dir, "Val_{val_acc:.2f}_epoch_{epoch:02d}_status_read.h5")  
cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 1,
  # save_weights_only = TRUE,
  verbose = 1)
#########################################################################################################################
AllImg=list.files(train_dir,recursive=T,include.dir=F)
train_step=round(length(AllImg)*(1-ValIndex)/bth_size)*2
val_step=round(length(AllImg)*(ValIndex)/bth_size)   
############################################
train_datagen = image_data_generator(
  rescale = 1/255,
#  rotation_range = 30,
#  width_shift_range = 0.1,
#  height_shift_range = 0.1,
#  shear_range = 0.1,
#    zoom_range = 0.2,
   brightness_range=c(1,3),
#   horizontal_flip = TRUE,
#    vertical_flip=TRUE,
  fill_mode = "nearest"
)
train_generator <- flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  class_mode = "categorical")
##########################################################################
val_datagen <- image_data_generator(rescale = 1/255) 
validation_generator <- flow_images_from_directory(
  val_dir,
  val_datagen,
  target_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  class_mode = "categorical"
)
#####################################################
conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(trgt_size, trgt_size, 3))
########################################################################
model_sex_read <<- keras_model_sequential() %>%
conv_base %>%
               layer_flatten() %>%   
               layer_dropout(rate=0.5) %>%			   
              layer_dense(units = 512, activation = "relu",name = "fc3") %>%
		layer_batch_normalization() %>%
			  layer_dense(units = 256, activation = "relu",name = "fc4") %>%
			  layer_dropout(rate=0.2) %>%	
              layer_dense(units = train_generator$num_classes, activation = "softmax",name = "predictions")
###############################################################
freeze_weights(conv_base)
###########################################
model_sex_read %>% compile(
  optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
##################################################
history <- model_sex_read %>% fit_generator(
  train_generator,
  steps_per_epoch = train_step,
  epochs = 3,
  validation_data = validation_generator,
  validation_steps = val_step,
  verbose = 1)
#  callbacks = list(early_stopping,cp_callback)

#####################################
########################################################
#####################################################
unfreeze_weights(conv_base, from = "block3_conv1")
###########################################################
model_sex_read %>% compile(
  optimizer =   optimizer_adam(lr= 0.0001 , decay = 1e-6 ),  #optimizer_rmsprop(lr = 1e-5),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

model_sex_read %>% save_model_hdf5("PREmodel_sex_read.h5")
###########################################################
train_datagen = image_data_generator(
  rescale = 1/255,
#  rotation_range = 90,
#  width_shift_range = 0.3,
#  height_shift_range = 0.3,
#  shear_range = 0.3,
#  zoom_range = 0.1,
  brightness_range=c(1,3),
#   horizontal_flip = TRUE,
#    vertical_flip=TRUE,
  fill_mode = "nearest"
)
train_generator <- flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  class_mode = "categorical")
#############################################
early_stopping <- callback_early_stopping(patience = 10)   # 5 epoch for check if regress exists
filepath <- file.path(checkpoint_dir, "Val_{val_acc:.2f}_epoch_{epoch:02d}_status_read.h5")  
cp_callback <- callback_model_checkpoint( 
  filepath = filepath,
  period = 1,
  # save_weights_only = TRUE,
  verbose = 1)
################################################
history <- model_sex_read %>% fit_generator(
  train_generator,
  steps_per_epoch = train_step,
  epochs = 5,
  validation_data = validation_generator,
  validation_steps = val_step,
  verbose = 1,
  callbacks = list(early_stopping,cp_callback)
)
######################################################
model_sex_read %>% save_model_hdf5("model_sex_read.h5")
###############################################################
a=get_weights(model_sex_read)
saveRDS(a,pth_save_weight)
#############################################################

