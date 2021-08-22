library(keras)
pthPreRrizModel="C:\\SSL_DB\\SSL_AUC\\20190702\\data\\TRAIN\\NFS_Adult\\Checkpoints\\Val_0.86_epoch_02_256.h5"
###############################
dice_coef <- 
	  custom_metric("dice_coef", function(y_true, y_pred) {
		smooth = 1
		y_true_f <- k_flatten(y_true)
		y_pred_f <- k_flatten(y_pred)
		intersection <- k_sum(y_true_f * y_pred_f)
		(2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
	  })
	dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
########################################
PreRrizModel=load_model_hdf5(pthPreRrizModel  , custom_objects = c(dice_coef = dice_coef,
                                                               dice_coef_loss=dice_coef_loss)) 
###########################################################################
listLyerForFrezz=c(
"activation", 
"activation_1", 
"activation_2", 
"activation_3", 
"activation_4", 
"activation_5", 
"activation_6",
"activation_7",
"conv2d_8",
"activation_8",
"conv2d_9",
"activation_9")

for (i in 1:length(listLyerForFrezz)) {
freeze_weights(PreRrizModel %>% get_layer(listLyerForFrezz[i]))
}

