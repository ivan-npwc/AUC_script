library(keras)
K <- backend()
PTH= "D:\\SSL_DB\\TRAIN\\WLRS\\Checkpoints\\WLRS_20210814_Val_0.22_epoch_35_256.h5"
#################################
if (exists("custom_metric")==T) {
dice_coef <<- 
	  custom_metric("dice_coef", function(y_true, y_pred) {
		smooth = 1
		y_true_f <- k_flatten(y_true)
		y_pred_f <- k_flatten(y_pred)
		intersection <- k_sum(y_true_f * y_pred_f)
		(2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
	  }) } else {
	  


 dice_coef <<- function(y_true, y_pred, smooth = 1) {
    y_true_f <- k_flatten(y_true)
    y_pred_f <- k_flatten(y_pred)
    intersection <- k_sum(y_true_f * y_pred_f)
    (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  }


}	  
	dice_coef_loss <<- function(y_true, y_pred) -dice_coef(y_true, y_pred)
	unetModel =load_model_hdf5(PTH, custom_objects=c(dice_coef=dice_coef,dice_coef_loss=dice_coef_loss))
	
	
	a=get_weights(unetModel)
	saveRDS(a,"WLRS_WLRS_20210814_Val_0.22_epoch_35_256")
	