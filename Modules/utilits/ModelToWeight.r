
library(keras)
library(reticulate)
use_condaenv("base",required = TRUE)
#py_config() 
ModelConvert=  "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\Unet_new data\\Checkpoints\\Unet_new data_20250520_val_0.53_epoch_200.h5"


filepathRDS=tools::file_path_sans_ext(ModelConvert)
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
#bce_dice_loss <- function(y_true, y_pred) {
#  result <- loss_binary_crossentropy(y_true, y_pred) +
#    (1 - dice_coef(y_true, y_pred))
#  return(result)
#}


unet1 <- load_model_hdf5(ModelConvert, custom_objects = c(dice_coef = dice_coef,
                                                        dice_coef_loss=dice_coef_loss))


unet1 <- unet1 %>%
        compile(
        optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),     #optimizer_nadam              
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
    )	
a1=get_weights(unet1)
saveRDS(a1,filepathRDS)