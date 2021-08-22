library(keras)
library(magick)
PTHweight="G:\\Мой диск\\AUC_script\\System data\\weights\\NFSAdult_20200212_val087"
################
K <- backend()
 dice_coef <- function(y_true, y_pred, smooth = 1) {
    y_true_f <- k_flatten(y_true)
    y_pred_f <- k_flatten(y_pred)
    intersection <- k_sum(y_true_f * y_pred_f)
    (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  }
  dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
  attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"
  attr(dice_coef, "py_function_name") <- "dice_coef"
  ###################
source("Modules/unetVGG16Create.r")

   weight=readRDS(PTHweight)
   set_weights(unet1,weight)
   
   unet1 <- unet1 %>%
        compile(
        optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),     #optimizer_nadam              
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
    )


#############################
img_path <-  "D:\\AUC_data\\NFS_Adult\\Image\\NFS_20180727_d021_f30313.JPG" #file.choose()           
       img <- image_read(img_path)
        img <- image_scale(img, "256x256!")		
        img_tensor <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
        dim(img_tensor) <- c(1, 256, 256, 3)

	#####################
	MYpreds <- unet1 %>% predict(img_tensor)
	
	
	
	
	
	MYafrican_elephant_output <- unet1$output   #[, 1] 

	
	MYoutput=  get_output_at(unet1,node_index=1)
	
	MYlast_conv_layer <- unet1 %>% get_layer("conv2d_7") 

	grads=k_gradients(MYoutput, MYlast_conv_layer$output)[[1]]    

pooled_grads <- k_mean(grads, axis = c(1, 2, 3))
             inputs=  get_input_at(unet1,node_index=1)
iterate <- k_function(list(inputs),
list(pooled_grads, MYlast_conv_layer$output[1,,,]))
c(pooled_grads_value, conv_layer_output_value) %<-% iterate(list(img_tensor))
for (i in 1:512) {
conv_layer_output_value[,,i] <-
conv_layer_output_value[,,i] * pooled_grads_value[[i]]
}


heatmap <- apply(conv_layer_output_value, c(1,2), mean)

#####
heatmap <- pmax(heatmap, 0)
heatmap <- heatmap / max(heatmap)

write_heatmap <- function(heatmap, filename, width = 224, height = 224,
bg = "white", col = terrain.colors(12)) {
png(filename, width = width, height = height, bg = bg)
op = par(mar = c(0,0,0,0))
on.exit({par(op); dev.off()}, add = TRUE)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(heatmap), axes = FALSE, asp = 1, col = col)
}
write_heatmap(heatmap, "conv2d_11.png")
#########################################################################













blobIN=image_read("elephant_heatmap.png") 

library(magick)
library(viridis)
image <- image_read(MYimg_path)
info <- image_info(image)
geometry <- sprintf("%dx%d!", info$width, info$height)
pal <- col2rgb(viridis(20), alpha = TRUE)
alpha <- floor(seq(0, 255, length = ncol(pal)))
pal_col <- rgb(t(pal), alpha = alpha, maxColorValue = 255)
 #write_heatmap(heatmap, "elephant_overlay.png",
  #width = 14, height = 14, bg = NA, col = pal_col)
#########################
image_read("elephant_heatmap.png") %>%
image_resize(geometry, filter = "quadratic") %>%
image_composite(image, operator = "blend", compose_args = "20") %>%
plot()













