
library(keras)
K <- backend()
#######################################
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
######################################################################
unet_layer <- function(object, filters, kernel_size = c(3, 3),
                       padding = "same", kernel_initializer = "he_normal",
                       dropout = 0.1, activation="relu"){

    object %>%
        layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
        layer_batch_normalization() %>%
        layer_activation(activation) %>%
        layer_batch_normalization() %>%
        layer_spatial_dropout_2d(rate = dropout) %>%
        layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
        layer_batch_normalization() %>%
        layer_activation(activation)
}
unet_f <- function(shape, nlevels = 4, nfilters = 16, dropouts = c(0.1, 0.1, 0.2, 0.2, 0.3)){
    message("Constructing U-Net with ", nlevels, " levels initial number of filters is: ", nfilters)
    filter_sizes <- nfilters*2^seq.int(0, nlevels)
    ## Loop over contracting layers
    clayers <- clayers_pooled <- list()
    ## inputs
    clayers_pooled[[1]] <- layer_input(shape = shape)
    for(i in 2:(nlevels+1)) {
        clayers[[i]] <- unet_layer(clayers_pooled[[i - 1]],
                                   filters = filter_sizes[i - 1],
                                   dropout = dropouts[i-1])

        clayers_pooled[[i]] <- layer_max_pooling_2d(clayers[[i]],
                                                    pool_size = c(2, 2),
                                                    strides = c(2, 2))
    }

    ## Loop over expanding layers
    elayers <- list()

    ## center
    elayers[[nlevels + 1]] <- unet_layer(clayers_pooled[[nlevels + 1]],
                                         filters = filter_sizes[nlevels + 1],
                                         dropout = dropouts[nlevels + 1])

    for(i in nlevels:1) {
        elayers[[i]] <- layer_conv_2d_transpose(elayers[[i+1]],
                                                filters = filter_sizes[i],
                                                kernel_size = c(2, 2),
                                                strides = c(2, 2),
                                                padding = "same")

        elayers[[i]] <- layer_concatenate(list(elayers[[i]], clayers[[i + 1]]), axis = 3)
        elayers[[i]] <- unet_layer(elayers[[i]], filters = filter_sizes[i], dropout = dropouts[i])

    }
    ## Output layer
    outputs <- layer_conv_2d(elayers[[1]], filters = 1, kernel_size = c(1, 1), activation = "sigmoid")
    return(keras_model(inputs = clayers_pooled[[1]], outputs = outputs))
}
#########################################################
unet1 <- unet_f(shape = c(256, 256, 3), nlevels = 4, nfilters = 16, dropouts = c(0.1, 0.1, 0.2, 0.2, 0.2))




