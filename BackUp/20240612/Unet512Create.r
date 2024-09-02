UnetVGG16Create=function () {
  library(keras)
  K <- backend()
###############################################################################################################
  if (exists("custom_metric")) {
    dice_coef <- custom_metric("dice_coef", function(y_true, y_pred) {
      smooth = 1
      y_true_f <- k_flatten(y_true)
      y_pred_f <- k_flatten(y_pred)
      intersection <- k_sum(y_true_f * y_pred_f)
      (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
    })
  } else {
    dice_coef <- function(y_true, y_pred, smooth = 1) {
      y_true_f <- k_flatten(y_true)
      y_pred_f <- k_flatten(y_pred)
      intersection <- k_sum(y_true_f * y_pred_f)
      (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
    }
  }

  dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
#########################################################################################################
  unet_layer <- function(object, filters, kernel_size = c(3, 3),
                         padding = "same", kernel_initializer = "he_normal",
                         dropout = 0.1, activation="relu"){
    object %>%
      layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
      layer_batch_normalization() %>%
      layer_activation(activation) %>%
      layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
      layer_batch_normalization() %>%
      layer_activation(activation)
  }

  unet1 <- function(shape, nlevels = 4, nfilters = 16, dropouts = c(0.1, 0.1, 0.2, 0.2, 0.3)){
    message("Constructing U-Net with ", nlevels, " levels initial number of filters is: ", nfilters)
    filter_sizes <- nfilters*2^seq.int(0, nlevels)
    clayers <- clayers_pooled <- list()
    clayers_pooled[[1]] <- layer_input(shape = shape)

    for(i in 2:(nlevels+1)) {
      clayers[[i]] <- unet_layer(clayers_pooled[[i - 1]], filters = filter_sizes[i - 1], dropout = dropouts[i-1])
      clayers_pooled[[i]] <- layer_max_pooling_2d(clayers[[i]], pool_size = c(2, 2), strides = c(2, 2))
    }

    elayers <- list()
    elayers[[nlevels + 1]] <- unet_layer(clayers_pooled[[nlevels + 1]], filters = filter_sizes[nlevels + 1], dropout = dropouts[nlevels + 1])

    for(i in nlevels:1) {
      elayers[[i]] <- layer_conv_2d_transpose(elayers[[i+1]],
                                              filters = filter_sizes[i],
                                              kernel_size = c(2, 2),
                                              strides = c(2, 2),
                                              padding = "same")
      elayers[[i]] <- layer_concatenate(list(elayers[[i]], clayers[[i + 1]]), axis = 3)
      elayers[[i]] <- unet_layer(elayers[[i]], filters = filter_sizes[i], dropout = dropouts[i])
    }

    outputs <- layer_conv_2d(elayers[[1]], filters = 1, kernel_size = c(1, 1), activation = "sigmoid")
    return(keras_model(inputs = clayers_pooled[[1]], outputs = outputs))
  }

  unet1 <- unet1(shape = c(512, 512, 3), nlevels = 4, nfilters = 16, dropouts = c(0.1, 0.1, 0.1, 0.1,0.1))
  unet1 <<- unet1 %>%
    compile(
      optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),
      loss = dice_coef_loss,
      metrics = c(dice_coef)
    )

  unet1
}
UnetVGG16Create()
