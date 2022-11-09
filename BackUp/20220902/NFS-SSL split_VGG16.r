 library(dplyr)
 library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(abind)
library(parallel)
library(doParallel)
library(foreach)
library(EBImage)
library(keras)
library(magick) 
	
	
	
	
          labelInput
		  Species
		  vision_dimensions =256
	      batch_siz e=64
	      mdlPTH = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/TF_F__20220819_loss_0.13_epoch_01.h5")
	      WeightPTH =  paste0(System_data,"/weights/NFS_AGE/NFS_age__AN-Bch_20220826_loss_026_epoch_05" )

         date1=substr(basename(labelInput),1,15)
         PthTblAgeRef =paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
 
Rlistfiles =NULL
Hlistfiles =NULL

DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery");Rlistfiles=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout");Hlistfiles=list.files(DirH,full.names=T)
if (dir.exists(DirH)==F) {
DirR=paste0(labelInput,"\\Predict\\Age_predict\\Rookery");Rlistfiles=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\Age_predict\\Haulout");Hlistfiles=list.files(DirH,full.names=T)
}
lstimgs = c(Rlistfiles,Hlistfiles)
##################################################################################
	     if(exists("model")==F){RmodelAge<<-load_model_hdf5(mdlPTH)}
		  Weight = readRDS(WeightPTH)
		  set_weights(model,Weight)  		
############################################################################
  data <- tibble::tibble(img = lstimgs)
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	 
	 dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img))
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		))
	   dataset3 <- dataset %>% dataset_batch(batch_size) %>%
                        	#   dataset_cache() %>%
							   dataset_map(unname) # Keras needs an unnamed output.
							   #%>% dataset_shuffle(buffer_size = #batch_size*vision_dimensions)
	}
	pred_data_set <- create_dataset(data, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
    pred=model %>% predict(pred_data_set)
	
###############################################################################################

    preds3=data.frame(pred)
	names(preds3) <- Age_Name
	preds3$link=basename(as.character(listImgPred))
for (i in 1:length(listImgPred)) {
   if(preds3$F[i]> preds3$TF[i]){preds3$name[i]="NFS"}
   if(preds3$F[i]< preds3$TF[i]){preds3$name[i]="SSL"}
   }
#################################################################################################
