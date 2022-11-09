
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
	      batch_size =64
		  
	      mdlPTH_NFS_SSLa_SSLp = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/NFS-SSLadult-SSLpup__20220913_loss_0.08_epoch_01.h5")
		  mdlPTH_NFS_SSLa = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/NFS-SSL__20220902_loss_0.19_epoch_02.h5")
		  
	      WeightPTH_NFS_SSLa_SSLp =  paste0(System_data,"/weights/NFS_AGE/NFS_SSLad_SSL_pup-val92")
		  WeightPTH_NFS_SSladult =  paste0(System_data,"/weights/NFS_AGE/NFS-SSL_vgg16")
		   
          date1=substr(basename(labelInput),1,15)
      
 
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
	     if(exists("mdl_NFS_SSLa_SSLp")==F){mdl_NFS_SSLa_SSLp<<-load_model_hdf5(mdlPTH_NFS_SSLa_SSLp)}
		  Weight = readRDS(WeightPTH_NFS_SSLa_SSLp)
		  set_weights(mdl_NFS_SSLa_SSLp,Weight)  		
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
    pred= mdl_NFS_SSLa_SSLp %>% predict(pred_data_set)
	
###############################################################################################
    preds3=data.frame(pred)
	names(preds3) <- c("NFS","SSLpup","SSLadult")
	preds3$link=lstimgs
for (i in 1:length(lstimgs)) {
    if(preds3$NFS[i]> preds3$SSLpup[i] & preds3$NFS[i]>preds3$SSLadult[i]){preds3$name[i]="NFS"}
    if(preds3$SSLpup[i]> preds3$NFS[i] & preds3$SSLpup[i]> preds3$SSLadult[i]){preds3$name[i]="SSLpup"}
	if(preds3$SSLadult[i]> preds3$NFS[i] & preds3$SSLadult[i]> preds3$SSLpup[i]){preds3$name[i]="SSLadult"}
   }
   preds3$name[preds3$SSLpup < 0.98]="NFS"
###############
SSLimgsP=NULL
SSLimgsP=preds3$link[preds3$name=="SSLpup"]
SLLdirP=paste0(labelInput,"\\Predict\\",Species,"\\SSLpup"); dir.create(SLLdirP,showWarnings = F)
file.copy(SSLimgsP,SLLdirP); unlink(SSLimgsP)

SSLimgsA=NULL
SSLimgsA=preds3$link[preds3$name=="SSLadult"]
SLLdirA=paste0(labelInput,"\\Predict\\",Species,"\\SSLadult"); dir.create(SLLdirA,showWarnings = F)
file.copy(SSLimgsA,SLLdirA); unlink(SSLimgsA)
print("Done NFS-SSl_adult-SSL _pup filter")
###############################################################################################################################################################
#############################################################################################################################################################
#################################################################################################################################################
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
	 if(exists("mdl_NFS_SSL")==F){mdl_NFS_SSL<<-load_model_hdf5(mdlPTH_NFS_SSLa)}
		  Weight = readRDS(WeightPTH_NFS_SSladult)
		  set_weights(mdl_NFS_SSL,Weight)  		
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
    pred= mdl_NFS_SSL %>% predict(pred_data_set)
	
###############################################################################################

    preds3=data.frame(pred)
	names(preds3) <- c("NFS","SSLerr")
	preds3$link=lstimgs
for (i in 1:length(lstimgs)) {
    if(preds3$NFS[i]>     preds3$SSLerr[i]){preds3$name[i]="NFS"}
    if(preds3$SSLerr[i]> preds3$NFS[i]){preds3$name[i]="SSLerr"}
   }
   preds3$name[preds3$SSLerr < 0.8]="NFS"

SSLimgs=NULL
SSLimgs=preds3$link[preds3$name=="SSLerr"]
SLLdir=paste0(labelInput,"\\Predict\\",Species,"\\SSLerr"); dir.create(SLLdir)
file.copy(SSLimgs,SLLdir); unlink(SSLimgs)
print("Done NFS-SSl filter")










