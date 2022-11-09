
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
		  
	      mdlPTH_NFS_SSLa_SSLp = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/SSLp_SSLa_NFS__20220923_loss_0.07_epoch_03.h5")
		
	     
		  date1=substr(basename(labelInput),1,15)
      
		  PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
		
          DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery")
          DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout")
		 
		  SLLdirA=paste0(labelInput,"\\Predict\\",Species,"\\SSLadult"); dir.create(SLLdirA,showWarnings = F)
          SLLdirP=paste0(labelInput,"\\Predict\\",Species,"\\SSLpup"); dir.create(SLLdirP,showWarnings = F)	  
################################################################################################    if function use again with new models
        TblAgeRef=read.csv(PthTblAgeRef)
		SSLimgsA=NULL;SSLimgsP=NULL;
		SSLimgsA=list.files(SLLdirA,full.names=T);SSLimgsP=list.files(SLLdirP,full.names=T)
		lstImgsErr=c(SSLimgsA,SSLimgsP)
if (length(lstImgsErr) != 0 ){
    TblAgeRef1=TblAgeRef[basename(TblAgeRef$pth_save_img) %in% basename(lstImgsErr),]
	 for (i in 1:length(TblAgeRef1$pth_save_img)){
    to=TblAgeRef1$pth_save_img[i]
    from= lstImgsErr[basename(lstImgsErr)==basename(to)]
      file.copy(from, to)  ;unlink(from)                 
	                     }
						 }
###################################################						 
          Rlistfiles =NULL
          Hlistfiles =NULL

Hlistfiles=list.files(DirH,full.names=T)
Rlistfiles=list.files(DirR,full.names=T)
lstimgs = c(Rlistfiles,Hlistfiles)
##################################################################################
	     if(exists("mdl_NFS_SSLa_SSLp")==F){mdl_NFS_SSLa_SSLp<<-load_model_hdf5(mdlPTH_NFS_SSLa_SSLp)}
	#	  Weight = readRDS(WeightPTH_NFS_SSLa_SSLp)
	#	  set_weights(mdl_NFS_SSLa_SSLp,Weight)  		
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
	
	names(preds3) <- c("NFS","SSLadult","SSLpup") #
	preds3$link=lstimgs
for (i in 1:length(lstimgs)) {
    if(preds3$NFS[i]> preds3$SSLpup[i] & preds3$NFS[i]>preds3$SSLadult[i]){preds3$name[i]="NFS"} #
    if(preds3$SSLpup[i]> preds3$NFS[i] & preds3$SSLpup[i]> preds3$SSLadult[i]){preds3$name[i]="SSLpup"} #
	if(preds3$SSLadult[i]> preds3$NFS[i] & preds3$SSLadult[i]> preds3$SSLpup[i]){preds3$name[i]="SSLadult"}
   }
    preds3$name[preds3$name=="SSLadult" &  preds3$SSLadult < 0.90 ]="NFS" 
    preds3$name[preds3$name=="SSLpup" & preds3$SSLpup < 0.90]="NFS"
###############
SSLimgsP=NULL
SSLimgsP=preds3$link[preds3$name=="SSLpup"]
file.copy(SSLimgsP,SLLdirP); unlink(SSLimgsP)
#####
SSLimgsA=NULL
SSLimgsA=preds3$link[preds3$name=="SSLadult"]

file.copy(SSLimgsA,SLLdirA); unlink(SSLimgsA)
print("Done NFS-SSl_adult-SSL _pup filter")
#####################








