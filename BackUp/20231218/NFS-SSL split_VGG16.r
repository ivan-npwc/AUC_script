
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
		  
	      mdlPTH_NFS_SSLa_SSLp = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/SSLpSort_SSLa_256_NFS_256__20220924_loss_0.03_epoch_09.h5")
		  mdlPTH_NFS_SSLa = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/NFS-SSL__20220902_loss_0.19_epoch_02.h5")
		  
	      #WeightPTH_NFS_SSLa_SSLp =  paste0(System_data,"/weights/NFS_AGE/NFS_SSLad_SSL_pup-val92")
		  WeightPTH_NFS_SSladult =  paste0(System_data,"/weights/NFS_AGE/NFS-SSL_vgg16")
		  date1=substr(basename(labelInput),1,15)
      
		  PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
		
          DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery")
          DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout")
	
		  SLLdirA=paste0(labelInput,"\\Predict\\",Species,"\\SSLa"); unlink(SLLdirA,recursive=T);dir.create(SLLdirA,showWarnings = F)
          SLLdirP=paste0(labelInput,"\\Predict\\",Species,"\\SSLp");  unlink(SLLdirP,recursive=T);dir.create(SLLdirP,showWarnings = F)	
		  SLLdirA2=paste0(labelInput,"\\Predict\\",Species,"\\SSLa2");  unlink(SLLdirA2,recursive=T);dir.create(SLLdirA2,showWarnings = F)
		  
		  DirH_256 = paste0(labelInput,"\\Predict\\",Species,"\\Haulout_256");unlink(DirH_256,recursive=T);dir.create(DirH_256)
		  DirR_256 = paste0(labelInput,"\\Predict\\",Species,"\\Rookery_256");unlink(DirR_256,recursive=T);dir.create(DirR_256)

################################################################################################    if function use again with new models
        TblAgeRef=read.csv(PthTblAgeRef)
		SSLimgsA=NULL;SSLimgsP=NULL;SSLimgsA2=NULL
		SSLimgsA=list.files(SLLdirA,full.names=T);SSLimgsP=list.files(SLLdirP,full.names=T);SSLimgsA=list.files(SLLdirA2,full.names=T)
		lstImgsErr=c(SSLimgsA,SSLimgsP,SSLimgsA)
if (length(lstImgsErr)!=0){
    TblAgeRef1=TblAgeRef[basename(TblAgeRef$pth_save_img) %in% basename(lstImgsErr),]
	 for (i in 1:length(TblAgeRef1$pth_save_img)){
    to=TblAgeRef1$pth_save_img[i]
    from= lstImgsErr[basename(lstImgsErr)==basename(to)]
      file.copy(from, to);unlink(from)                 
	                     }
						 }
###############################################
####################################################

  cl <- makePSOCKcluster(detectCores (logical=FALSE)-1) 
  clusterEvalQ(cl, {
	library(EBImage)
	})
  registerDoParallel(cl)

  listImgs=list.files(DirH, full.names=T)
  #  for (u in 1:length(listImgs)) {
	foreach(u = 1:length(listImgs)) %dopar% {	    	
          pth_img= paste0(listImgs [u])
		  pth_save_img= paste0(DirH_256,"\\",basename(pth_img))
          img = readImage(pth_img)        
         img_crop = img[128:383, 128:383,1:3]
	   writeImage(img_crop, pth_save_img, quality = 85)	
}

  listImgs=list.files(DirR, full.names=T)
  if (length(listImgs)>1){
  #  for (u in 1:length(listImgs)) {
	foreach(u = 1:length(listImgs)) %dopar% {	    	
          pth_img= paste0(listImgs [u])
		  pth_save_img= paste0(DirR_256,"\\",basename(pth_img))
          img = readImage(pth_img)        
         img_crop = img[128:383, 128:383,1:3]
	   writeImage(img_crop, pth_save_img, quality = 85)	
}
}
####################################################					 
###################################################						 
          Rlistfiles =NULL
          Hlistfiles =NULL

Hlistfiles=list.files(DirH_256,full.names=T) #DirH
#Rlistfiles=list.files(DirR_256,full.names=T) #DirR
lstimgs = c(Rlistfiles,Hlistfiles)
##################################################################################
	     if(exists("mdl_NFS_SSLa_SSLp")==F){mdl_NFS_SSLa_SSLp<<-load_model_hdf5(mdlPTH_NFS_SSLa_SSLp)}
		#  Weight = readRDS(WeightPTH_NFS_SSLa_SSLp)
		#  set_weights(mdl_NFS_SSLa_SSLp,Weight)  		
############################################################################
  data <- tibble::tibble(img = lstimgs)
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	 
	 dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img))
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)
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
    preds3$name[preds3$name=="SSLadult" &  preds3$SSLadult < 0.99]="NFS" 
    preds3$name[preds3$name=="SSLpup" & preds3$SSLpup < 0.75]="NFS"
#####################################################

######################################################
SSLimgsP=NULL
SSLimgsP=preds3$link[preds3$name=="SSLpup"]

SSLimgsP=gsub("Rookery_256","Rookery",SSLimgsP)
SSLimgsP=gsub("Haulout_256","Haulout",SSLimgsP)


file.copy(SSLimgsP,SLLdirP); unlink(SSLimgsP)
#
SSLimgsA=NULL
SSLimgsA=preds3$link[preds3$name=="SSLadult"]

SSLimgsA=gsub("Rookery_256","Rookery",SSLimgsA)
SSLimgsA=gsub("Haulout_256","Haulout",SSLimgsA)




file.copy(SSLimgsA,SLLdirA); unlink(SSLimgsA)
print("Done NFS-SSl_adult-SSL _pup filter")
###############################################################################################################################################################
#############################################################################################################################################################
#################################################################################################################################################
Rlistfiles =NULL
Hlistfiles =NULL

DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery")#;Rlistfiles=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout");Hlistfiles=list.files(DirH,full.names=T)
if (dir.exists(DirH)==F) {
DirR=paste0(labelInput,"\\Predict\\Age_predict\\Rookery")#;Rlistfiles=list.files(DirR,full.names=T)
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



file.copy(SSLimgs,SLLdirA2); unlink(SSLimgs)
print("Done NFS-SSl filter")
####################################################################################### 









