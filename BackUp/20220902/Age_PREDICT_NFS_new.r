 library(dplyr)
 library(tfdatasets)
 library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(fs)
 labelInput
 date1=substr(basename(labelInput),1,15)
 month<<-substr(basename(labelInput),5,6)
 Species
 
source("Modules/KMLwrite_function.r")
RmodelAge=NULL

DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout");H=list.files(DirH,full.names=T)
if (dir.exists(DirH)==F) {
DirR=paste0(labelInput,"\\Predict\\Age_predict\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\Age_predict\\Haulout");H=list.files(DirH,full.names=T)
}

##################################################################################
Age_Pred_Fun=function(
    Species="NFSAdult", 
	trgt_size = 256,
	vision_dimensions=256,
	batch_size=32,
	type,
	#check=F,
	month1,
	R_mdlBSAgePTH ="C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS_TF-F_VGG16\\Checkpoints\\TF_F__20220819_loss_0.13_epoch_01.h5",
	#H_mdlBSAgePTH = paste0(System_data,"/weights/SSL_AGE/SSLageBASE/Sa_P_J_An__20220127_loss_0.20_epoch_03.h5"),
	
	RookerytWeightPTH= "C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS_TF-F_VGG16\\Checkpoints\\20220824_loss_013_epoch_03"
	#HauloutWeightPTH=  paste0(System_data,"/weights/SSL_AGE/Sa_P_J_An__20220127_loss_020_epoch_03" ))
      )	{	

    
	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	date1=substr(basename(labelInput),1,15)
	PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
	
	if(is.null(RmodelAge)==T){RmodelAge<<-load_model_hdf5(R_mdlBSAgePTH)}
	#if(exists("HmodelAge")==F){HmodelAge<<-load_model_hdf5(H_mdlBSAgePTH)}
##################################################################################	
if (type=="Rookery"){		  
		  RookerytWeight=readRDS(RookerytWeightPTH)
		  set_weights(RmodelAge,RookerytWeight)
		  modelAge<-RmodelAge
	           		
	      Age_Name= c("F","TF") 
	      
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.kml")
		  pth<- paste0(labelInput,"\\Predict\\",Species,"\\Rookery")
		 listImgPred<-list.files(pth, full.names=T)
	       
  }
############################################################################
##############

if (length(listImgPred)>0) {
pred=NULL


pred_paths <- tibble::tibble(input = dir_ls(pth))
#####################
tf_read_image <-  function(path, format = "image", resize = NULL, ...) {
                          img <- path %>%
                          tf$io$read_file() %>%
                          tf$io[[paste0("decode_", format)]](...) 
                   if (!is.null(resize))
                         img <- img %>%
                         tf$image$resize(as.integer(resize)) %>%
						 tf$image$convert_image_dtype(dtype = tf$float64)
                         img
 }
img_size <- c(256, 256)
#######################################################################
tf_read_image_and_resize <- function(...,  resize = img_size)
tf_read_image(..., resize = resize)
######################################################################
     make_dataset <- function(paths_df) {
	           tensor_slices_dataset(paths_df) %>%
               dataset_map(function(path) { 
                             image <- path$input %>%
                             tf_read_image_and_resize("jpeg", channels = 3L) 
                             image
                           }) %>%
 dataset_cache() %>% 
# dataset_shuffle(buffer_size = nrow(paths_df)) %>% 
 dataset_batch(32)
}
#################################################################



pred_data_set <- make_dataset(pred_paths)
    pred=modelAge %>% predict(pred_data_set)
	pred
###############################################################################################
	preds3=data.frame(pred)
	names(preds3) <- c("F", "TF")
	lstImgDf=data.frame(listImg=basename(as.character(listImgPred)),order=c(1:length(listImgPred)))
	lstImgDf=lstImgDf[order(lstImgDf$order,decreasing=T),]
	
	
for (i in 1:length(listImgPred)) {
   if(preds3$F[i]> preds3$TF[i]){preds3$name[i]="F"}
   if(preds3$F[i]< preds3$TF[i]){preds3$name[i]="TF"}
   }
   
   preds3$link=basename(lstImgDf$listImg)
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))
      #preds3$link=basename(as.character(preds3$imgPth))
      TblGAg <- merge(x=preds3,y=TblAgeRef,by="link",all.x=T)

      TableGeoAge1 <- data.frame(pth_save_img=TblGAg$pth_save_img, lon=TblGAg$lat,lat=TblGAg$lon,age= TblGAg$name)
	  TableGeoAge2=TableGeoAge1 %>% filter(age != "P")
  
      write.csv(TableGeoAge2,PTH_TableGeoAge)
	  KMLwrite(Img3=TableGeoAge2,kmlPathSave)
}
}

#if (length(H) !=0){Age_Pred_Fun(type="Haulout",month1=month); print("Haulout")}
if (length(R) !=0){Age_Pred_Fun(type="Rookery",month1=month); print("Rookery")}

