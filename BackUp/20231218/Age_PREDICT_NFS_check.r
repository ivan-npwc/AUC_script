 library(dplyr)
 library(tfdatasets)
 library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
 labelInput
 date1=substr(basename(labelInput),1,15)
 month<<-substr(basename(labelInput),5,6)
 Species
 
source("Modules/KMLwrite_function.r")


DirR=paste0(labelInput,"\\Predict\\",Species,"_check\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"_check\\Haulout");H=list.files(DirH,full.names=T)


##################################################################################
Age_Pred_Fun=function(
    Species="NFSAdult", 
	vision_dimensions=256,
	batch_size=64,
	type,
	Age_Name,
	listImgPred,
	R_mdlBSAgePTH = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/TF_F_An_Bch_20220819_loss_0.13_epoch_01.h5"),
	H_mdlBSAgePTH =paste0(System_data,"/weights/NFS_AGE/NFSageBASE/TF_F_An_Bch_20220819_loss_0.13_epoch_01.h5"),
	
	RookerytWeightPTH= paste0(System_data,"/weights/NFS_AGE/NFS_age_TF-F-20220824_loss_013_epoch_03" ),
	HauloutWeightPTH=  paste0(System_data,"/weights/NFS_AGE/NFS_age__AN-Bch_20220826_loss_026_epoch_05" ))	{	

    
	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	date1=substr(basename(labelInput),1,15)
	PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_check_AgeRef.csv")
	
	
	if(exists("RmodelAge")==F){RmodelAge<<-load_model_hdf5(R_mdlBSAgePTH)}
	if(exists("HmodelAge")==F){HmodelAge<<-load_model_hdf5(H_mdlBSAgePTH)}
##################################################################################	
if (type=="Rookery"){		  
		  RookerytWeight=readRDS(RookerytWeightPTH)
		  set_weights(RmodelAge,RookerytWeight)
		  modelAge<-RmodelAge   		
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY_check.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY_check.kml")       
  }
############################################################################
if (type=="Haulout"){  
		  HauloutWeight=readRDS(HauloutWeightPTH)
		  set_weights(HmodelAge,HauloutWeight)
		  modelAge<-HmodelAge	 
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT_check.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT_check.kml")    
  }
############################################################################
  data <- tibble::tibble(img = listImgPred)
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
    pred=modelAge %>% predict(pred_data_set)
	
###############################################################################################

    preds3=data.frame(pred)
	names(preds3) <- Age_Name
	preds3$link=basename(as.character(listImgPred))
for (i in 1:length(listImgPred)) {
   if(preds3[,1][i]> preds3[,2][i]){preds3$name[i]=Age_Name[1]}
   if(preds3[,1][i]< preds3[,2][i]){preds3$name[i]=Age_Name[2]}
   }
 ######################################
 if (type=="Haulout"){preds3$name[preds3$AN < 0.83]="Bch"}
 if (type=="Rookery"){preds3$name[preds3$TF < 0.65]="F"}
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))

      TblGAg <- merge(x=preds3,y=TblAgeRef,by="link",all.x=T)

      TableGeoAge1 <- data.frame(pth_save_img=TblGAg$pth_save_img, lon=TblGAg$lat,lat=TblGAg$lon,age= TblGAg$name)
	  TableGeoAge2=TableGeoAge1 %>% filter(age != "P")
  
      write.csv(TableGeoAge2,PTH_TableGeoAge)
	  KMLwrite(Img3=TableGeoAge2,kmlPathSave)

}
##################################
if (length(H) !=0){Age_Pred_Fun(type="Haulout",Age_Name= c("AN", "Bch"),listImgPred=H); print("Haulout")}
if (length(R) !=0){Age_Pred_Fun(type="Rookery", Age_Name= c("F","TF"), listImgPred=R); print("Rookery")}
#########################################################################################

 rkPTH = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY_check.csv")
 hPth = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT_check.csv")
 PthPup=paste0(labelInput,"\\Predict\\SSLPup_",date1,".csv")

 
 kmlPathSave=paste0(labelInput,"\\Predict\\","NFSAdult_check_", date1, ".kml")

rk=NULL
Hl=NULL
Pup=NULL
adults1=NULL

if (length(R) !=0){rk=read.csv(rkPTH)}
if (length(H) !=0){Hl=read.csv(hPth)}


ad=rbind(rk,Hl)
if(is.null(ad)==F){adults1=data.frame(lat =as.numeric(ad$lat), lon=as.numeric(ad$lon),  age=ad$age)}
PointsToWrite=rbind(adults1,Pup)


if (is.null(PointsToWrite)==F) {KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)}
