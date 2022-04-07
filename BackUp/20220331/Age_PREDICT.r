 library(dplyr)
 
 labelInput
 date1=substr(basename(labelInput),1,15)
 Species
 
source("Modules/KMLwrite_function.r")
date1=substr(basename(labelInput),1,15)

DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout");H=list.files(DirH,full.names=T)
if (dir.exists(DirH)==F) {
DirR=paste0(labelInput,"\\Predict\\Age_predict\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\Age_predict\\Haulout");H=list.files(DirH,full.names=T)
}

##################################################################################
Age_Pred_Fun=function(
    Species="SSLAdult",           
	trgt_size = 256,
	type,
	#check=F,
	R_mdlBSAgePTH = paste0(System_data,"/weights/SSL_AGE/SSLageBASE/J_F_An_Jan26 2022_loss_0.09_epoch_04.h5"),
	H_mdlBSAgePTH = paste0(System_data,"/weights/SSL_AGE/SSLageBASE/Sa_P_J_An__20220127_loss_0.20_epoch_03.h5"),
	
	RookerytWeightPTH= paste0(System_data,"/weights/SSL_AGE/J_F_An_Jan26 2022_loss_009_epoch_04"),
	HauloutWeightPTH=  paste0(System_data,"/weights/SSL_AGE/Sa_P_J_An__20220127_loss_020_epoch_03" )) {	

    
	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	library(EBImage)
	
	date1=substr(basename(labelInput),1,15)
	PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
	
	if(exists("RmodelAge")==F){RmodelAge<<-load_model_hdf5(R_mdlBSAgePTH)}
	if(exists("HmodelAge")==F){HmodelAge<<-load_model_hdf5(H_mdlBSAgePTH)}
##################################################################################	
if (type=="Rookery"){		  
		  RookerytWeight=readRDS(RookerytWeightPTH)
		  set_weights(RmodelAge,RookerytWeight)
		  modelAge<-RmodelAge
	      Age_Name= c("TF","F", "J")      		
          
	      
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.kml")
		  pth<- paste0(labelInput,"\\Predict\\",Species,"\\Rookery")
		 listImgPred<-list.files(pth, full.names=T)
	       
  }
############################################################################
if (type=="Haulout"){  
		  HauloutWeight=readRDS(HauloutWeightPTH)
		  set_weights(HmodelAge,HauloutWeight)
		  modelAge<-HmodelAge
	      Age_Name= c("AN","J","P", "SA")      		 
	    #  PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.kml")
		  pth<- paste0(labelInput,"\\Predict\\",Species,"\\Haulout")
		  listImgPred<-list.files(pth, full.names=T)
	      
  }
############################################################################
#if (type=="F_Sa"){  # WE NEED ONLY FOUND SA AMONG F
#		  F_Sa_model=load_model_hdf5( "E:\\YANDEX DISC\\YandexDisk\\System data\\weights\\SSL_AGE\\SSLageBASE\\F_Sa__20220128_loss_0.32_epoch_01.h5")
#		  F_Sa_Weight=readRDS( "E:\\YANDEX DISC\\YandexDisk\\System data\\weights\\SSL_AGE\\F_Sa__20220128_loss_032_epoch_01")
#		  set_weights(F_Sa_model,F_Sa_Weight)
#		  modelAge<-F_Sa_model
#	      Age_Name= c("F", "Sa")
#	    #  PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
#	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_F_Sa.csv")
#          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_F_Sa.kml")
		  
#		  H_PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.csv")
#		  R_PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
#		  H_TableGeoAge=read.csv(H_PTH_TableGeoAge)
#		  R_TableGeoAge=read.csv(R_PTH_TableGeoAge)
#		  TableGeoAge1=rbind(H_TableGeoAge,R_TableGeoAge)
		  
		  
#		  listImgPred <- TableGeoAge1 %>% filter(age == "F")  %>% select (pth_save_img)  # exlude SA among F
#		  listImgPred<-listImgPred$pth_save_img
		
		 		 	       
 # }
##############################################################################
 imageRead <- function(image_file,
                          target_width = trgt_size, 
                          target_height = trgt_size) {
      img <- image_read(image_file)
	  img=image_flop(img)
	  img=image_rotate(img,270)
      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
      result <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
      dim(result) <- c(1, target_width, target_height, 3)
   
	  

   #   img=readImage(image_file)
#	  img=resize(img,target_width,target_height)
#	  result=imageData(img)
#	  dim(result) <- c(1, target_width, target_height, 3)
	  
	   return(result)
	  
	  
	  
	  
	}
##############

if (length(listImgPred)>0) {
finWrite=NULL
for (u in 1: length(listImgPred)) {
  imgPth=listImgPred[u]
   img_tensor=imageRead(imgPth)
  preds = c(modelAge %>% predict(img_tensor))
  name= Age_Name  
  position=c(1:length(name))
  result=data.frame(preds,name)
  result=result[order(-preds),]
  result<-data.frame(result,position)
  result=result[1,]
  result=cbind(result,imgPth)
  finWrite=rbind(finWrite,result)
}
} 
preds3<-finWrite
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))
      preds3$link=basename(as.character(preds3$imgPth))
      TblGAg <- merge(x=preds3,y=TblAgeRef,by="link",all.x=T)

      TableGeoAge1 <- data.frame(pth_save_img=TblGAg$pth_save_img, lon=TblGAg$lat,lat=TblGAg$lon,age= TblGAg$name)
	  TableGeoAge2=TableGeoAge1 %>% filter(age != "P")
  
      write.csv(TableGeoAge2,PTH_TableGeoAge)
	  KMLwrite(Img3=TableGeoAge2,kmlPathSave)
##############################################################################################
#if (check==T){
#	  dirSave=paste0(labelInput,"\\Predict\\Age_check");unlink(dirSave)
#      dir.create(dirSave,showWarnings = F)	  
#for (i in 1:length(preds3[,1])) {
#row1=preds3[i,]
#from=row1$imgPth
#folder=row1$name
#to=paste0(dirSave,"\\",folder)
#dir.create(to,showWarnings = F)
#file.copy(from,to)
#}
#}
#########################################
}

if (length(H) !=0){Age_Pred_Fun(type="Haulout"); print("Haulout")}
if (length(R) !=0){Age_Pred_Fun(type="Rookery"); print("Rookery")}





 rkPTH = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
 hPth = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.csv")
 PthPup=paste0(labelInput,"\\Predict\\SSLPup_",date1,".csv")

 
 kmlPathSave=paste0(labelInput,"\\Predict\\","SSLAdult", "_", date1, ".kml")

rk=NULL
Hl=NULL
Pup=NULL
adults1=NULL

if (length(R) !=0){rk=read.csv(rkPTH)}
if (length(H) !=0){Hl=read.csv(hPth)}
if (file.exists(PthPup)){P=read.csv(PthPup);Pup1=data.frame(lat =as.numeric(P$lat), lon=as.numeric(P$lon),  age=P$age)
                         Pup= Pup1[!Pup1$age %in% c("SmallError"),]; Pup1$age="P"
      }

ad=rbind(rk,Hl)
if(is.null(ad)==F){adults1=data.frame(lat =as.numeric(ad$lat),     lon=as.numeric(ad$lon),  age=ad$age)}


PointsToWrite=rbind(adults1,Pup)


if (is.null(PointsToWrite)==F) {KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)}
