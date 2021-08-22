source("Modules/KMLwrite_function.r")
#source("C:/SSL_DB/AUC_script/Modules/KMLwrite_function.r")
##################################################################################
Age_Pred_Fun=function(
  #  labelInput, #"C:\\SSL_DB\\2020_138_OPP\\20200612_143112"
    Species="SSLAdult",           
	batch_size = 64,
	trgt_size = 256,
	type,
	check=F,
	modelBASEAgePTH="System data/weights/SSL AGE/ROOKERY SSLAdult_Feb 19  2021_Val_0.89_epoch_01.h5",
	RookerytWeightPTH =    "System data/weights/SSL AGE/SSLageROOKERYweight",
	HauloutWeightPTH =    "System data/weights/SSL AGE/SSLageHAULOUTweight" ) {	
	#"C:\\SSL_DB\\AUC_script\\System data\\weights\\SSL AGE\\SSLageHAULOUTweight"
	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	if(exists("modelAge")==F){modelAge=load_model_hdf5(modelBASEAgePTH)}
	
if (type=="Rookery"){
     #     modelAgePTH =    "C:\\SSL_DB\\SSL_age\\Checkpoints\\SSLAdult_Feb 19  2021_Val_0.89_epoch_01.h5"
		 # modelAge=load_model_hdf5(modelAgePTH)
		  
		  RookerytWeight=readRDS(RookerytWeightPTH)
		  set_weights(modelAge,RookerytWeight)
	      Age_Name= c("TF","F", "J")      		
          pth<<- paste0(labelInput,"\\Predict\\Age_predict\\Rookery")
	      PthTblAgeRef=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"AgeRef.csv")
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_ROOKERY.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_ROOKERY.kml")
	     # ProbAge_PTH=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_ProbAge.csv")      
  }
############################################################################
if (type=="Haulout"){
      #  modelAgePTH =    "C:\\SSL_DB\\SSL_age\\HAULOUT\\Checkpoints\\SSLAdult_Feb 22  2021_Val_0.92_epoch_02.h5"
		# modelAge=load_model_hdf5(modelAgePTH)
	     
		  HauloutWeight=readRDS(HauloutWeightPTH)
		  set_weights(modelAge,HauloutWeight)
	      Age_Name= c("An","J", "Sa")      		
          pth<<- paste0(labelInput,"\\Predict\\Age_predict\\Haulout")
	      PthTblAgeRef=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"AgeRef.csv")
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_HAULOUT.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_HAULOUT.kml")
	     # ProbAge_PTH=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"_ProbAge.csv")    
  }
############################################################################
 imageRead <- function(image_file,
                          target_width = trgt_size, 
                          target_height = trgt_size) {
      img <- image_read(image_file)
	  img=image_flop(img)
	  img=image_rotate(img,270)
      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
      result <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
      dim(result) <- c(1, target_width, target_height, 3)
      return(result)
	}
##############
listImgPred<<-list.files(pth)
if (length(listImgPred)>0) {
finWrite=NULL
for (u in 1: length(listImgPred)) {
  imgPth=paste0(pth, "\\", listImgPred[u])
   img_tensor=imageRead(imgPth)
  preds = c(modelAge %>% predict(img_tensor))
  name= Age_Name  
  position=c(1:length(name))
  result=data.frame(preds,name)
  result=result[order(-preds),]
  result<<-data.frame(result,position)
  result=result[1,]
  result=cbind(result,imgPth)
  finWrite=rbind(finWrite,result)
}
} 
preds3<<-finWrite
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))
	  
      preds3$link=basename(as.character(preds3$imgPth))
      TableGeoAge <<- merge(x=preds3,y=TblAgeRef,by="link",all=T)
	  TableGeoAge=TableGeoAge[is.na(TableGeoAge$name)==F,]
	  
      TableGeoAge1 <<- data.frame(lon=TableGeoAge$lat,lat=TableGeoAge$lon,age= TableGeoAge$name)
  
      write.csv(TableGeoAge1,PTH_TableGeoAge)
	  KMLwrite(Img3=TableGeoAge1,kmlPathSave)
##############################################################################################
if (check==T){
	  dirSave=paste0(labelInput,"\\Predict\\Age_check");unlink(dirSave)
      dir.create(dirSave,showWarnings = F)	  
for (i in 1:length(preds3[,1])) {
row1=preds3[i,]
from=row1$imgPth
folder=row1$name
to=paste0(dirSave,"\\",folder)
dir.create(to,showWarnings = F)
file.copy(from,to)
}
}
#########################################
}
Age_Pred_Fun(type="Haulout")
Age_Pred_Fun(type="Rookery")
