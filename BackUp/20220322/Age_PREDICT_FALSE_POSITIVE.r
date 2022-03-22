    library(dplyr)
    library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	library(EBImage)
    source("Modules/KMLwrite_function.r")


##################################################################################
    labelInput
    Species="SSLAdult"         
	trgt_size = 256
	mdlBSAgePTH = paste0(System_data,"/weights/FPage/TruePositive_FalsePositive__20220321_loss_0.05_epoch_04.h5")
    WeightPTH= paste0(System_data,"/weights/FPage/TruePositive_FalsePositive__20220321_loss_005_epoch_04"),
	date1=substr(basename(labelInput),1,15)
    Dir=paste0(labelInput,"\\Predict\\Age_predict");
	ListImgs=list.files(Dir,full.names=T)
	PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
	
	if(exists("modelFP")==F){modelFP<<-load_model_hdf5(mdlBSAgePTH)}
##################################################################################	
	  
		  Weight=readRDS(WeightPTH)
		  set_weights(modelFP,Weight)
		  modelAge<<-modelFP
	      Age_Name= c("TRUE","FALSE")      		
          
	      
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.kml")
		  pth<- paste0(labelInput,"\\Predict\\Age_predict")
		 listImgPred<-list.files(pth, full.names=T,recursive=TRUE)
	       
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

  
      write.csv(TableGeoAge2,PTH_TableGeoAge)
	  KMLwrite(Img3=TableGeoAge2,kmlPathSave)
##############################################################################################
KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)
