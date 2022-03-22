    library(dplyr)

	library(EBImage)
    library(keras)
	library(magick) 
	library(EBImage)

##################################################################################
    labelInput
    Species="SSLAdult"         
	trgt_size = 256
	mdlBSAgePTH ="C:\\Users\\usato\\Documents\\YandexDisk\\System data\\weights\\FPage\\TruePositive_FalsePositive__20220321_loss_0.05_epoch_04.h5"# paste0(System_data,"/weights/FPage/TruePositive_FalsePositive__20220321_loss_0.05_epoch_04.h5")
    WeightPTH= "C:\\Users\\usato\\Documents\\YandexDisk\\System data\\weights\\FPage\\TruePositive_FalsePositive__20220321_loss_005_epoch_04"#paste0(System_data,"/weights/FPage/TruePositive_FalsePositive__20220321_loss_005_epoch_04"),
	date1=substr(basename(labelInput),1,15)
    Dir=paste0(labelInput,"\\Predict\\Age_predict");
	ListImgs=list.files(Dir,full.names=T,recursive=TRUE)
 FPdir=paste0(Dir,"\\FP"); dir.create(FPdir)
	if(exists("modelFP")==F){modelFP<<-load_model_hdf5(mdlBSAgePTH)}
##################################################################################	  
		  Weight=readRDS(WeightPTH)
		  set_weights(modelFP,Weight)
	      Age_Name= c("FALSE","TRUE")      		
			       
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
if (length(ListImgs)>0) {
finWrite=NULL
for (u in 1: length(ListImgs)) {
  imgPth=ListImgs[u]
   img_tensor=imageRead(imgPth)
  preds = c(modelFP %>% predict(img_tensor))
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
FPt=finWrite[finWrite$name=="FALSE",]
#################################################################################################
for (i in 1:length(FPt$imgPth)){
imgPth=FPt$imgPth[i]
file.copy(imgPth,FPdir)
unlink(imgPth)
}







