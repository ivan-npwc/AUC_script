    library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	
	labelInput
    Species  #= "LRG"          
	trgt_size = 256
    modelREGpth <<-   paste0(System_data, "\\weights\\",LRGH_MSRMNTS)
	ImgsPth= paste0(labelInput,"\\Predict\\",Species,"_Measurements\\Image")
    pthFinWrite= paste0(labelInput,"\\Predict\\",Species,"_Measurements\\Predict_Measurements.csv")
	
	
    date1=substr(basename(labelInput),1,15)
	listImgPred<<-list.files(ImgsPth,full.names=T)
	
	if(exists("model_regresion")==F){model_regresion=load_model_hdf5(modelREGpth)}
	

############################################################################
 imageRead <- function(image_file,
                          target_width = trgt_size, 
                          target_height = trgt_size) {
      img <- image_read(image_file)
	 # img=image_flop(img)
	 # img=image_rotate(img,270)
      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
      result <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
      dim(result) <- c(1, target_width, target_height, 3)
      return(result)
	}
##################################
if (length(listImgPred)>0) {
finWrite=NULL
for (u in 1: length(listImgPred)) {
  imgPth=listImgPred[u]
   img_tensor=imageRead(imgPth)
  preds = c(model_regresion %>% predict(img_tensor))
  
  result=data.frame(imgPth,preds)
  finWrite=rbind(finWrite,result)
}
} 
write.csv(finWrite,pthFinWrite, row.names=F)
#################################################################################################

for (i in 1:length(listImgPred)) {

oldName=listImgPred[i]
pred=round(finWrite$preds[finWrite$imgPth==listImgPred[i]])
newNmae=paste0(ImgsPth,"\\",pred,"#",basename(oldName))
file.rename(oldName,newNmae)

}












