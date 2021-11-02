    library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	
	labelInput
    Species="LRG"          
	trgt_size = 256
    modelPTH <<-   ""
	
    date1=substr(basename(labelInput),1,15)
	pth= "D:\\PL_DB\\2021_3101_OPP\\20210725_084310\\Predict\\LRG_Measurements\\Image"
	listImgPred<<-list.files(pth,full.names=T)
	
	if(exists("model_regresion")==F){model_regresion=load_model_hdf5(modelPTH)}
	

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

for (i in 1:length(finWrite$preds)) {
preds=as.numeric(finWrite$preds[i])
real=as.numeric(strsplit(basename(finWrite$imgPth[i]),"_")[[1]][1])

finWrite$Correction[i]=real/preds

}


#################################################################################################
	 
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

