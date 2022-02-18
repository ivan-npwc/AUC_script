library(dplyr)


IncludeEmptyImgs=F
selectedAge="CU_P"
Species
labelInput

library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(EBImage)
   Species
  date1=substr(basename(labelInput),1,15)

HauloutDir=paste0(labelInput,"\\Predict\\Haulout")
MaskImgDir=paste0(labelInput,"\\", "Mask_Image"); if (dir.exists(MaskImgDir)== F ) {dir.create(MaskImgDir)}
maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask") ;if (dir.exists(maskDir)== F ) {dir.create(maskDir)}
imgDir=paste0(labelInput,"\\Mask_Image\\Image");if (dir.exists(imgDir)== F ) {dir.create(imgDir)}
TablePoints=read.csv(paste0(labelInput, "\\",date1, "Points.csv"))
pthImg=paste0(labelInput,"\\",date1, "_CountDist.csv")
TableImg=read.csv(pthImg)
######################################################################################################## CREATE MASK WITH POINTS
TablePoints=TablePoints %>% filter(sex== selectedAge)

mskList=list.files(maskDir)
NeedImgs=gsub("png","jpg",mskList)
NeedImgs1=paste0(HauloutDir,"\\",NeedImgs)
file.copy(NeedImgs1,imgDir)
 
#############################################################################  CONVERT MSK FOR WLRS
if (Species =="WLRS") {
listMsk=list.files(maskDir,full.names=T)
 for (i in 1: length(listMsk)) {
Msk= readImage(listMsk[i])
 Msk=1-Msk
 writeImage(Msk, listMsk[i])
}
}
#################################################################   IMGS MSK WITHOUT ANIMALS
  if (IncludeEmptyImgs==T) {
  
 
  ListImgsHaulout=list.files(HauloutDir)
  Presence=list.files(maskDir)
  ImgPres=list.files(imgDir)
  NeedImgs=ListImgsHaulout[!(ListImgsHaulout %in% ImgPres)]
  NeedImgs1=paste0(HauloutDir,"\\",NeedImgs)
  file.copy(NeedImgs1,imgDir)
  
  ListEmptyMsk=gsub("jpg","png",NeedImgs)
  
  foreach(y = 1:length(ListEmptyImgs)) %dopar% {	
   # for (y in 1:length(ListEmptyImgs)){
	   img = ListEmptyMsk[y]
       fig=image_blank(1024,1024,color = "black")
	   pathImgSave=paste0(maskDir, "\\",img)
       pathImgSave=gsub("jpg","png",pathImgSave)
       image_write(fig, path = pathImgSave, format = "png")   
    }
	
	
	}
 

 

  
  
  

