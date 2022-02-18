library(magick)
library(tools)

if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
#split into 4 images from one 

labelInput    #=  "D:\\AUC_data\\SSL_Pup"

imgDirFROM=    paste0(labelInput,"\\Image")
maskDirFROM=   paste0(labelInput,"\\Mask")

imgDirTo=    paste0(labelInput,"\\Image_smal");dir.create(imgDirTo,showWarnings=F)
maskDirTo=   paste0(labelInput,"\\Mask_smal");dir.create(maskDirTo,showWarnings=F)


listBigMsk=list.files(maskDirFROM, full.names=T)




cl <- makePSOCKcluster(detectCores (logical=FALSE)) 

clusterEvalQ(cl, {
  library(magick)	 
  library(tools)
})
registerDoParallel(cl)
foreach(i = 1:length(listBigMsk)) %dopar% {	
  #    for (i in 1: length(listBigMsk)) {
  
  
  Crop1=NULL
  Crop2=NULL
  Crop3=NULL
  Crop4=NULL
  
  
  mskP=listBigMsk[i]
  Nm= file_path_sans_ext(basename(mskP))
  ############################################# mask
  BgMsk=readImage(mskP)
  
  
  CropM1=BgMsk[128:639,128:639,]
  CropM2=BgMsk[384:895,128:639,]
  CropM3=BgMsk[128:639,384:895,]
  CropM4=BgMsk[384:895,384:895,]
  
  
  pthSave1=paste0(maskDirTo,"\\",Nm,"#1.png")
  pthSave2=paste0(maskDirTo,"\\",Nm,"#2.png")
  pthSave3=paste0(maskDirTo,"\\",Nm,"#3.png")
  pthSave4=paste0(maskDirTo,"\\",Nm,"#4.png")
  
  writeImage(CropM1,pthSave1)
  writeImage(CropM2,pthSave2)
  writeImage(CropM3,pthSave3)
  writeImage(CropM4,pthSave4)
  
  ############################################## 
  ################################################## image
  imgP=paste0(imgDirFROM,"\\",Nm,".jpg")
  
  BgImg=readImage(imgP)
  
  
  Crop1=BgImg[128:639,128:639,]
  Crop2=BgImg[384:895,128:639,]
  Crop3=BgImg[128:639,384:895,]
  Crop4=BgImg[384:895,384:895,]
  
  
  pthSave1=paste0(imgDirTo,"\\",Nm,"#1.jpg")
  pthSave2=paste0(imgDirTo,"\\",Nm,"#2.jpg")
  pthSave3=paste0(imgDirTo,"\\",Nm,"#3.jpg")
  pthSave4=paste0(imgDirTo,"\\",Nm,"#4.jpg")
  
  writeImage(Crop1,pthSave1)
  writeImage(Crop2,pthSave2)
  writeImage(Crop3,pthSave3)
  writeImage(Crop4,pthSave4)
  
  
}

stopCluster(cl)
