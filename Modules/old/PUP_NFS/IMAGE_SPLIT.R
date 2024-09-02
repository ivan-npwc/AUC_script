if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
library(tools)
library(EBImage)

 labelInput  
 date1=substr(basename(labelInput),1,15)
 
 BigImgDir=paste0(labelInput, "\\", "Predict", "\\","Haulout")
 SaveImgDir=paste0(labelInput, "\\", "Predict", "\\","PUP")


 if (dir.exists(SaveImgDir)==F) {dir.create(SaveImgDir)}

 listBigImgs=list.files(BigImgDir,full.names=T)


 
 cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
   
    clusterEvalQ(cl, {
    library(tools)
     library(EBImage)	 
    })
    registerDoParallel(cl)
  foreach(i = 1:length(listBigImgs)) %dopar% {	
 #    for (i in 1: length(listBigImgs)) {
 
 
 Crop1=NULL
 Crop2=NULL
 Crop3=NULL
 Crop4=NULL
  
 
  imgP=listBigImgs[i]
  Nm= file_path_sans_ext(basename(imgP))
  BgImg=readImage(imgP)
  

  Crop1=BgImg[128:639,128:639,]
  Crop2=BgImg[384:895,128:639,]
  Crop3=BgImg[128:639,384:895,]
  Crop4=BgImg[384:895,384:895,]
  

  pthSave1=paste0(SaveImgDir,"\\",Nm,"#1.jpg")
  pthSave2=paste0(SaveImgDir,"\\",Nm,"#2.jpg")
  pthSave3=paste0(SaveImgDir,"\\",Nm,"#3.jpg")
  pthSave4=paste0(SaveImgDir,"\\",Nm,"#4.jpg")
  
  writeImage(Crop1,pthSave1)
  writeImage(Crop2,pthSave2)
  writeImage(Crop3,pthSave3)
  writeImage(Crop4,pthSave4)
  
  }
  stopCluster(cl)
