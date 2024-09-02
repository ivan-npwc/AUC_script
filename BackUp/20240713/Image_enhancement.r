 
 #library("reticulate")}
 library("parallel")
 library("doParallel")
# library("foreach")}
 library(EBImage)
 library(magick)
 
  labelInput="H:\\2024_138_OPP\\20240622_085946"
  Species="NFSAdult"
  
  imgDir=paste0(labelInput,"\\Predict\\Haulout")
  SaveDir=paste0(labelInput,"\\Predict\\Image_enhancement") 
  
  
  dir.create(SaveDir, showWarnings=F)
  imgList=list.files(imgDir,full.names=T,recursive=T,include.dirs=F)
  
  cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
   
    clusterEvalQ(cl, {
      library(EBImage)
      library(magick)	  
    })
    registerDoParallel(cl)
	
  foreach(i = 1:length(imgList)) %dopar% {	

  imgPTH=imgList[i]
  savePTH=paste0(SaveDir,"\\",basename(imgPTH))
  img=image_read(imgPTH)
  img1=image_modulate(img, brightness = 150, saturation = 150, hue = 100)
  image_write(img1,savePTH)
  
  }
  
  stopCluster(cl)