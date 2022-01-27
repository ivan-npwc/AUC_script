library(EBImage)

labelInput


ImgMskDir=paste0(labelInput,"\\Mask_Image")


  if(dir.exists(ImgMskDir)==F) {stop("No Images-Masks found")}
  MskDir= paste0(ImgMskDir,"\\Mask")
  ImgDir=paste0(ImgMskDir,"\\Image")
  SaveDir=paste0(ImgMskDir,"\\BlobMask")
  if (dir.exists(SaveDir)==F) {dir.create(SaveDir)}
  listMsk=list.files(MskDir,full.names=T)
  
  
  

library(parallel)
library(doParallel)
library(foreach)

cl <- makePSOCKcluster(detectCores (logical=F)-1) 
clusterEvalQ(cl, {
library(magick)
library(EBImage)
})	
registerDoParallel(cl)
  
  
  foreach(i = 1:length(listMsk)) %dopar% {
 # for (i in 1:length(listMsk)) {
    pthMsk=listMsk[i]
    pthImg=paste0(ImgDir,"\\",basename(pthMsk))
    pthImg=gsub("png","jpg",pthImg)
    pthSave=paste0(SaveDir,"\\",basename(pthImg))
    Msk=readImage(pthMsk)
    Img=readImage(pthImg)
    Msk1 = getFrame(Msk, 1)
    Msk2 = thresh(Msk1, 18, 18, 0.009)  
    #  Msk2 <- fillHull(Msk2)
    #  Msk2 = opening(Msk2, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
    #  Msk2 = fillHull(Msk2)
    Msk3= dilate(Msk2, kern=makeBrush(251, shape='disc'))
    Msk4= erode(Msk3, kern=makeBrush(151, shape='disc'))
    Img[Msk4==0]=0
    writeImage(Img,pthSave)
    
  }
  
  unlink(ImgDir, recursive=T)
  


generalPath= paste0(labelInput,"\\Mask_Image")
ImagePath=paste0(generalPath,"\\", "BlobMask")
MaskPath=paste0(generalPath,"\\", "Mask")
PathCheck=paste0(generalPath,"\\", "Check")
dir.create(PathCheck)
MskList=list.files(MaskPath)


foreach(i = 1:length(MskList)) %dopar% {
   mskP=paste0(MaskPath,"\\",MskList[i]) 
   ImgP= paste0(ImagePath,"\\",gsub("png","jpg",MskList[i]))
   image=image_read(ImgP)
   mask=image_read(mskP)
   Check=image_composite(image,mask,operator = "blend", compose_args="70")
   PathCheckImg=paste0(PathCheck,"\\",MskList[i])
   image_write(Check,PathCheckImg,format="jpg")     
}
stopCluster(cl)