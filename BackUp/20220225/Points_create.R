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
TableImg =read.csv(pthImg)
######################################################################################################## CREATE MASK WITH POINTS
TablePoints = TablePoints %>% filter(sex== selectedAge)
imgList= TableImg   %>% filter(All512>0) %>% summarise (imgList=unique(link)) 
imgList=imgList$imgList


cl <- makePSOCKcluster(detectCores (logical=F)) 
clusterEvalQ(cl, {	
  library(magick)
})
registerDoParallel(cl)
foreach(i = 1:length(imgList)) %dopar% {	
#   for (i in 1:length(imgList)) {
  img=imgList[i]
  
  points=  data.frame(TablePoints[TablePoints$link==img,])
   if (length(points$sex)>0) {
  
  lat=points$lat
  lon=points$lon
  
  xlim<<-unique(c(points$west50,points$east50))
  ylim<<-unique(c(points$south50,points$north50))
 
 cex=  paste0(points$sex)

 # cex=  gsub("An", "1.5", cex) 
 # cex=  gsub("TN", "1.5", cex) 
 # cex=  gsub("TF", "1.5", cex) 
 # cex=  gsub("Sa", "0.9", cex)
 # cex=  gsub("SA", "0.9", cex)
 # cex=  gsub("AF",  "0.7", cex)
 # cex=  gsub("F",  "0.7", cex) 
 # cex=  gsub("J",  "0.7", cex) 	
 # cex=  gsub("U",  "0.6", cex)
 # cex=  gsub("P",  "0.25", cex)  
  cex=  gsub("CU_P",  "0.25", cex)
  
   if (Species =="WLRS") {cex=0.6}
  cex=as.numeric(cex)
  ###################
  fig <- image_graph(width = 1084, height = 1084, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n") 
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=cex,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')	 
  dev.off()
  fig=image_crop(fig,"1024x1024+30+30+30+30")
   #imgN=gsub("png","gif", img)
  pathImgSave=paste0(maskDir, "\\", date1, "_",img)
  pathImgSave=gsub("jpg","png",pathImgSave)
  image_write(fig, path = pathImgSave, format = "png")   
}
}
stopCluster(cl)




mskList=list.files(maskDir)
NeedImgs=gsub("png","jpg",mskList)
NeedImgs1=paste0(HauloutDir,"\\",NeedImgs)
file.copy(NeedImgs1,imgDir)
 
#############################################################################  CONVERT MSK FOR WLRS
#if (Species =="WLRS") {
#listMsk=list.files(maskDir,full.names=T)
# for (i in 1: length(listMsk)) {
#Msk= readImage(listMsk[i])
# Msk=1-Msk
# writeImage(Msk, listMsk[i])
#}
#}
#################################################################   IMGS MSK WITHOUT ANIMALS
#  if (IncludeEmptyImgs==T) {
#  
# 
#  ListImgsHaulout=list.files(HauloutDir)
#  Presence=list.files(maskDir)
#  ImgPres=list.files(imgDir)
#  NeedImgs=ListImgsHaulout[!(ListImgsHaulout %in% ImgPres)]
#  NeedImgs1=paste0(HauloutDir,"\\",NeedImgs)
#  file.copy(NeedImgs1,imgDir)
#  
#  ListEmptyMsk=gsub("jpg","png",NeedImgs)
#  
#  foreach(y = 1:length(ListEmptyImgs)) %dopar% {	
#   # for (y in 1:length(ListEmptyImgs)){
#	   img = ListEmptyMsk[y]
#       fig=image_blank(1024,1024,color = "black")
#	   pathImgSave=paste0(maskDir, "\\",img)
#       pathImgSave=gsub("jpg","png",pathImgSave)
#       image_write(fig, path = pathImgSave, format = "png")   
#    }
	
	
#	}
 

 

  
  
  

