
IncludeEmptyImgs=T


labelInput

library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(EBImage)

date1=substr(basename(labelInput),1,15)

HauloutDir=paste0(labelInput, "\\", "Predict", "\\","PUP")

MaskImgDir=paste0(labelInput,"\\", "Mask_Image_PUP"); if (dir.exists(MaskImgDir)== F ) {dir.create(MaskImgDir)}
maskDir=paste0(labelInput,"\\", "Mask_Image_PUP", "\\","Mask") ;if (dir.exists(maskDir)== F ) {dir.create(maskDir)}
imgDir=paste0(labelInput,"\\Mask_Image_PUP\\Image");if (dir.exists(imgDir)== F ) {dir.create(imgDir)}

TablePointsPTH=paste0(labelInput,"\\Predict\\Pup_PointsOnImg_",date1,".csv")
TablePoints=read.csv(TablePointsPTH)
#pthImg=paste0(labelInput,"\\",date, "_CountDist.csv")
#TableImg=read.csv(pthImg)
######################################################################################################## CREATE MASK WITH POINTS
#TableImg=TableImg[TableImg$All512>0,]
#imgList=unique(TableImg$link)
#imgList=imgList[is.na(imgList) ==F]
imgList=unique(TablePoints$X)

cl <- makePSOCKcluster(detectCores (logical=F)) 
clusterEvalQ(cl, {	
  library(magick)
  library(EBImage)
})
registerDoParallel(cl)
#foreach(i = 1:length(imgList)) %dopar% {	
   for (i in 1:length(imgList)) {
     
	 img=imgList[i]
     points=  data.frame(TablePoints[TablePoints$X==img,])
     lat=points$lat
     lon=points$lon
     xlim <<-unique(c(points$west,points$east))
     ylim <<-unique(c(points$south,points$north))
	 

   cex=0.25

 
  ###################
  fig <- image_graph(width = 542, height = 542, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n")
  
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=cex,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')
	   dev.off()
       fig=image_crop(fig,"1024x1024+15+15+15+15")

   #imgN=gsub("png","gif", img)
  pathImgSave=paste0(maskDir, "\\",img)
  pathImgSave=gsub("jpg","png",pathImgSave)
  image_write(fig, path = pathImgSave, format = "png")   
}
##########################################################################    COPY IMGS FOR MSK WITH ANIMALS
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
 

 

  
  
  

stopCluster(cl)


