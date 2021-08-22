
IncludeEmptyImgs=F

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
 date1=basename(labelInput)
  date=date1   #substr(date1,1,nchar(date1)-4)
MaskImgDir=paste0(labelInput,"\\", "Mask_Image"); if (dir.exists(MaskImgDir)== F ) {dir.create(MaskImgDir)}
maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask") ;if (dir.exists(maskDir)== F ) {dir.create(maskDir)}
TablePoints=read.csv(paste0(labelInput, "\\",date, "Points.csv"))
pthImg=paste0(labelInput,"\\",date, "_CountDist.csv")
TableImg=read.csv(pthImg)
########################################################################################################
TableImg=TableImg[TableImg$All512>0,]
imgList=unique(TableImg$link)

imgList=list.files(paste0(labelInput,"\\Mask_Image\\Image"))

cl <- makePSOCKcluster(detectCores (logical=F)) 
clusterEvalQ(cl, {	
  library(magick)
  library(EBImage)
})
registerDoParallel(cl)
foreach(i = 1:length(imgList)) %dopar% {	
  # for (i in 1:length(imgList)) {
  img=imgList[i]
     points=  data.frame(TablePoints[TablePoints$link==img,])

     lat=points$lat
     lon=points$lon
     xlim<<-unique(c(points$west50,points$east50))
     ylim<<-unique(c(points$south50,points$north50))
 
 cex=  paste0(points$sex)
    if (Species =="WLRS") {cex=0.6} else {
  cex=  gsub("An", "1.5", cex) 
  cex=  gsub("TN", "1.5", cex) 
  cex=  gsub("TF", "1.5", cex) 
  cex=  gsub("Sa", "0.9", cex)
  cex=  gsub("SA", "0.9", cex)
  cex=  gsub("AF",  "0.7", cex)
  cex=  gsub("F",  "0.7", cex) 
  cex=  gsub("J",  "0.7", cex) 	
  cex=  gsub("U",  "0.6", cex)
  cex=  gsub("P",  "0.25", cex)         }
  cex=as.numeric(cex)
  ###################
  fig <- image_graph(width = 1084, height = 1084, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n")
  
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=cex,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')
	   dev.off()
       fig=image_crop(fig,"1024x1024+30+30+30+30")

   #imgN=gsub("png","gif", img)
  pathImgSave=paste0(maskDir, "\\", date, "_",img)
  pathImgSave=gsub("jpg","png",pathImgSave)
  image_write(fig, path = pathImgSave, format = "png")   
}
###################

if (Species =="WLRS") {
listMsk=list.files(maskDir,full.names=T)
 for (i in 1: length(listMsk)) {
Msk= readImage(listMsk[i])
 Msk=1-Msk
 writeImage(Msk, listMsk[i])
}
}

#################################################################
  if (IncludeEmptyImgs==T) {
  
  HauloutDir=paste0(labelInput,"\\Predict\\Haulout")
  ListImgsHaulout=list.files(HauloutDir)
  ListImgsHaulout=gsub("jpg","png",ListImgsHaulout)
  
  Presence=list.files(maskDir)
  ListEmptyImgs=ListImgsHaulout[!(ListImgsHaulout %in% Presence)]
  foreach(y = 1:length(ListEmptyImgs)) %dopar% {	
   # for (y in 1:length(ListEmptyImgs)){
	   img=ListEmptyImgs[y]
       fig=image_blank(1024,1024,color = "black")
	   pathImgSave=paste0(maskDir, "\\",img)
       pathImgSave=gsub("jpg","png",pathImgSave)
       image_write(fig, path = pathImgSave, format = "png")   
    }
	
	
	}
 

 

  
  
  

stopCluster(cl)


