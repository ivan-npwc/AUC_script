
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(magick)

date=basename(labelInput)
PointsDir=paste0(labelInput,"\\Predict\\PointsObserverImg") ;if (dir.exists(PointsDir)== F ) {dir.create(PointsDir)}


pthImg=paste0(labelInput,"\\Predict\\",date, "_CountDist.csv")
pthPoints=paste0(labelInput,"\\Predict\\", date,"Points.csv")

TablePoints=read.csv(pthPoints)
TablePoints=TablePoints[TablePoints$sex !="P",]
TablePoints=TablePoints[TablePoints$sex !="DP",]
TableImg=read.csv(pthImg)
########################################################################################################
TableImg=TableImg[TableImg$All512>0,]
imgList=unique(TableImg$link)
imgList=imgList[is.na(imgList)==F]

cl <- makePSOCKcluster(4) 
clusterEvalQ(cl, {	
  library(magick)
})
registerDoParallel(cl)
foreach(i = 1:length(imgList)) %dopar% {	
#    for (i in 1:length(imgList)) {
  img=paste0(imgList[i])
  points1=  data.frame(TablePoints[TablePoints$link==img,])
  lat=points1$lat
  lon=points1$lon
  xlim<<-unique(c(points1$west50,points1$east50))
  ylim<<-unique(c(points1$south50,points1$north50))
  ###################
  fig <- image_graph(width = 1084, height = 1084, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n") 
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=0.2,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')	 
  dev.off()
  fig=image_crop(fig,"1024x1024+30+30+30+30")
  pathImgSave=paste0(PointsDir, "\\", date, "_",img)
  image_write(fig, path = pathImgSave, format = "JPG")   
}
stopCluster(cl)


