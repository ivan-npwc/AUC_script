AgePredictPrepare=function (labelInput,Species) {
  library(sp)
  library(spatialEco)
  library(raster)
  library(magick)
  library(parallel)
  library(doParallel)
  
  cl <- makePSOCKcluster(detectCores ()-3) 
  clusterEvalQ(cl, {
  
    library(sp)
    library(spatialEco)
    library(raster)
    library(magick)
	
	})
  registerDoParallel(cl)
  
    PthTblAgeRef=paste0(labelInput,"\\Predict\\",basename(labelInput),"_", Species,"AgeRef.csv")
    Pth_GeoInfoImg=paste0(labelInput,"\\", basename(labelInput),"_table.csv")
    RookeryFolder=paste0(labelInput,"\\Predict\\Rookery")
    HouloutFolder=paste0(labelInput,"\\Predict\\Haulout")
	
    AgeFolder	= paste0(labelInput,"\\Predict\\Age_predict"); if (dir.exists(AgeFolder)==T) {unlink(AgeFolder,recursive=T)};dir.create(AgeFolder)
    AgeBigFolder = paste0(AgeFolder,"\\Big");  dir.create(AgeBigFolder)
    AgeSmallFolder = paste0(AgeFolder,"\\Small");dir.create(AgeSmallFolder)
	
    PthPredictCount=paste0(labelInput,"\\Predict\\",Species,"_", basename(labelInput),".csv")
TblAgeRef=NULL
   listImgsHoulout=list.files(HouloutFolder, full.names=T)
     if (dir.exists(RookeryFolder)==T) {
         listImgsRookery=list.files(RookeryFolder, full.names=T)
         listImgsHoulout=rbind(listImgsHoulout,listImgsRookery)
	}	 
	GeoInfoImg=read.csv(Pth_GeoInfoImg)
	GeoInfoImg$link1=paste0(GeoInfoImg$date,"_",GeoInfoImg$link)
	ImgList=GeoInfoImg[GeoInfoImg$link1 %in% basename(listImgsHoulout),]
    ImgList$pth=listImgsHoulout
	TableCount=read.csv(PthPredictCount)
	####################
	Points <- SpatialPointsDataFrame (data.frame(TableCount$lon,TableCount$lat), data.frame(TableCount$age))
	proj4string(Points) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
#########################################################################   
  TblAgeRef = NULL
 #  for (i in 1: length(ImgList$link1)) {
  TblAgeRef =	foreach(i = 1:length(ImgList$link1),.combine=rbind) %dopar% {
 
	    imgName=paste0(ImgList$link1[i])
		RowSort=ImgList[ImgList$link1==imgName,]
	     NSdif50=(RowSort$north-RowSort$south)/2 
         RowSort$north50=RowSort$north+NSdif50  
         SNdif50=(RowSort$north-RowSort$south)/2
         RowSort$south50=RowSort$south-SNdif50 
         WEdiff50= (RowSort$east-RowSort$west)/2
         RowSort$west50=RowSort$west-WEdiff50 
         EWdiff50= (RowSort$east-RowSort$west)/2 
         RowSort$east50=RowSort$east+EWdiff50 
      
	  
        Limit512=data.frame(west=RowSort$west,east=RowSort$east,south=RowSort$south,north=RowSort$north)
        Poligon512 <- as(raster::extent(Limit512$west,Limit512$east,Limit512$south,Limit512$north), "SpatialPolygons")
        proj4string(Poligon512) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
        AmimalsInside512 <- point.in.poly(Points,Poligon512) 
        AmimalsInside512=as.data.frame(AmimalsInside512)
        AmimalsInside512 =AmimalsInside512[is.na(AmimalsInside512[,2]) == FALSE,]
		
		
  if (length(AmimalsInside512$coords.x1)>0 ) {
  	 
	     x=as.numeric((RowSort$east50-AmimalsInside512$coords.x1)/(RowSort$east50-RowSort$west50)*1024)
	     y=as.numeric((RowSort$north50-AmimalsInside512$coords.x2)/(RowSort$north50-RowSort$south50)*1024)
		 lat=as.numeric(AmimalsInside512$coords.x2)
		 lon= as.numeric(AmimalsInside512$coords.x1)
		 pth_img=paste0(RowSort$pth) 
		 AgeRefRow= data.frame(lon,lat,x=x,y=y,pth_img = pth_img)
		 AgeRefRow
	#	 TblAgeRef =rbind(AgeRefRow,TblAgeRef)		 
} 
 # 
}
   u=c(1:length(TblAgeRef$pth_img))
   TblAgeRef$pth_save_img= paste0(AgeBigFolder,"\\", u,"#", basename(as.character(TblAgeRef$pth_img)))
    write.csv(TblAgeRef,PthTblAgeRef,row.names=F)
###########################################################################
   TblAgeRef=read.csv(PthTblAgeRef)
   listImgs=unique(TblAgeRef$pth_img)
   
   # for (u in 1:length(listImgs)) {
	foreach(u = 1:length(listImgs)) %dopar% {
          pth_img= paste0(listImgs [u])
		  ListAnimalsPoints=TblAgeRef[TblAgeRef$pth_img== pth_img,]

          img=image_read(pth_img)
	      img1= image_rotate(img, 180)
	      img2=image_flip(img1)
   
   
  #   foreach(i = 1:length(ListAnimalsPoints$lon)) %dopar% {
	 for (i in 1 : length(ListAnimalsPoints$lon)) { 
	 pth_save_img= paste0(ListAnimalsPoints$pth_save_img[i])
     x=ListAnimalsPoints$x[i]
     y=ListAnimalsPoints$y[i]
   
   imgDraw=image_draw(img2)
	abline(h=y,col=2,lwd=10)
	abline(v=x,col=2,lwd=10)
	  dev.off()
	image_write(imgDraw,pth_save_img) 
}
}
###################################################################################
    tableRef1=read.csv(PthTblAgeRef)
      foreach(i = 1:length(tableRef1$pth_save_img)) %dopar% {
    #   for (i in 1:length(tableRef1$pth)) {
 pth=paste0(tableRef1$pth_save_img[i])
   if (file.exists(pth)) {
       age=paste0(tableRef1$age[i])
       x=tableRef1$x[i]-256
       y=tableRef1$y[i]-256
	     fldAge=paste0(AgeSmallFolder,"\\",age); if (dir.exists(fldAge)==F) {dir.create(fldAge)}
	     savePth=paste0(fldAge,"\\",basename(pth))
	     size_crop=paste0("512x512-",x, "+",y)
           img=image_read(pth)
           crop=image_crop(img, size_crop)
           image_write(crop,savePth)
 }
 }
stopCluster(cl)
}
AgePredictPrepare (labelInput,Species)