library(sp)
library(spatialEco)
library(raster)
library(magick)
 library(parallel)
  library(doParallel)
  
  TblAgeRef=NULL
  Species="SSLAdult"
  saveFolder =  "C:\\SSL_DB\\2020_138_OPP\\20200612_143112\\AN"
  DirOPP="C:\\SSL_DB\\2020_138_OPP"
 SelectedCategory=c("An","TF","TN","AN")   #c("An","TF","TN","AN") #J Sa
 TblAgeRefSave=   paste0(saveFolder,"\\TblAgeRef_",SelectedCategory[1], ".csv")  
 
DirsForTest= c("")  #c("20200612_143112", "20200613_134208","20200614_135547","20200705_123506","20200720_130923","20200730_123840")
   
  cl <- makePSOCKcluster(4) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)
    library(magick)
	})
  registerDoParallel(cl)

listOPP=     "C:\\SSL_DB\\2020_138_OPP\\20200612_143112"     #list.files(DirOPP, full.names=T)
for (g in 1:length(listOPP)) {
  labelInput=listOPP[g]
  if (!(basename(labelInput) %in% DirsForTest)){
     
  date1=substr(basename(labelInput),1,8)
  ObsCountDir=paste0(labelInput,"\\Observer count")
  PthObserverCount=list.files(ObsCountDir, pattern= ".shp", full.names=T)
  if (file.exists(PthObserverCount)==T) {
	ImgDirHoul=paste0(labelInput,"\\Predict\\Haulout")
	ImgDirRookery=paste0(labelInput,"\\Predict\\Rookery")
	ImgListHoul=list.files(ImgDirHoul)
	ImgListRookery=list.files(ImgDirRookery)
	Pth_GeoInfoImg=list.files(labelInput,full.names=T, pattern="_table.csv")
	GeoInfoImg=read.csv(Pth_GeoInfoImg)
	GeoInfoImg$link1=paste0(GeoInfoImg$date,"_",GeoInfoImg$link)
	ImgList=GeoInfoImg[GeoInfoImg$link1 %in% ImgListHoul,]

	
	TableCount=shapefile(PthObserverCount)
	proj4string(TableCount) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

	######################################################################### 
 #   AgeRefRow1=  foreach(i = 1:length(ImgList$link1)) %dopar% {
    	for (i in 1:length(ImgList$link1)) {
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
  
      xlim<<-unique(c(RowSort$west50,RowSort$east50))
      ylim<<-unique(c(RowSort$south50,RowSort$north50))
	  
	  
	
	 
  Limit512=data.frame(west=RowSort$west,east=RowSort$east,south=RowSort$south,north=RowSort$north)
  Poligon512 <- as(raster::extent(Limit512$west,Limit512$east,Limit512$south,Limit512$north), "SpatialPolygons")
  proj4string(Poligon512) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  AmimalsInside512 <- point.in.poly(TableCount,Poligon512) 
  AmimalsInside512=as.data.frame(AmimalsInside512)
  AmimalsInside512=AmimalsInside512[is.na(AmimalsInside512[,3]) == FALSE,]
  AmimalsInside512=AmimalsInside512[AmimalsInside512$LAYER %in% SelectedCategory,]
  
if (length(AmimalsInside512$coords.x1)>0 ) {

      pth_img=paste0(ImgDirHoul,"\\",imgName) 
       img=image_read(pth_img)
	   img1= image_rotate(img, 180)
	   img2=image_flip(img1)

	   
	     x=as.numeric((RowSort$east50-AmimalsInside512$coords.x1)/(RowSort$east50-RowSort$west50)*1024)
	     y=as.numeric((RowSort$north50-AmimalsInside512$coords.x2)/(RowSort$north50-RowSort$south50)*1024)
		 lat=as.numeric(AmimalsInside512$coords.x2)
		 lon= as.numeric(AmimalsInside512$coords.x1) 
		
		 

       for (f in 1:length(AmimalsInside512[,1])) {
	   
     AnimalSort=AmimalsInside512[f,]
	 sex=  AnimalSort$LAYER
	 if (sex %in% c("TF","TN","An","AN")){sex="An"}
	 x=(RowSort$east50-AnimalSort$coords.x1)/(RowSort$east50-RowSort$west50)*1024
	 y=(RowSort$north50-AnimalSort$coords.x2)/(RowSort$north50-RowSort$south50)*1024
	imgDraw=image_draw(img2)
	abline(h=y,col=2,lwd=10)
	abline(v=x,col=2,lwd=10)
      date1= substr(imgName,1,8)
	  
	#  if (date1 > 20190501 & date1 <= 20190615) {points(0,0,pch=15,cex=40,col=2)}
	#  if (date1 > 20190615 & date1 <= 20190630) {points(1024,0,pch=15,cex=40,col=2)}
	#  if (date1 > 20190630 & date1 <= 20190715) {points(1024,1024,pch=15,cex=40,col=2)}
    # if (date1 > 20190715 & date1 <= 20190801) {points(0,1024,pch=15,cex=40,col=2)}

	dev.off()
	save_folder_sex = paste0(saveFolder,"\\",sex)
	if (dir.exists(save_folder_sex)==F) {dir.create(save_folder_sex)}
	pth_save_img=paste0(save_folder_sex,"\\",sex,"_",f,"_",imgName)
	AgeRefRow= data.frame(lon,lat,x=x,y=y,pth_img = pth_save_img)
	TblAgeRef =rbind(AgeRefRow,TblAgeRef)
	image_write(imgDraw,pth_save_img)
}
} 
}	
}
}
}
write.csv(TblAgeRef,TblAgeRefSave)
##
tableRef1=read.csv(TblAgeRefSave)
AgeSmallFolder = paste0(saveFolder,"\\Small"); if (dir.exists(AgeSmallFolder)==F) {dir.create(AgeSmallFolder)}
  listImgsPTH=unique(tableRef1$pth_img)
      foreach(i = 1:length(listImgsPTH)) %dopar% {
	
    #   for (i in 1:length(tableRef1$pth_img)) {
	pth=paste0(listImgsPTH[i])
	row1=tableRef1[tableRef1$pth_img==pth,][1,]
	#pth=paste0(tableRef1$pth_img[i])
   if (file.exists(pth)) {
       age=SelectedCategory[1] #paste0(tableRef1$age[i])
       x=row1$x-256
       y=row1$y-256
	     fldAge=paste0(AgeSmallFolder,"\\",age); if (dir.exists(fldAge)==F) {dir.create(fldAge)}
	     savePth=paste0(fldAge,"\\",basename(pth))
	     size_crop=paste0("512x512-",x, "+",y)
           img=image_read(pth)
           crop=image_crop(img, size_crop)
           image_write(crop,savePth)
 }
 }
stopCluster(cl)
