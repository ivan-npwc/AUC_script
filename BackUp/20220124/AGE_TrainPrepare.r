   library(sp)
   library(spatialEco)
   library(raster)
   library(magick)
   library(parallel)
   library(doParallel)
   library(EBImage)
  
 labelInput
 Species
 TblAgeRef=NULL
 AgeRefRow1=NULL
 SelectedCategory=    c("An","TF","TN","AN","AF","F","J","SA","Sa","P")   #c("F") #c("J") # c("Sa") c("P")     #
  
   saveFolder = paste0(labelInput,"\\Predict\\Age_predict\\Train")
   if (dir.exists(saveFolder)==F){dir.create(paste0(labelInput,"\\Predict\\Age_predict"));dir.create(saveFolder)}
  TblAgeRefSave=   paste0(labelInput,"\\TRAIN_TblAgeRef_",SelectedCategory[1], ".csv")  
 
 
  cl <- makePSOCKcluster(detectCores(logical = F)-1) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)
    library(magick)
	 library(EBImage)
	})
  registerDoParallel(cl)
  ########################################################################################################
  ##########################################################################################################
  date1=substr(basename(labelInput),1,8)
  ObsCountDir=paste0(labelInput,"\\Observer count")
  PthObserverCount=list.files(ObsCountDir, pattern= ".shp", full.names=T)
  if (file.exists(PthObserverCount)==T) {
	ImgDirHoul=paste0(labelInput,"\\Predict\\Haulout")
	ImgDirRookery=paste0(labelInput,"\\Predict\\Rookery")
	ImgListHoul=list.files(ImgDirHoul)

	Pth_GeoInfoImg=list.files(labelInput,full.names=T, pattern="_table.csv")
	GeoInfoImg=read.csv(Pth_GeoInfoImg)
	GeoInfoImg$link1=paste0(GeoInfoImg$date,"_",GeoInfoImg$link)
	ImgList=GeoInfoImg[GeoInfoImg$link1 %in% ImgListHoul,]

	
	TableCount=shapefile(PthObserverCount)
	proj4string(TableCount) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
######################################################################### 
    AgeRefRow1=  foreach(i = 1:length(ImgList$link1)) %dopar% {
   # 	for (i in 1:length(ImgList$link1)) {
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

if (length(AmimalsInside512$coords.x1)>0 ) {
      pth_img=paste0(ImgDirHoul,"\\",imgName) 
	     x=as.numeric((RowSort$east50-AmimalsInside512$coords.x1)/(RowSort$east50-RowSort$west50)*1024)
	     y=as.numeric((RowSort$north50-AmimalsInside512$coords.x2)/(RowSort$north50-RowSort$south50)*1024)
		 lat=as.numeric(AmimalsInside512$coords.x2)
		 lon= as.numeric(AmimalsInside512$coords.x1) 
		 age=AmimalsInside512$LAYER
	AgeRefRow= data.frame(age=age,lon,lat,x=x,y=y,pth_img = pth_img,OPP=labelInput)
	AgeRefRow
#	AgeRefRow1=rbind(AgeRefRow,AgeRefRow1)
}
}
}	
tbl1=NULL
for (i in 1:length(AgeRefRow1)){
row1=AgeRefRow1[i]
 if (is.null(row1[[1]])==F){
 tbl1=rbind(data.frame(row1[[1]]),tbl1)
 }}

######
for (i in 1: length(tbl1$pth_img)) {
     if (tbl1$age[i] %in% c("TF","TN","An","AN")){tbl1$age[i]="An"}
	 if (tbl1$age[i] %in% c("AF","f")){tbl1$age[i]="F"}
	 if (tbl1$age[i] %in% c("SA","sa")){tbl1$age[i]="Sa"}
	 tbl1$pthSave[i]=paste0(saveFolder,"\\",tbl1$age[i],"_",i,"_",basename(tbl1$pth_img[i]))
}
write.csv(tbl1,TblAgeRefSave)
########################################################################
tbl1=read.csv(TblAgeRefSave)
tbl1=tbl1[tbl1$age %in% SelectedCategory,]
#listImgs= unique(tbl1$pth_img)

    foreach(e = 1:length(tbl1$pth_img)) %dopar% {
	   row1=   tbl1[e,]
	   pth_img=row1$pth_img
	   img=readImage(pth_img)
	   age= row1$age
	   pthSave=row1$pthSave

 #  if (file.exists(pth_img)) {
           x=row1$x
	       y=row1$y
       FromLeft=  1024- x  -256
       FromDown = y  -256
	   FromLeftStop=FromLeft+511
	   FromDownStop=FromDown+511
       img_crop = img[FromLeft:FromLeftStop, FromDown:FromDownStop,1:3]
	   writeImage(img_crop, pthSave, quality = 85)
	
 }
 ###########################################################

   
     foreach(i = 1:length(tbl1$pthSave)) %dopar% {
	# for (i in 1 : length(tbl1$pthSave)) { 
	 pth=tbl1$pthSave[i]
	 img=image_read(pth)

   
    imgDraw=image_draw(img)
	abline(h=256,col=2,lwd=10)
	abline(v=256,col=2,lwd=10)
	 
	image_write(imgDraw,pth) 
	dev.off()
 }
stopCluster(cl)
