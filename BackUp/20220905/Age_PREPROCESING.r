
labelInput
date1=substr(basename(labelInput),1,15)
Species
System_data

  library(sp)
  library(spatialEco)
  library(raster)
  library(magick)
  library(parallel)
  library(doParallel)
  library(EBImage)
  
  cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)
    library(magick)
	library(EBImage)
	})
  registerDoParallel(cl)
  
    PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
    Pth_GeoInfoImg=paste0(labelInput,"\\", date1,"_table.csv")
	PthPredictCount=paste0(labelInput,"\\Predict\\",Species,"_", date1,".csv")
    HouloutFolder=paste0(labelInput,"\\Predict\\Haulout");listImgs=list.files(HouloutFolder, full.names=T)
    ageFolder=paste0(labelInput,"\\Predict\\", Species)
	unlink(ageFolder, recursive=T);dir.create(ageFolder)
  hdr=paste0(ageFolder,"\\Rookery");dir.create(hdr)
  rdr=paste0(ageFolder,"\\Haulout");dir.create(rdr)
   


	
	DirRPol=paste0(labelInput,"\\Polygons\\Rookery");PthPolR=list.files(DirRPol,full.names=T, pattern=".shp")
	PRJ="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
	if (length(PthPolR) !=0){RookeryPol=shapefile(PthPolR);proj4string(RookeryPol) <- PRJ}
	
    TblAgeRef=NULL
	
	#############################################################################################################
	GeoInfoImg=read.csv(Pth_GeoInfoImg)
	GeoInfoImg$link1=paste0(GeoInfoImg$date,"_",GeoInfoImg$link)
	ImgList=GeoInfoImg[GeoInfoImg$link1 %in% basename(listImgs),]
    ImgList$pth=listImgs
	TableCount1=read.csv(PthPredictCount)
	TableCount=TableCount1[!TableCount1$age %in% c("SmallError","BigError"),]
 if (Species=="SSLAdult"){TableCount=TableCount[TableCount$age != "P",]}	
	####################
	Points <- SpatialPointsDataFrame(data.frame(TableCount$lon,TableCount$lat), data.frame(TableCount$age))
	proj4string(Points) <- PRJ
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
        index= Points %over% Poligon512
		Points$index=index
        AmimalsInside512=  coordinates(Points[is.na(Points$index)==F,])
		
  if (length(AmimalsInside512[,1])>0 ) {
  	 
	     x=as.numeric((RowSort$east50-AmimalsInside512[,1])/(RowSort$east50-RowSort$west50)*1024)
	     y=as.numeric((RowSort$north50-AmimalsInside512[,2])/(RowSort$north50-RowSort$south50)*1024)
		 lat=as.numeric(AmimalsInside512[,2])
		 lon= as.numeric(AmimalsInside512[,1])
		 pth_img=paste0(RowSort$pth) 
		 AgeRefRow= data.frame(lon,lat,x=x,y=y,pth_img = pth_img)
		 AgeRefRow		 
} 
}
  # TblAgeRef$pth_save_img= paste0(AgeBigFolder,"\\", u,"#", basename(as.character(TblAgeRef$pth_img)))
###########################################################################
TblAgeRef =unique(TblAgeRef)
 coords <- data.frame(lat=TblAgeRef$lon,lon= TblAgeRef$lat)  
	TblAgeRefNoLatLon=TblAgeRef
	TblAgeRefNoLatLon$lon  =NULL 
	TblAgeRefNoLatLon$lat=NULL
	PrdLocPnts <- SpatialPointsDataFrame(coords = coords,
                                        data = TblAgeRefNoLatLon, 
                                        proj4string = CRS(PRJ))  

if (length(PthPolR) !=0){
index= PrdLocPnts  %over%   RookeryPol
index1=data.frame(index)
index1[,1][is.na(index1[,1])==F]="Rookery"
index1[,1][is.na(index1[,1])==T]="Haulout"
PrdLocPnts$Rookery=index1[,1] 
} else {PrdLocPnts$Rookery="Haulout"}



TblAgeRef=PrdLocPnts
TblAgeRef$pth_img=as.character(TblAgeRef$pth_img)
 for (i in 1:length(TblAgeRef[,1])) {
 if (TblAgeRef$Rookery[i]=="Rookery"){
 TblAgeRef$pth_save_img[i]=paste0(ageFolder,"\\Rookery\\",i,"_",basename(TblAgeRef$pth_img[i]))}
 if (TblAgeRef$Rookery[i]=="Haulout"){
 TblAgeRef$pth_save_img[i]=paste0(ageFolder,"\\Haulout\\",i,"_",basename(TblAgeRef$pth_img[i]))}
 }
 write.csv(TblAgeRef,PthTblAgeRef,row.names=F)
################################################
#krest=image_read("Krest.png")
#imgDraw=image_draw(krest)
#	abline(h=256,col=2,lwd=10)
#	abline(v=256,col=2,lwd=10)
#image_write(imgDraw,"krest.png") 
#dev.off()
####################################################################   CLIPS
  TblAgeRef=read.csv(PthTblAgeRef)
  listImgs=unique(TblAgeRef$pth_img)
   # for (u in 1:length(listImgs)) {
	foreach(u = 1:length(listImgs)) %dopar% {
          pth_img= paste0(listImgs [u])
		  ListAnimalsPoints=TblAgeRef[TblAgeRef$pth_img== pth_img,]

          img = readImage(pth_img)
          img1=flip(img)
		  img2=rotate(img1,180)
       
	 for (i in 1 :length(ListAnimalsPoints$lon) ) {  #
     img_crop=NULL	 
	 pth_save_img= paste0(ListAnimalsPoints$pth_save_img[i])
     x=ListAnimalsPoints$x[i]
     y=ListAnimalsPoints$y[i]
  
       FromLeft=   x  -256
       FromDown = y  -256
	   FromLeftStop=FromLeft+511
	   FromDownStop=FromDown+511

       img_crop = img2[FromLeft:FromLeftStop, FromDown:FromDownStop,1:3]

	   writeImage(img_crop, pth_save_img, quality = 85)	
}

}
#############
     foreach(i = 1:length(TblAgeRef$pth_save_img)) %dopar% {
	# for (i in 1 : length(TblAgeRef$pth_save_img)) { 
	 pth=TblAgeRef$pth_save_img[i]
	 img=image_read(pth)
    imgDraw=image_draw(img)
	abline(h=256,col=2,lwd=10)
	abline(v=256,col=2,lwd=10)
	 
	image_write(imgDraw,pth) 
	dev.off()
}
 stopCluster(cl)
