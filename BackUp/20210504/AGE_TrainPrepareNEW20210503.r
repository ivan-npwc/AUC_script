   library(sp)
   library(spatialEco)
   library(raster)
   library(magick)
   library(parallel)
   library(doParallel)
  
  SelectedCategory=c("An","TF","TN","AN")   #c("F") #c("J") # c("Sa") 
  saveFolder = "D:\\SSL_DB\\TRAIN\\SSLage\\NewData\\AN"
  
  
  
  DirOPP="F:\\SSL_DB\\2020_138_OPP\\KAD"
  DirsForTest= c("2222")
  
  TblAgeRef=NULL
  Species="SSLAdult"
  TblAgeRefSave=   paste0(saveFolder,"\\TblAgeRef_",SelectedCategory[1], ".csv")  
 
 
   
  cl <- makePSOCKcluster(detectCores(logical = F)) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)
    library(magick)
	})
  registerDoParallel(cl)

listOPP=  list.files(DirOPP, full.names=T)
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
#  AmimalsInside512=AmimalsInside512[AmimalsInside512$LAYER %in% SelectedCategory,]
  
if (length(AmimalsInside512$coords.x1)>0 ) {

      pth_img=paste0(ImgDirHoul,"\\",imgName) 
   #    img=image_read(pth_img)
	#   img1= image_rotate(img, 180)
	  # img2=image_flip(img1)

	   
	     x=as.numeric((RowSort$east50-AmimalsInside512$coords.x1)/(RowSort$east50-RowSort$west50)*1024)
	     y=as.numeric((RowSort$north50-AmimalsInside512$coords.x2)/(RowSort$north50-RowSort$south50)*1024)
		 lat=as.numeric(AmimalsInside512$coords.x2)
		 lon= as.numeric(AmimalsInside512$coords.x1) 
		 age=AmimalsInside512$LAYER
		
		 

 #      for (f in 1:length(AmimalsInside512[,1])) {
#	   
#     AnimalSort=AmimalsInside512[f,]
#	 sex=  AnimalSort$LAYER
#	 if (sex %in% c("TF","TN","An","AN")){sex="An"}
#	 x=(RowSort$east50-AnimalSort$coords.x1)/(RowSort$east50-RowSort$west50)*1024
#	 y=(RowSort$north50-AnimalSort$coords.x2)/(RowSort$north50-RowSort$south50)*1024
#	imgDraw=image_draw(img2)
#	abline(h=y,col=2,lwd=10)
#	abline(v=x,col=2,lwd=10)
   #   date1= substr(imgName,1,8)
	  
	
#	dev.off()
#	save_folder_sex = paste0(saveFolder,"\\",sex)
#	if (dir.exists(save_folder_sex)==F) {dir.create(save_folder_sex)}
#	pth_save_img=paste0(save_folder_sex,"\\",sex,"_",f,"_",imgName)
	
	AgeRefRow= data.frame(age=age,lon,lat,x=x,y=y,pth_img = pth_img,OPP=labelInput)
	AgeRefRow
#	TblAgeRef =rbind(AgeRefRow,TblAgeRef)
	
#	image_write(imgDraw,pth_save_img)
}
}

 
}	
}
tbl1=NULL
for (i in 1:length(AgeRefRow1)){
row1=AgeRefRow1[i]
 if (is.null(row1[[1]])==F){
 tbl1=rbind(data.frame(row1[[1]]),tbl1)
 }}



write.csv(tbl1,TblAgeRefSave)





##
tableRef1=read.csv(TblAgeRefSave)
if (sex %in% c("TF","TN","An","AN")){sex="An"}
#AgeSmallFolder = paste0(saveFolder,"\\Small"); if (dir.exists(AgeSmallFolder)==F) {dir.create(AgeSmallFolder)}
  #listImgsPTH=unique(tbl1$pth_img)
      foreach(i = 1:length(tbl1$pth_img)) %dopar% {
	
    #   for (i in 1:length(tableRef1$pth_img)) {
	row1=tbl1[i,]
	pth_img=row1$pth_img
    x=row1$x
	y=row1$y
	age= row1$age
	pthSave=paste0(saveFolder,"\\",age,"_",i,"_",basename(pth_img))
	
	 #fldAge=paste0(AgeSmallFolder,"\\",age); if (dir.exists(fldAge)==F) {dir.create(fldAge)}
	 #savePth=paste0(saveFolder,"\\",basename(pth))
	
   if (file.exists(pth_img)) {
   	 
	   #  pth_img=paste0(ImgDirHoul,"\\",imgName) 
       img=image_read(pth_img)
	   img1= image_rotate(img, 180)
	   img2=image_flip(img1)
#	 x=(RowSort$east50-AnimalSort$coords.x1)/(RowSort$east50-RowSort$west50)*1024
#	 y=(RowSort$north50-AnimalSort$coords.x2)/(RowSort$north50-RowSort$south50)*1024
	imgDraw=image_draw(img2)
	abline(h=y,col=2,lwd=10)
	abline(v=x,col=2,lwd=10) 
	
       FromLeft=  x   #+256
       FromDown=y    #-256
	   size_crop= paste0("512x512!-",FromLeft, "+",FromDown)
       crop=NULL
           crop=image_crop(imgDraw, size_crop)
		    crop
		#   image_crop(img2,"512x512!+166")     #!-166+411"
		   
		  
           image_write(crop,savePth)
 }
 }
stopCluster(cl)
