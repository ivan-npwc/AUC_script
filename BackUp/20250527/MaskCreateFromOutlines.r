  # source("C:\\Users\\usato\\SSL_DB\\AUC\\Modules\\MaskCreateFromOutlines.r")
   
   
	library(parallel)
	library(doParallel)
	library(foreach)
	library(sp)
	library(spatialEco)
	library(raster)
	library(rgdal)
	####################################################
	#  check = T
	#  labelInput = "D:\\PV_DB/2023_H0099_OPP/20230502_135032/20230502_135032_MINI3PRO_20m"
	  date  = substr(basename(labelInput),1,15)
	  MaskImgDir=paste0(labelInput,"\\", "Mask_Image"); if (dir.exists(MaskImgDir)== F ) {dir.create(MaskImgDir)}
	  maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask") ;if (dir.exists(maskDir)== F ) {dir.create(maskDir)}
	  imgDir=paste0(labelInput,"\\Mask_Image\\Image");if (dir.exists(imgDir)== F ) {dir.create(imgDir)}
	  pthTable=paste0(labelInput,"\\", date,".csv")
	  pthHouloutImg=paste0(labelInput,"\\Predict\\TilesOverlap")
	  ObserverPointDIR=paste0(labelInput,"\\Polygons\\Animal_outline")
	  #############################################################
	  PATHPoints=list.files(ObserverPointDIR,full.names=T,pattern="kml")
	  
		if (length(PATHPoints) == 1){

	  Points <-readOGR(PATHPoints)   
	   crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
	   proj4string(Points) <- crs
	   listImgHoul=list.files(pthHouloutImg)
	   table0=read.csv(pthTable)
	   table1=table0[table0$imgName %in% listImgHoul,]

	  ############################################################
		NSdif50=(table1$north-table1$south)/2 
		table1$north50=table1$north+NSdif50  
		SNdif50=(table1$north-table1$south)/2
		table1$south50=table1$south-SNdif50 
		WEdiff50= (table1$east-table1$west)/2
		table1$west50=table1$west-WEdiff50 
		EWdiff50= (table1$east-table1$west)/2 
		table1$east50=table1$east+EWdiff50 
	  ####################################################################################################
	  table2= data.frame(imgName=table1$imgName, west=table1$west, east=table1$east,south=table1$south, north=table1$north,  
	  north50=  table1$north50, south50= table1$south50,west50= table1$west50, east50= table1$east50)
	  ################################################################################################################################################
#	  cl <- makePSOCKcluster(detectCores (logical=F)-1) 
#	  clusterEvalQ(cl, {
#	    library(sp)
#	    library(spatialEco)
#	    library(raster)	
 #        library (magick)		
#	  })
#	  registerDoParallel(cl)
	
		 
		 options(warn=-1)
		for (y in 1:length(table2[,1])) {
			selectRow=table2[y,]
			Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
			proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		 
		   pts = Points[!is.na(over(Points,as(Poligon,"SpatialPolygons"))),]
		   if (length(pts)> 0) {
		   
		  pth_mask=paste0(maskDir,"\\", date,"_",selectRow$imgName)
		  pth_mask=gsub("jpg","png",pth_mask)
		  pth_img=paste0(imgDir,"\\", date,"_",selectRow$imgName)
		  pth_img=gsub("png","jpg",pth_img)
		  
		  img_from=paste0(labelInput,"\\Predict\\TilesOverlap\\",selectRow$imgName)
		  
		  xlim<<-unique(c(selectRow$west50,selectRow$east50))
		  ylim<<-unique(c(selectRow$south50,selectRow$north50))
		  fig=NULL
		  fig <- image_graph(width = 1084, height = 1084, res =720) 
		  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n") 
		  

		  plot(pts,xlim=xlim,ylim=ylim, col="red",
			   pch=16,bg=16,axes=F, ann=F, xaxt='n', yaxt='n')	 
		  dev.off()
		   fig=image_crop(fig,"1024x1024+30+30+30+30") 
		   fig = image_resize(fig, "1024x1240!")
		  fig=image_crop(fig,"1024x1024+0+108+0+108") 
		  image_write(fig, path = pth_mask, format = "png")   
	
		file.copy(img_from, pth_img)
		options(warn=0)
			}
		  }
	  }

	  
	########################################################################    
#	 if (check == T){ 
#	  
#	generalPath= paste0(labelInput,"\\Mask_Image")
#	ImagePath=paste0(generalPath,"\\", "Image")
#	MaskPath=paste0(generalPath,"\\", "Mask")
#	PathCheck=paste0(generalPath,"\\", "Check")
#	unlink(PathCheck, recursive=T)
#	dir.create(PathCheck)
#	MskList=list.files(MaskPath)
#
#
#	foreach(i = 1:length(MskList)) %dopar% {
#
#	  mskP=paste0(MaskPath,"\\",MskList[i]) 
#	  ImgP= paste0(ImagePath,"\\",gsub("png","jpg",MskList[i]))
#
#	   image=image_read(ImgP)
#	   mask=image_read(mskP)
#	   Check=image_composite(image,mask,operator = "blend", compose_args="50")
#	   PathCheckImg=paste0(PathCheck,"\\",MskList[i])
#	   
#	   image_write(Check,PathCheckImg,format="jpg")
#	}
#}

# stopCluster(cl)