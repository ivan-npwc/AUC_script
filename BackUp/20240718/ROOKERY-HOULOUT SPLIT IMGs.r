		  library(sp)
		  library(rgdal)
		  
		  labelInput = "H:\\2024_138_OPP\\20240716_073620"
		  Species    = "NFSAdult"
		  date1=substr(basename(labelInput),1,15)


		crs = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
		Pth_GeoInfoImg=paste0(labelInput,"\\", date1,"_table.csv")
		HouloutFolder=paste0(labelInput,"\\Predict\\Haulout");listImgs=list.files(HouloutFolder, full.names=T)
		RKFolder=paste0(labelInput,"\\Predict\\Rookery");dir.create(RKFolder, showWarnings = F)

		DirRPol=paste0(labelInput,"\\Polygons\\Rookery");PthPolR=list.files(DirRPol,full.names=T, pattern=".shp|kml")
		if (length(PthPolR) !=0){RookeryPol=readOGR(PthPolR);proj4string(RookeryPol) <- crs} else {stop("No Rookery polygon found")}
		TblAgeRef=NULL
		
		#############################################################################################################
		GeoInfoImg=read.csv(Pth_GeoInfoImg)
		GeoInfoImg$link1=paste0(GeoInfoImg$date,"_",GeoInfoImg$link)
		ImgList=GeoInfoImg[GeoInfoImg$link1 %in% basename(listImgs),]
		ImgList$pth=listImgs



		coords1 <- data.frame(lat= ImgList$west, lon=ImgList$south)   
		data1   <- data.frame(pth=ImgList$pth, Rookery="Houlout")   
		imgsCoords1 <- SpatialPointsDataFrame(coords = coords1,data = data1, proj4string = crs)
		
		coords2 <- data.frame(lat= ImgList$west, lon=ImgList$north)   
		data2   <- data.frame(pth=ImgList$pth, Rookery="Houlout")   
		imgsCoords2 <- SpatialPointsDataFrame(coords = coords2,data = data2, proj4string = crs)
			
		coords3 <- data.frame(lat= ImgList$east, lon=ImgList$south)   
		data3   <- data.frame(pth=ImgList$pth, Rookery="Houlout")   
		imgsCoords3 <- SpatialPointsDataFrame(coords = coords3,data = data3, proj4string = crs)	
		
		coords4 <- data.frame(lat= ImgList$east, lon=ImgList$north)   
		data4   <- data.frame(pth=ImgList$pth, Rookery="Houlout")   
		imgsCoords4 <- SpatialPointsDataFrame(coords = coords4,data = data4, proj4string = crs)
			
		imgsCoords=rbind(imgsCoords1,imgsCoords2,imgsCoords3,imgsCoords4)	
		imgsCoords=	spTransform(imgsCoords,crs)
		proj4string(imgsCoords) <- crs

		###############################################################
		
		RookeryImgs = imgsCoords[!is.na(over(imgsCoords,as(RookeryPol,"SpatialPolygons"))),]
		
		imgsCoords$Rookery[imgsCoords$pth %in% RookeryImgs$pth] = "Rookery"
		
		
		
		
		