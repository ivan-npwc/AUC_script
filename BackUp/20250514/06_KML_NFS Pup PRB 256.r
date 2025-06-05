source("Modules/07.1_KMLwrite_function.r")

  library(dplyr)
	
                         Filter_by_Rookery_pol = TRUE 
					 
							 labelInput
							 date1=substr(basename(labelInput),1,15)
					         Species= "NFSPup256"
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",date1, ".csv")
					         dat<-read.csv(pathTablePoints)
                             if (Species =="NFSPup256") {BlobFemaleLimit=210}
							 kmlPathSave1=paste0(labelInput,"\\Predict\\","#",NFS_Pup_weight_pth,"#",date1,".kml")
							 ObserverPointP=list.files(paste0(labelInput,"\\Observer_count"), full.names=T, pattern="kml|shp")[1]
				if (is.na(ObserverPointP)==T){ ObserverPointP=list.files(paste0(labelInput,"\\Polygons\\All_layers"), full.names=T, pattern="kml|shp")[1]}			 
							 Rookery_pol=list.files(paste0(labelInput,"\\Polygons\\Model"), full.names=T, pattern="kml|shp")[1]
			  #  if (is.na(Rookery_pol)==T){ Rookery_pol=list.files(paste0(labelInput,"\\Polygons\\Model"), full.names=T, pattern="kml|shp")[1]}		 
 if (is.na(Rookery_pol)==F){
####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################
 dat<-dat[is.na(dat$lat) ==F ,] 
 dat3 <- data.frame(lat=dat$lat,   lon=dat$lon,  area=dat$s.area)
 ########################################################################
Pointsfilter=function (tble,pthPolygon) {
    coords <- data.frame(lat= tble$lon, lon=tble$lat)   
    data   <- data.frame(area= tble$area)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  CRS(crs))
	Points =spTransform(Points,CRS(crs))								 							 
    polygon_poly=readOGR(pthPolygon)  
    proj4string(polygon_poly) <- CRS(crs)
   
     pts = Points[!is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]
   
 #  index= Points %over% polygon_poly
 #  Points$index=index[,1]
 #  Points1=Points[is.na(Points$index)==F,]
 #  latlon=data.frame(coordinates(Points1))
  # PointsIN=data.frame(area=Points1$area,lat= latlon$lon ,lon=latlon$lat)
   return(pts)
   }

#############################################################################################################################
   PupPoints1=NULL
   if (Filter_by_Rookery_pol == TRUE) {
   PupPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_pol) 
   PupPoints1=data.frame(area=PupPoints$area,lat= PupPoints$lon ,lon=PupPoints$lat)
   } else { PupPoints1=dat3}
  
   for (k  in 1:length(PupPoints1$area)) {
     if (PupPoints1$area[k] <= BlobFemaleLimit)  {PupPoints1$age[k]="ERR"}
	# if (PupPoints1$area[k] <= 70)  {PupPoints1$age[k]="SmallError"}
     if (PupPoints1$area[k] > BlobFemaleLimit)   {PupPoints1$age[k]="P"} 
	 }

 

######################################################################################################### 	

KMLwrite(Img3=PupPoints1,kmlPathSave= kmlPathSave1)
############

autoC=PupPoints1 %>% group_by(age)%>% summarize(count=n())
Auto_Count=autoC$count
if (file.exists(ObserverPointP)) {
obs=readOGR(ObserverPointP)
print(paste0("Observer count ", length(obs)))
print(paste0("Auto count ", Auto_Count))
PercentFromObserverCount= round(abs(length(obs) -  Auto_Count)/length(obs)*100)
print(paste0("PercentFromObserverCount:    ", PercentFromObserverCount))
}
}