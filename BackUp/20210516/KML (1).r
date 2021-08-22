source("Modules/KMLwrite_function.r")
#library(spatialEco)	

							 labelInput
					         Species
                            # MinDist=0.1  # for WLRS remove double points
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",basename(labelInput), ".csv")
							 Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")
							 Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
							 Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
							 kmlPathSave<<-paste0(labelInput,"\\Predict\\",Species, "_", basename(labelInput), ".kml")
                             csvPathSave= paste0(labelInput,"\\Predict\\",Species,"_", basename(labelInput), ".csv")
							 
 						 
   if(dir.exists(Haulout_polygonDir)==T){Haulout_polygon<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")}
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon<-list.files(Exlude_polygonDir,full.names=T,pattern=".shp")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon <-list.files(Rookery_polygonDir,full.names=T,pattern=".shp")}
	 
   
 if (Species =="NFSAdult") {BlobFemaleLimit=410}
 if (Species =="SSLAdult") {BlobFemaleLimit=1200}
 if (Species =="NFSPup") {BlobFemaleLimit=0}
 if (Species =="WLRS") {BlobFemaleLimit=0}
 if (Species == "SSLPup") {BlobFemaleLimit=0}
####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints=NULL
###################
 dat<-read.csv(pathTablePoints)
 dat<-dat[is.na(dat$lat) ==F,] 
 #if (Species== "WLRS") {dat=dat[dat$MinDist> MinDist,]}
 dat3 <- data.frame(lat=dat$lon,   lon=dat$lat,  age=dat$s.area)
 ########################################################################
Pointsfilter=function (tble,pthPolygon) {
coords <- data.frame(lat= tble$lat, lon=tble$lon)   
    data   <- data.frame(age= tble$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
   polygon_poly=shapefile(pthPolygon)  
   proj4string(polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
   index= Points %over% polygon_poly
   Points$index=index[,1]
   Points1=Points[is.na(Points$index)==F,]
   latlon=data.frame(coordinates(Points1))
   PointsIN=data.frame(age=Points1$age,lat= latlon$lon ,lon=latlon$lat)
   return(PointsIN)
   }

 ###################################################################################################################### EXLUDE POLIGON for all
	if (length(Exlude_polygon) != 0) {                                                                                #
	coords <- data.frame(lat= dat3$lat, lon=dat3$lon)                                                                 #   
    data   <- data.frame(age= dat3$age)   # data                                                                      #
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"		
	
    PointsExlude <- point.in.poly(Points,polygon_exlude_polygon)
	PointsExlude1=as.data.frame(PointsExlude)
    PointsExlude2=PointsExlude1[is.na(PointsExlude1[,2]) == T,]
	dat3 <<- data.frame(lat=PointsExlude2$coords.x1,lon=PointsExlude2$coords.x2,age=PointsExlude2$age)  	
 }    
#############################################################################################################################   ROOKERY for  NFS SSL  adult
if (length(Rookery_polygon) != 0  & Species %in% c("NFSadult","SSLadult")) {   
RookeryPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_polygon) 
	 RookeryPoints$sex= "0"
     for (k  in 1:length(RookeryPoints$age)) {
     if (RookeryPoints$age[k] <= BlobFemaleLimit)  {RookeryPoints$sex[k]="F"}
     if (RookeryPoints$age[k] > BlobFemaleLimit)   {RookeryPoints$sex[k]="TF"} 
	 }
}
  #######################################################################################################################     HAULOUT NFS SSL WLRS adult
 if (length(Haulout_polygon) != 0 & Species %in% c("NFSadult","SSLadult","WLRS")) { 
   PointsHoulout2=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon) 
   if (Species=="WLRS"){PointsHoulout2$sex="U";PointsHoulout2$age="U"} else {
    PointsHoulout2$sex="0"
	 for (k  in 1:length(PointsHoulout2$age)) {
     if (PointsHoulout2$age[k] <= BlobFemaleLimit)  {PointsHoulout2$sex[k]="Bch"}
     if (PointsHoulout2$age[k] > BlobFemaleLimit)   {PointsHoulout2$sex[k]="An"}
   }
   }
   }
################################################################################################################# ROOKERY SSL NFS PUP   
   if (Species %in% c("SSLPup","NFSPup")   & length(Rookery_polygon) != 0) {
   PupPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_polygon) 
   PupPoints$sex="P"
 }
######################################################################################################### 	
PointsToWrite=rbind(PointsHoulout2,RookeryPoints,PupPoints)
KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)

