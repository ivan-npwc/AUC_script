source("Modules/KMLwrite_function.r")
#library(spatialEco)	

							 labelInput
					         Species
                             MinDist=0.6  # for WLRS remove double points
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
 if (Species =="WLRS") {BlobFemaleLimit=40}
 if (Species == "SSLPup") {BlobFemaleLimit=0}
####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints=NULL
PointsToWrite=NULL
###################
 dat<-read.csv(pathTablePoints)
 dat<-dat[is.na(dat$lat) ==F ,] 

 
if (Species== "WLRS") {

dat=dat[dat$MinDist > MinDist,]
dat<-dat[is.na(dat$lat) ==F ,] 
#More=dat[dat$MinDist > MinDist,]
#Less=dat[dat$MinDist < MinDist,]

length(dat$MinDist)
length(More$MinDist)
 
}
 dat3 <- data.frame(lat=dat$lon,   lon=dat$lat,  area=dat$s.area)
 ########################################################################
Pointsfilter=function (tble,pthPolygon) {
coords <- data.frame(lat= tble$lat, lon=tble$lon)   
    data   <- data.frame(area= tble$area)   # data
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
   PointsIN=data.frame(area=Points1$area,lat= latlon$lon ,lon=latlon$lat)
   return(PointsIN)
   }

 ###################################################################################################################### EXLUDE POLIGON for all
	if (length(Exlude_polygon) != 0) {                                                                                #
	coords <- data.frame(lat= dat3$lat, lon=dat3$lon)                                                                 #   
    data   <- data.frame(area= dat3$area)   # data                                                                      #
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"		
	
    Pointagelude <- point.in.poly(Points,polygon_exlude_polygon)
	Pointagelude1=as.data.frame(Pointagelude)
    Pointagelude2=Pointagelude1[is.na(Pointagelude1[,2]) == T,]
	dat3 <<- data.frame(lat=Pointagelude2$coords.x1,lon=Pointagelude2$coords.x2,area=Pointagelude2$area)  	
 }    
#############################################################################################################################   ROOKERY for  NFS SSL  adult
if (length(Rookery_polygon) != 0  & Species %in% c("NFSAdult","SSLAdult")) {   
RookeryPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_polygon) 
	 RookeryPoints$age= "0"
     for (k  in 1:length(RookeryPoints$area)) {
     if (RookeryPoints$area[k] <= BlobFemaleLimit)  {RookeryPoints$age[k]="F"}
     if (RookeryPoints$area[k] > BlobFemaleLimit)   {RookeryPoints$age[k]="TF"} 
	 }
}
  #######################################################################################################################     HAULOUT NFS SSL WLRS adult
 if (length(Haulout_polygon) != 0 & Species %in% c("NFSAdult","SSLAdult","WLRS")) { 
   PointsHoulout2=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon) 
 #  if (Species=="WLRS"){PointsHoulout2$age="U";PointsHoulout2$area="U"} else {
    PointsHoulout2$age="0"
	 for (k  in 1:length(PointsHoulout2$area)) {
     if (PointsHoulout2$area[k] <= BlobFemaleLimit)  {PointsHoulout2$age[k]="Bch"}
     if (PointsHoulout2$area[k] > BlobFemaleLimit)   {PointsHoulout2$age[k]="An"}
   }
#   }
   }
################################################################################################################# ROOKERY SSL NFS PUP   
   if (Species %in% c("SSLPup","NFSPup")   & length(Rookery_polygon) != 0) {
   PupPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_polygon) 
   PupPoints$age="P"
 }
######################################################################################################### 	
PointsToWrite=rbind(PointsHoulout2,RookeryPoints,PupPoints)
KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)

