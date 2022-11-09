source("Modules/KMLwrite_function.r")

  library(dplyr)

#library(spatialEco)	
                             clastering=F
				#	if (Species=="WLRS"){clastering=T}		 
							 minDistance =0.2 # min distance 0.1 m minDistance
							 labelInput
							 date1=substr(basename(labelInput),1,15)
					         Species
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",date1, ".csv")
							 Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")
							 Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
							 Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
					if (ModelCheckAlg==F){
					kmlPathSave <<- paste0(labelInput,"\\Predict\\",Species,"_", date1, ".kml")
					kmlPathSave1 <- kmlPathSave
                    csvPathSave= gsub(".kml",".csv",kmlPathSave)
					}	
                  if (ModelCheckAlg==T){
					kmlPathSave <<- CheckModelkmlPathSave
                    csvPathSave <<- CheckModelcsvPathSave
					}



					
 						 
   if(dir.exists(Haulout_polygonDir)==T){Haulout_polygon<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")}
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon<-list.files(Exlude_polygonDir,full.names=T,pattern=".shp")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon <-list.files(Rookery_polygonDir,full.names=T,pattern=".shp")}
	 
   
 if (Species =="NFSAdult") {BlobFemaleLimit=410;BlobSAlimit=320; limitSmallError=10}
 
 if (Species =="SSLAdult") {BlobFemaleLimit=1200;BlobSAlimit=1200; limitSmallError=100}
 if (Species =="NFSPup") {BlobFemaleLimit=70}
 if (Species =="WLRS") {BlobFemaleLimit=40}
 if (Species == "SSLPup") {BlobFemaleLimit=300; limitSmallError=100}
  if (Species == "LRG") {BlobFemaleLimit=600}  # 900- junior
####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################
 dat<-read.csv(pathTablePoints)
 dat<-dat[is.na(dat$lat) ==F ,] 
 dat3 <- data.frame(lat=dat$lon,   lon=dat$lat,  area=dat$s.area)
 #################################################################
 if ( clastering==T) {
 options(warn=-1)
    coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(area= dat3$area)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  CRS(crs))
	Points =spTransform(Points,CRS(crs))
    mdist <- distm(Points)
	hc <- hclust(as.dist(mdist), method="complete")
	Points$clust <- cutree(hc, h=minDistance)
dat4=data.frame(Points) 
dat4[,5] = NULL
dat5= dat4 %>% 
			 group_by(clust)%>% 
			 summarise(lat=mean(lat,na.rm = T),lon=mean(lon,na.rm = T),  area = mean(area,na.rm = T))		 
dat3=data.frame(dat5) 
options(warn=0)
 }
 ########################################################################
Pointsfilter=function (tble,pthPolygon) {
coords <- data.frame(lat= tble$lat, lon=tble$lon)   
    data   <- data.frame(area= tble$area)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  CRS(crs))
	Points =spTransform(Points,CRS(crs))								 							 
    polygon_poly=shapefile(pthPolygon)  
    proj4string(polygon_poly) <- CRS(crs)
   
     pts = Points[!is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]
   
 #  index= Points %over% polygon_poly
 #  Points$index=index[,1]
 #  Points1=Points[is.na(Points$index)==F,]
 #  latlon=data.frame(coordinates(Points1))
  # PointsIN=data.frame(area=Points1$area,lat= latlon$lon ,lon=latlon$lat)
   return(pts)
   }

 ###################################################################################################################### EXLUDE POLIGON for all
	if (length(Exlude_polygon) != 0) {                                                                                #
	coords <- data.frame(lat= dat3$lat, lon=dat3$lon)                                                                 #   
    data   <- data.frame(area= dat3$area)   # data                                                                      #
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = CRS(crs))
	Points =spTransform(Points,CRS(crs))
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- CRS(crs)		
	
    Pointagelude <- point.in.poly(Points,polygon_exlude_polygon)
	Pointagelude1=as.data.frame(Pointagelude)
    Pointagelude2=Pointagelude1[is.na(Pointagelude1[,2]) == T,]
	dat3 <<- data.frame(lat=Pointagelude2$coords.x1,lon=Pointagelude2$coords.x2,area=Pointagelude2$area)  	
 }    
#############################################################################################################################   ROOKERY for  NFS SSL  adult
if (length(Rookery_polygon) != 0  & Species %in% c("NFSAdult","SSLAdult")) { 

  PRAn=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon)                                   # first filter throo haulout, animals presence polygon

RP=Pointsfilter(tble=PRAn,pthPolygon=Rookery_polygon)
  if (length(RP)>0){
  RP1=data.frame(RP)
   RookeryPoints=data.frame(area=RP1$area,lat= RP1$lon ,lon=RP1$lat)
	 RookeryPoints$age= "0"
     for (k  in 1:length(RookeryPoints$area)) {
     if (RookeryPoints$area[k] <= BlobFemaleLimit)  {RookeryPoints$age[k]="F"}
     if (RookeryPoints$area[k] > BlobFemaleLimit)   {RookeryPoints$age[k]="TF"} 
	 if (RookeryPoints$area[k] <= limitSmallError)  {RookeryPoints$age[k]="SmallError"}
	 }
     }
	 }
  #######################################################################################################################     HAULOUT NFS SSL WLRS adult
 if (length(Haulout_polygon) != 0 & Species %in% c("NFSAdult","SSLAdult","WLRS","LRG")) { 
   PH1=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon) 
   
   if (length(Rookery_polygon) != 0) {            # WE NEED EXLUDE ROOKERY FROM HAULOUT !!!!
	RP3= shapefile(Rookery_polygon)  
   proj4string(RP3) <- CRS(crs)
	PH1=PH1[is.na(over(PH1,as(RP3,"SpatialPolygons"))),]
	}
	
	
if (length(PH1)>0){	
	PH2=data.frame(PH1)
	PointsHoulout2=data.frame(area=PH2$area,lat= PH2$lon ,lon=PH2$lat)
 #  if (Species=="WLRS"){PointsHoulout2$age="U";PointsHoulout2$area="U"} else {
    PointsHoulout2$age="0"
	 for (k  in 1:length(PointsHoulout2$area)) {
     if (PointsHoulout2$area[k] <= BlobSAlimit)  {PointsHoulout2$age[k]="Bch"}
     if (PointsHoulout2$area[k] > BlobSAlimit)   {PointsHoulout2$age[k]="AN"}
	 if (PointsHoulout2$area[k] <= limitSmallError)  {PointsHoulout2$age[k]="SmallError"}
	 
	 
	 
	 
   }
#   }
   }
   }
################################################################################################################# ROOKERY SSL NFS PUP   
   PupPoints1=NULL
   if (Species == "NFSPup") {
   PupPoints=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon) 
   PupPoints1=data.frame(area=PupPoints$area,lat= PupPoints$lon ,lon=PupPoints$lat)
   for (k  in 1:length(PupPoints1$area)) {
     if (PupPoints1$area[k] <= BlobFemaleLimit)  {PupPoints1$age[k]="P"}
	# if (PupPoints1$area[k] <= 70)  {PupPoints1$age[k]="SmallError"}
     if (PupPoints1$area[k] > BlobFemaleLimit)   {PupPoints1$age[k]="P"} 
	 }
 #  if (Species == "SSLPup") {PupPoints1=PupPoints1 %>% filter( area > limitSmallError)} 
 }
 ######################################################################################################
    if (Species =="SSLPup") {
	if(UseRookeryOnly){
	   if (length(Rookery_polygon)==0){stop("No Rookery polygons found. Create Rookery polygons or use Haulout polygons")}
	 PupPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_polygon)} 
	if(UseRookeryOnly==F){ PupPoints=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon)} 
  
  PupPoints1=data.frame(area=PupPoints$area,lat= PupPoints$lon ,lon=PupPoints$lat)
   PupPoints1$age="P"
   for (k  in 1:length(PupPoints1$area)) {
     if (PupPoints1$area[k] <= BlobFemaleLimit)  {PupPoints1$age[k]="P"}
	 if (PupPoints1$area[k] <= 70)  {PupPoints1$age[k]="SmallError"}
     if (PupPoints1$area[k] > BlobFemaleLimit)   {PupPoints1$age[k]="BigError"} 
   	 }
    PupPoints2=PupPoints1[PupPoints1$age != "SmallError",]	
	PupPoints2$area=NULL
	PupPoints2$age="P"
	
#########	
    rkPTH = paste0(labelInput,"\\Predict\\",date1,"_", "SSLAdult","_ROOKERY.csv")
    hPth = paste0(labelInput,"\\Predict\\",date1,"_", "SSLAdult","_HAULOUT.csv")
    kmlPathSave1=paste0(labelInput,"\\Predict\\","SSLAdult", "_", date1, ".kml")
 rk=NULL;Hl=NULL
 if (file.exists(rkPTH)){rk=read.csv(rkPTH)}
Hl=read.csv(hPth)
ad=rbind(rk,Hl)
adults1=data.frame(lat =as.numeric(ad$lat),     lon=as.numeric(ad$lon),  age=ad$age)
adults1$age[adults1$age=="An"]="AN"
adults1$age[adults1$age=="Sa"]="Sa"

PupPoints3=rbind(PupPoints2,adults1)
KMLwrite(Img3=PupPoints1,kmlPathSave=kmlPathSave)
	}
######################################################################################################### 	
PointsToWrite=rbind(PointsHoulout2,RookeryPoints,PupPoints3,PupPoints1)
KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave1)


