  
  
  library(dplyr)
  
  source("Modules/KMLwrite_function.r")

  



               labelInput #= "C:\\Users\\usato\\NFS_DB\\2024_138_OPP\\20240705_084420"
               Species="NFSAdult"

                           
				      
						
							 date1=substr(basename(labelInput),1,15)
					         
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",date1, ".csv")
   						 
							 Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")
							 Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
							 Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
					
					       #  Path_read  <- paste0(labelInput,"\\Predict\\",date1,"_NFSAdult_HAULOUT.kml")
					         kmlPathSave <- paste0(labelInput,"\\Predict\\",Species,"_", date1, ".kml")	
						

                                     
					
 						 
   if(dir.exists(Haulout_polygonDir)==T){Haulout_polygon<-list.files(Haulout_polygonDir,full.names=T,pattern="shp|kml")}
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon<-list.files(Exlude_polygonDir,full.names=T,pattern="shp|kml")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon <-list.files(Rookery_polygonDir,full.names=T,pattern=".shp|kml")}
	 


####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################

 dat=readOGR(kmlPathSave)
 dat1=data.frame(dat)
 dat3 <- data.frame(lat = dat1$coords.x1,  lon = dat1$coords.x2, area = dat1$Description) 

 #######
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
    polygon_exlude_polygon=readOGR(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- CRS(crs)		
	
    Pointagelude <- point.in.poly(Points,polygon_exlude_polygon)
	Pointagelude1=as.data.frame(Pointagelude)
    Pointagelude2=Pointagelude1[is.na(Pointagelude1[,2]) == T,]
	dat3 <<- data.frame(lat=Pointagelude2$coords.x1,lon=Pointagelude2$coords.x2,area=Pointagelude2$area)  	
 }    
#############################################################################################################################   ROOKERY for  NFS SSL  adult
if (length(Rookery_polygon) != 0) { 

  PRAn=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon)                                   # first filter throo haulout, animals presence polygon
  RP=Pointsfilter(tble=PRAn,pthPolygon=Rookery_polygon)
  if (length(RP)>0){
  RP1=data.frame(RP)
 
    RookeryPoints=data.frame(area=RP1$area,lat= RP1$lon ,lon=RP1$lat)
 	 
	 RookeryPoints$age=RookeryPoints$area
	 RookeryPoints$age[RookeryPoints$age=="AN"]="TF"
	 RookeryPoints$age[RookeryPoints$age=="Bch"]="F"
	 }
	 }
	 
  #######################################################################################################################     HAULOUT NFS SSL WLRS adult
 if (length(Haulout_polygon) != 0) { 
   PH1=Pointsfilter(tble=dat3,pthPolygon=Haulout_polygon) 
   
   if (length(Rookery_polygon) != 0) {            # WE NEED EXLUDE ROOKERY FROM HAULOUT !!!!
	
	RP3= shapefile(Rookery_polygon)  
    proj4string(RP3) <- CRS(crs)
	PH1=PH1[is.na(over(PH1,as(RP3,"SpatialPolygons"))),]
	}
		
  if (length(PH1)>0){	
	PH2=data.frame(PH1)
	
 
	PointsHoulout2=data.frame(area=PH2$area,lat= PH2$lon ,lon=PH2$lat)
	 
	PointsHoulout2$age=PointsHoulout2$area
	PointsHoulout2$age[PointsHoulout2$age=="TF"]="AN"
	PointsHoulout2$age[PointsHoulout2$age=="F"]="Bch"
   }
   }

	
######################################################################################################### 	
PointsToWrite<<-rbind(PointsHoulout2,RookeryPoints)
print(length(PointsToWrite$age))
print(length(dat1$area))

KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)




