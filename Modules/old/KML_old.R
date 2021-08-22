library(spatialEco)
source("Modules/KMLwrite_function.r")
#################################
 date=basename(labelInput)
 kmlPathSave_Houlout<<-paste0(labelInput,"\\Predict\\Houlout_PointsPredict_", date, ".kml") 
  kmlPathSave_Garem<<-paste0(labelInput,"\\Predict\\Garem_PointsPredict_", date, ".kml") 
  pathTablePoints<<-paste0(labelInput, "\\Predict\\Houlout_BlobTable_GEO_",date, ".csv")
##########################################################################
 writeKMLResultFun=function ( 
                           Haulout_polygon, 
                           Exlude_polygon,
                           pathTablePoints,
                          kmlPathSave_Houlout){
##################
if (exists("Img3")==T) {rm(Img3)}
if (exists("Img3_rookery")==T) {rm(Img3_rookery)}
if (exists("Img3_houlout")==T) {rm(Img3_houlout)}
 dat<<-read.csv(pathTablePoints)
  ###########
 dat$age="0"
   for (k  in 1:length(dat$s.area)) {
     if (dat$s.area[k] <= 500)                       {dat$age[k]=1}
   #  if (dat$s.area[k] > 400 & dat$s.area[k] < 600)  {dat$age[k]=2} 
     if (dat$s.area[k] > 500)                       {dat$age[k]=3} 
}

 dat3=data.frame(lat=dat$lon,   lon=dat$lat,  age=dat$age)
 
  
  
  ############################################################################################### HOULOUT POLYGON
    coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(age= dat3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
   polygon_poly=shapefile(Haulout_polygon)
   proj4string(polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"					 
    Img0 <- point.in.poly(Points,polygon_poly) 
    Img=as.data.frame(Img0)
    Img2=Img[is.na(Img[,2]) == FALSE,]
	Img3=data.frame(age=Img2$age,lat=Img2$coords.x2,lon=Img2$coords.x1)  
################################################################################################  EXLUDE POLIGON
	if (file.exists(Exlude_polygon)==T) {
	Img3=NULL
	coords <- data.frame(lat= Img2$coords.x1, lon=Img2$coords.x2)   
    data   <- data.frame(age= Img2$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"					 
    PointsExlude <- point.in.poly(Points,polygon_exlude_polygon)
	
	
	 PointsExlude1=as.data.frame(PointsExlude)
    PointsExlude2=PointsExlude1[is.na(PointsExlude1[,2]) == T,]
	Img3=data.frame(age=PointsExlude2$age,lat=PointsExlude2$coords.x2,lon=PointsExlude2$coords.x1)  
	
 }                                                                                         	# reverse coordinate !!!
 ####################################################################################################################### ROOKERY POLYGON
 if (file.exists(Rookery_polygon)==T) {
   coords <- data.frame(lat= Img3$lon, lon=Img3$lat)   
    data   <- data.frame(age= Img3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
 
 
	Rookery_polygonP=shapefile(Rookery_polygon)
    proj4string(Rookery_polygonP) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
 
     PointsRookery <- point.in.poly(Points,Rookery_polygonP)
	 PointsRookery1=as.data.frame(PointsRookery)
	 
     PointsRookery2=PointsRookery1[is.na(PointsRookery1[,2]) == F,]
	 
	 Img3_rookery=data.frame(age=PointsRookery2$age,lat=PointsRookery2$coords.x2,lon=PointsRookery2$coords.x1)  
	 KMLwrite(Img3=Img3_rookery,kmlPathSave=kmlPathSave_Garem)
	 
	 PointsHoulout2=PointsRookery1[is.na(PointsRookery1[,2]) == T,]
	 Img3_houlout=data.frame(age=PointsHoulout2$age,lat=PointsHoulout2$coords.x2,lon=PointsHoulout2$coords.x1)  
	 KMLwrite(Img3=Img3_houlout,kmlPathSave=kmlPathSave_Houlout)
	 } else {KMLwrite(Img3=Img3,kmlPathSave=kmlPathSave_Houlout)}
	}
#######################################################################################################################
	 writeKMLResultFun( Haulout_polygon, 
                        Exlude_polygon,
                         pathTablePoints,
                         kmlPathSave_Houlout)
						 
	 if (Pup==T) {
	Rookery_polygon <<- "_"
	writeKMLResultFun( Haulout_polygon, 
                        Exlude_polygon,
                         pathTablePoints=paste0(labelInput, "\\Predict\\Pup_BlobTable_GEO_",date, ".csv"),
                         kmlPathSave_Houlout=paste0(labelInput,"\\Predict\\Pup_PointsPredict_", date, ".kml"))}			 
						 