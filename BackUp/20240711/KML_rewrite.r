  
  
  library(dplyr)
  
  source("Modules/KMLwrite_function.r")

  



               labelInput #= "C:\\Users\\usato\\NFS_DB\\2024_138_OPP\\20240705_084420"
               Species="NFSAdult"

                           
				      
						
							 date1=substr(basename(labelInput),1,15)
					         
                             crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
					
   						 
							 Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")
							 Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
							 Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
					
					         kmlPathSave <- paste0(labelInput,"\\Predict\\",Species,"_", date1, ".kml")	
						

                                     
					
 						 
   if(dir.exists(Haulout_polygonDir)==T){Haulout_polygon<-list.files(Haulout_polygonDir,full.names=T,pattern="shp|kml")}
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon_pth<-list.files(Exlude_polygonDir,full.names=T,pattern="shp|kml")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon_pth <-list.files(Rookery_polygonDir,full.names=T,pattern=".shp|kml")}
	 


####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################

  if (file.exists(kmlPathSave)){Points= readOGR(kmlPathSave)} else {stop("No kml pred found")}
  if (file.exists(Exlude_polygon_pth)){ polygon_exlude=readOGR(Exlude_polygon_pth)} else {stop("No Exlude_polygon  found")}
  if (file.exists(Rookery_polygon_pth)){Rookery_polygon=readOGR(Rookery_polygon_pth)} else {stop("No Rookery_polygon found")}
 
 proj4string(Points) <- crs	
 proj4string(polygon_exlude) <- crs	
 proj4string(Rookery_polygon) <- crs	
 
                                                                        
 OverExlude = Points[is.na(over(Points,as(polygon_exlude,"SpatialPolygons"))),] 
 rk = OverExlude[!is.na(over(OverExlude,as(Rookery_polygon,"SpatialPolygons"))),]
 hl = OverExlude[is.na(over(OverExlude,as(Rookery_polygon,"SpatialPolygons"))),]

     rk=data.frame(rk)
	 hl =data.frame(hl) 
	 
	 
	hl$Description[hl$Description=="TF"]="AN"
	hl$Description[hl$Description=="F"]="Bch"
	
	rk$Description[rk$Description=="AN"]="TF"
	rk$Description[rk$Description=="Bch"]="F"
	 
	 ad=rbind(hl,rk)
  
  
   coords <- data.frame(lat= ad$coords.x1, lon=ad$coords.x2)   
    data   <- data.frame(Description= ad$Description)   # data
    Points1 <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  crs)
    Points1 =spTransform(Points1,crs)

    exludedAnimals=length(Points$Description)-length(Points1$Description)
	
	print(paste0(exludedAnimals, "  Fur seal were exluded from prediction by exlude polygons"))
	
    unlink(kmlPathSave)
    writeOGR(Points1,kmlPathSave,driver="KML", layer="Description")




