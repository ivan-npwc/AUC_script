source("Modules/KMLwrite_function.r")
# writeKMLResultFun=function (labelInput1=labelInput,
 #                            Species1=Species) {
							 
							labelInput1=labelInput
                           Species1=Species 
							 
							 
		                      library(spatialEco)					 
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",basename(labelInput), ".csv")
							
							 Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Houlout")
							 Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
							 Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
							 
	#if(dir.exists(Exlude_polygonDir)==F){ Exlude_polygonDir=gsub("Exlude","Exclude",Exlude_polygonDir)}                                				 
	if(dir.exists(Haulout_polygonDir)==F){Haulout_polygonDir=gsub("Houlout","Haulout",Haulout_polygonDir)}
	    						 
							 Haulout_polygon<<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon=list.files(Exlude_polygonDir,full.names=T,pattern=".shp")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon =list.files(Rookery_polygonDir,full.names=T,pattern=".shp")}
	
   

   kmlPathSave<<-paste0(labelInput1,"\\Predict\\",Species1, "_", basename(labelInput1), ".kml")
   csvPathSave= paste0(labelInput1,"\\Predict\\",Species1,"_", basename(labelInput1), ".csv")
  
   
 if (Species1 =="NFSAdult") {BlobFemaleLimit=410}
 if (Species1 =="SSLAdult") {BlobFemaleLimit=1200}
 if (Species1 =="NFSPup") {BlobFemaleLimit=0}
  if (Species1 =="WLRS") {BlobFemaleLimit=0}
  
 if (Species1 == "SSLPup") {BlobFemaleLimit=0
        if (length(Rookery_polygon) != 0) {Haulout_polygon<<-Rookery_polygon;Rookery_polygon <<-0}}
		
####################################################################################################
if (exists("Img3")==T) {rm(Img3)}
if (exists("Img3_rookery")==T) {rm(Img3_rookery)}
if (exists("PointsHoulout1")==T) {rm(PointsHoulout1)}
if (exists("PointsHoulout")==T) {rm(PointsHoulout)}

 dat<<-read.csv(pathTablePoints)
 
 #if (Species== "WLRS") {dat=dat[dat$MinDist> MinDist
 
 
 dat3 <<- data.frame(lat=dat$lon,   lon=dat$lat,  age=dat$s.area)
 ########################################################################### EXLUDE POLIGON
	if (length(Exlude_polygon) != 0) {
	coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(age= dat3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"					 
    PointsExlude <- point.in.poly(Points,polygon_exlude_polygon)
	
	
	 PointsExlude1=as.data.frame(PointsExlude)
    PointsExlude2=PointsExlude1[is.na(PointsExlude1[,2]) == T,]
	dat3=data.frame(age=PointsExlude2$age,lon=PointsExlude2$coords.x2,lat=PointsExlude2$coords.x1)  	
 }    
####################################################################################ROOKERY
if (length(Rookery_polygon) != 0) { 
             
   coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(age= dat3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
	 Rookery_polygonP=shapefile(Rookery_polygon)
     proj4string(Rookery_polygonP) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
     PointsRookery <- point.in.poly(Points,Rookery_polygonP)
	if (file.exists(Haulout_polygon)==T) { 
	 polygon_poly=shapefile(Haulout_polygon)
	 spTransform(polygon_poly,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

	 PointsRookery <- point.in.poly(PointsRookery,polygon_poly)
	 }
	 PointsRookery1 <<- as.data.frame(PointsRookery)
     PointsRookery2=PointsRookery1[is.na(PointsRookery1[,2]) == F,]
	 Img3_rookery=data.frame(age=PointsRookery2$age,lat=PointsRookery2$coords.x2,lon=PointsRookery2$coords.x1)  
	 
	 Img3_rookery$sex= "0"
     for (k  in 1:length(Img3_rookery$age)) {
     if (Img3_rookery$age[k] <= BlobFemaleLimit)  {Img3_rookery$sex[k]="F"}
     if (Img3_rookery$age[k] > BlobFemaleLimit)   {Img3_rookery$sex[k]="TF"} 
	 
	 }
}

  ########################################################################### HOULOUT POLYGON
    coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(age= dat3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
   polygon_poly=shapefile(Haulout_polygon)  
   index= Points %over% polygon_poly
   Points$index=index[,1]
   Points1=Points[is.na(Points$index)==F,]
   latlon=data.frame(coordinates(Points1))
   PointsHoulout2=data.frame(age=Points1$age,lat= latlon$lon ,lon=latlon$lat)
   
    if (Species1 == "SSLPup") {
   
    proj4string(polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    spTransform(polygon_poly,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
   
    Img0 <- point.in.poly(Points,polygon_poly) 
    Img=as.data.frame(Img0)
    Img2=Img[is.na(Img[,2]) == FALSE,]
	PointsHoulout2=data.frame(age=Img2$age,lat=Img2$coords.x2,lon=Img2$coords.x1)
	
	}
	
	

	if (exists("Img3_rookery")==F) {
	 PointsHoulout2$sex="0"
	 for (k  in 1:length(PointsHoulout2$age)) {
     if (PointsHoulout2$age[k] <= BlobFemaleLimit)  {PointsHoulout2$sex[k]="Bch"}
     if (PointsHoulout2$age[k] > BlobFemaleLimit)   {PointsHoulout2$sex[k]="An"} 
}} else {

     PointsHoulout1=PointsRookery1[is.na(PointsRookery1[,2]) == T,]
	 PointsHoulout2=data.frame(age=PointsHoulout1$age,lat=PointsHoulout1$coords.x2,lon=PointsHoulout1$coords.x1)  
   PointsHoulout2$sex="0"
   for (k  in 1:length(PointsHoulout2$age)) {
     if (PointsHoulout2$age[k] <= BlobFemaleLimit)  {PointsHoulout2$sex[k]="Bch"}
     if (PointsHoulout2$age[k] > BlobFemaleLimit)   {PointsHoulout2$sex[k]="An"} 
	 
} 
}

############################################################################
if (exists("Img3_rookery")== T) {PointsHoulout2=rbind(PointsHoulout2,Img3_rookery)}
PointsHoulout2=data.frame(lat=PointsHoulout2$lat, lon= PointsHoulout2$lon,age=PointsHoulout2$sex)
if (Species1 == "NFSPup") {PointsHoulout2$age="P"}
if (Species1 == "SSLPup") {PointsHoulout2$age="P"}
KMLwrite(Img3=PointsHoulout2,kmlPathSave=kmlPathSave)
#}
                                                                          	# reverse coordinate !!!
####################################################################################################################### ROOKERY POLYGON
# writeKMLResultFun()