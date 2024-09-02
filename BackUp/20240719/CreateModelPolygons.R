library(raster)
library(spatialEco)


crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
res = 0.00015 # size of model polygon
nP = 20
type = "nonaligned" # type of location of polygons  (random,stratified,nonaligned,regular)

HaulPolygonsDir=paste0(labelInput,"\\Polygons\\Houlout");if (dir.exists(HaulPolygonsDir)==F){HaulPolygonsDir=paste0(labelInput,"\\Polygons\\Haulout")}

if (dir.exists(HaulPolygonsDir)==T){
  HaulPolygonsPTH=list.files(HaulPolygonsDir,full.names=T, pattern="shp")
  ModelPolDir=paste0(labelInput,"\\Polygons\\Model");if (dir.exists(ModelPolDir)==F){dir.create(ModelPolDir)}
  Save=paste0(ModelPolDir,"\\Model.kml")
  HaulPolygons=shapefile(HaulPolygonsPTH)
   proj4string(HaulPolygons) <- crs
   
  samplePoints=spsample(HaulPolygons,n = nP,type = type)
  coordinate=data.frame(coordinates(samplePoints));coordinate$name="No";coordinates(coordinate) <- ~x1+x2
  RandomHexagons= hexagons(coordinate, res = res)
  IDpol=na.omit(over(coordinate,RandomHexagons))
  hexagons=RandomHexagons[RandomHexagons$HEXID %in% IDpol$HEXID,]
  proj4string(hexagons) <- crs
                          
	sum(area(hexagons))/area(HaulPolygons)*100		# percent of model polygonn over haulout polygon		  
						  
		 
	      writeOGR(hexagons,  Save, "Model", "KML")
		 

  
}
################################################################################################

crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
res = 0.00015 # size of model polygon
nP = 10
type = "nonaligned" # type of location of polygons  (random,stratified,nonaligned,regular)

HaulPolygonsDir = paste0(labelInput,"\\Polygons\\Houlout");if (dir.exists(HaulPolygonsDir)==F){HaulPolygonsDir=paste0(labelInput,"\\Polygons\\Rookery")}
 OPTPolDir=paste0(labelInput,"\\Polygons\\Prediction optimisation");if (dir.exists(OPTPolDir)==F){dir.create(OPTPolDir)}
 
if (dir.exists(HaulPolygonsDir)==T){
  HaulPolygonsPTH=list.files(HaulPolygonsDir,full.names=T, pattern="shp")
 
  Save=paste0(OPTPolDir,"\\Prediction optimisation.kml")
  HaulPolygons=shapefile(HaulPolygonsPTH)
   proj4string(HaulPolygons) <- crs
   
  samplePoints=spsample(HaulPolygons,n = nP,type = type)
  coordinate=data.frame(coordinates(samplePoints));coordinate$name="No";coordinates(coordinate) <- ~x1+x2
  RandomHexagons= hexagons(coordinate, res = res)
  IDpol=na.omit(over(coordinate,RandomHexagons))
  hexagons=RandomHexagons[RandomHexagons$HEXID %in% IDpol$HEXID,]
  proj4string(hexagons) <- crs
                          
	percent=sum(area(hexagons))/area(HaulPolygons)*100		# percent of model polygonn over haulout polygon		  
	
	print(percent)

	      writeOGR(hexagons,  Save, "Prediction optimisation", "KML")
 
}