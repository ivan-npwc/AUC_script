library(raster)
library(spatialEco)

#labelInput= "E:\\NFS_2019\\2019_138_OPP\\20190611"
res = 0.0001 # size of model polygon
nP = 40
type = "nonaligned" # type of location of polygons  (random,stratified,nonaligned,regular)

HaulPolygonsDir=paste0(labelInput,"\\Polygons\\Houlout");if (dir.exists(HaulPolygonsDir)==F){HaulPolygonsDir=paste0(labelInput,"\\Polygons\\Haulout")}
if (dir.exists(HaulPolygonsDir)==T){
  HaulPolygonsPTH=list.files(HaulPolygonsDir,full.names=T, pattern="shp")
  ModelPolDir=paste0(labelInput,"\\Polygons\\Model");if (dir.exists(ModelPolDir)==F){dir.create(ModelPolDir)}
  Save=paste0(ModelPolDir,"\\Model.shp")
  HaulPolygons=shapefile(HaulPolygonsPTH)
  samplePoints=spsample(HaulPolygons,n = nP,type = type)

  coordinate=data.frame(coordinates(samplePoints));coordinate$name="No";coordinates(coordinate) <- ~x1+x2
  RandomHexagons= hexagons(coordinate, res = res)
  
  IDpol=na.omit(over(coordinate,RandomHexagons))
  hexagons=RandomHexagons[RandomHexagons$HEXID %in% IDpol$HEXID,]
  proj4string(hexagons) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
                          
  #plot(hexagons)	
  writeOGR(hexagons,  Save, layer ="Model", driver="ESRI Shapefile")
}