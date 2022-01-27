


library(spatialEco)
library(dplyr)
library(raster)
library(RSQLite)
source("Modules/01_create_tile_polygons.r")

labelInput
Species
site
System_data
crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

DirLocSites=paste0(System_data,"\\polygons_local_sites\\",site);pthLcSt=list.files(DirLocSites,full.names=T,pattern = "kml|shp")
locSite=readOGR(pthLcSt)
proj4string(locSite) <- CRS(crs)

HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Houlout")
RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
ObserverPointDIR=paste0(labelInput,"\\Observer_count")
########################################################################################### check if exists subdir	
listFiles=list.files(labelInput)
GoogleKMZ=paste0(basename(labelInput),".kmz")
checkSubDir= GoogleKMZ %in%  listFiles 
if (checkSubDir==T) {listLabelInput=labelInput}
if (checkSubDir==F) {listLabelInput=list.dirs(labelInput,full.names=T,recursive =F)}
##########################################@#########################################spp data frame for tiles google kmz
for (i in 1:length(listLabelInput)) {
  labelInputSub=listLabelInput[i]
  SpP_dataW=create_tile_polygons(labelInput=labelInputSub)
  if (i==1){SpP_dataALL=SpP_dataW}
  if (i!=1){SpP_dataALL=rbind(SpP_dataALL,SpP_dataW)}	
}
#############################################################################CHECK TILES OVER LOCAL SITE
proj4string(SpP_dataALL)<- CRS(crs)
#  plot(locSite, col=2)
# plot(SpP_dataALL,add=T)

completenes=NULL
NmLcSt=as.character(data.frame(locSite)[,2])
for (i in 1:length(locSite)) {
  localSite= locSite[i,]
  NameLocalSite=NmLcSt[i]
  pts = SpP_dataALL[!is.na(over(SpP_dataALL,as(localSite,"SpatialPolygons"))),]
  # plot(pts, col=2); plot(localSite, add=T)
  if (length(pts) !=0){	
    AreaTiles=sum(area(pts))
    AreaLocSite= area(localSite)
    Coverege= AreaTiles/AreaLocSite*100
  }
  if (length(pts) ==0){Coverege=0}	
  row1=data.frame(data=basename(labelInput),site=site, local_site=NameLocalSite,coverage=Coverege)
  completenes=rbind(completenes,row1)
}

completenes <<- completenes




























