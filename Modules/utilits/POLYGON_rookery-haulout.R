
library(spatialEco)
library(filesstrings)
PointsPorderImage=NULL
coords=NULL
data=NULL
crs=NULL
PointsImg=NULL
#######################################
  table_img_pth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
  table_img=read.csv(table_img_pth)
  table_img$link1=paste0(table_img$date , "_", table_img$link)

   
   CombinationOne=data.frame(lat= table_img$west, lon=table_img$south,link= table_img$link1)
   CombinationTo=data.frame(lat= table_img$west, lon=table_img$north,link= table_img$link1)
   CombinationThry=data.frame(lat= table_img$east, lon=table_img$south,link= table_img$link1)
   CombinationFor=data.frame(lat= table_img$east, lon=table_img$south,link= table_img$link1)
   PointsPorderImage=rbind(CombinationOne,CombinationTo,CombinationThry,CombinationFor)
   
    
    coords <- data.frame(lat= PointsPorderImage$lat, lon=PointsPorderImage$lon)   
    data   <- data.frame(link= PointsPorderImage$link)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    PointsImg <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
###########################################################################
if (file.exists(Haulout_polygon) == T) {
  Dir_Haulout <<- paste0(labelInput,"\\Predict\\Haulout")

  if (dir.exists(Dir_Haulout)==F) {dir.create(Dir_Haulout)}
  Haulout_polygon_poly=shapefile(Haulout_polygon) # need kml
  proj4string(Haulout_polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  vectorSequence=Haulout_polygon_poly$FID
  for (p in 1: length(vectorSequence)) {
 #################################################################### 
  
    Haulout_polygon_poly_seq=Haulout_polygon_poly[Haulout_polygon_poly$FID == vectorSequence[p],]
    HauloutImg0 <- point.in.poly(PointsImg,Haulout_polygon_poly_seq) 
    
    HauloutImg=as.data.frame(HauloutImg0)
    HauloutImg2=HauloutImg[is.na(HauloutImg[,2]) == FALSE,]
    HauloutImg3=data.frame(link=unique(HauloutImg2$link))
    
    from=as.vector(paste0(labelInput,"\\Predict\\Input\\",HauloutImg3$link))
    file.copy(from,Dir_Haulout)
  }}
###################################################
#if (file.exists(Rookery_polygon) == T) {
#  Dir_Rookery<<-paste0(labelInput,"\\Predict\\Rookery")
#  table_img_pth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
#  table_img=read.csv(table_img_pth)
#  table_img$link1=paste0(table_img$date , "_", table_img$link)
#  if (dir.exists(Dir_Rookery)==F) {dir.create(Dir_Rookery)}
#  
#  Rookery_polygon_poly=shapefile(Rookery_polygon) # need kml
#  proj4string(Rookery_polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
#  vectorSequence=Rookery_polygon_poly$FID
#  for (p in 1: length(vectorSequence)) {
#    Rookery_polygon_poly_seq=Rookery_polygon_poly[Rookery_polygon_poly$FID==vectorSequence[p],]
#							 
#    RookeryImg0 <- point.in.poly(PointsImg,Rookery_polygon_poly_seq) 
#    RookeryImg=as.data.frame(RookeryImg0)
#    RookeryImg2=RookeryImg[is.na(RookeryImg[,2]) == FALSE,]
#	RookeryImg3=data.frame(link=unique(RookeryImg2$link))
#    #RookeryImg3=data.frame(link=RookeryImg2$link,lat=RookeryImg2$coords.x1,lon=RookeryImg2$coords.x2)
#    if (length(RookeryImg3$link) !=0) {
#
#    from=as.vector(paste0(labelInput,"\\Predict\\Haulout\\",RookeryImg3$link))
#	for ( y in 1:length(from)) {
#	from1=from[y]
#	if (file.exists(from1)==T) {
#   file.move(from1,Dir_Rookery)
#	}}}
#  }}




