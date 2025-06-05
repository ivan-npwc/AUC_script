







ErrDir=paste0(labelInput, "\\Error")
pointsPthDir=paste0(ErrDir,"\\Points")




Species
labelInput
date1=substr(basename(labelInput),1,15)

 source("Modules/01_create_tile_polygons.r")

crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
table_img_pth=paste0(labelInput,"\\", date1, "_table.csv")

if(dir.exists(ErrDir)==F){stop("No Error Points found")}


pointsPth=list.files(pointsPthDir,full.names=T,recursive=T,pattern=".shp")
PoligonError=paste0(ErrDir, "\\Polygons")
 SavePTHpol=paste0(PoligonError,"\\Error.kml")

DirImgError=paste0(ErrDir,"\\Image")
DirMskError=paste0(ErrDir,"\\Mask")

unlink(list.files(DirImgError, full.names=T))
listMsk=list.files(DirMskError, full.names=T, recursive=T, include.dirs=T)
unlink(listMsk, recursive=T)



HauloutDir=paste0(labelInput,"\\Predict\\Haulout")

if(dir.exists(DirImgError)==F) {dir.create(DirImgError)}
if(dir.exists(DirMskError)==F) {dir.create(DirMskError)}

#dir.create(paste0(labelInput, "\\Error\\Empty\\"))
#dirEmptyErrorInput=paste0(labelInput, "\\Error\\Empty\\Input")
#dirEmptyErrorMsk= paste0(labelInput, "\\Error\\Empty\\Mask")
#dirEmptyErrorImg = paste0(labelInput, "\\Error\\Empty\\Image")
#if (dir.exists(dirEmptyErrorMsk)==F) {dir.create(dirEmptyErrorMsk)}
#if (dir.exists(dirEmptyErrorImg)==F) {dir.create(dirEmptyErrorImg)}
#if (dir.exists(dirEmptyErrorInput)==F) {dir.create(dirEmptyErrorInput)}
#pointsPth<<- Pth_img_error
if (Species != "WLRS") {

pointsError=shapefile(pointsPth)
proj4string(pointsError) <- crs
pointsError=data.frame(pointsError)
pointsError=data.frame(lat=pointsError$coords.x1,lon=pointsError$coords.x2)

table_img=read.csv(table_img_pth)
table_img$link=paste0(table_img$date , "_", table_img$link)
###################
NSdif50=(table_img$north-table_img$south)/2 
table_img$north50=table_img$north              #-NSdif50  
SNdif50=(table_img$north-table_img$south)/2
table_img$south50=table_img$south              #+SNdif50 
WEdiff50= (table_img$east-table_img$west)/2
table_img$west50=table_img$west                #+WEdiff50 
EWdiff50= (table_img$east-table_img$west)/2 
table_img$east50=table_img$east                 #-EWdiff50 
for (i in 1:length(pointsError[,1])){
  lat=as.numeric(pointsError$lat[i])
  lon=as.numeric(pointsError$lon[i])
  sort=table_img[abs(table_img$north50-lon)==min(abs(table_img$north50-lon)),]
  sort1=sort[abs(sort$east50-lat)==min(abs(sort$east50-lat)),]
  if (i==1) {img_error=sort1} else {
    img_error=rbind(img_error,sort1)
    listError<<-c(img_error$link)
  }}
  
searchDir=paste0(labelInput,"\\Predict")
searchListImg=list.files(searchDir,full.names = T,recursive = T,include.dirs = F)
searchTableImg=data.frame(pth=searchListImg, img=basename(searchListImg))

searchErrorImg<<-searchTableImg[searchTableImg$img %in% listError,]

if (length(searchErrorImg[,1])==0) {
listError= gsub("png","jpg",listError)
searchErrorImg<<-searchTableImg[searchTableImg$img %in% listError,]}



for (i in 1: length(searchErrorImg$pth)) {
  from=paste0(searchErrorImg$pth[i])
  to = paste0(DirImgError, "\\",Species,"_",searchErrorImg$img[i])
  file.copy(from, to)
}
##################################################################################################
listImgError=list.files(DirImgError)
 Tpth=paste0(labelInput,"\\", date1, "_table.csv")
 tableNearImg=read.csv(Tpth)
 tableNearImg$link=paste0(tableNearImg$date,"_", tableNearImg$link)
 tableNearImg=tableNearImg[tableNearImg$link %in% listImgError,]
 
 OverImgList1=NULL
  for (i in 1:length(tableNearImg$link)) {
  row1=tableNearImg[i,]
  ListNearImg=c(paste0(row1$date,"_",row1$leftName),paste0(row1$date,"_",row1$upName) ,paste0(row1$date,"_",row1$rightName),paste0(row1$date,"_",row1$downName))
  OverImgList=tableNearImg$link[tableNearImg$link %in% ListNearImg]
  OverImgList1=c(OverImgList,OverImgList1)

 # tableNearImg=tableNearImg[!(tableNearImg$link %in% OverImgList),]
  }
   unlink(paste0(DirImgError,"\\",OverImgList1))
   }
 ######################################################################################################################
#######################################################################################################################   WLRS
if (Species=="WLRS") {

 SpP_dataW=create_tile_polygons(labelInput)
 
pointsError=shapefile(pointsPth)
proj4string(pointsError) <- crs
pointsError=spTransform(pointsError,CRS(crs))
 
 index= SpP_dataW %over% pointsError
 index1= index[is.na(index[,2])==F,]
 imgList=rownames(index1)
 imgList1=gsub("/","_",imgList)
 imgList2=paste0(basename(labelInput),"_",imgList1)
 imgList3=paste0(HauloutDir,"\\",imgList2)
 file.copy(imgList3,DirImgError)

imgList4=basename(imgList3)
for (i in 1:length(imgList4)) {imgList4[i]=gsub(paste0(date1,"_"),"",imgList4[i])}

table_img=read.csv(table_img_pth)
sortTable=table_img[table_img$link %in% basename(imgList4),]

for (i in 1 :length(sortTable[,1])) {
#row1=sortTable[i,]
OverLopWestEast=abs(abs(sortTable[i,]$west)- abs(sortTable[i,]$east))/2
OverLopSouthNorth=abs(abs(sortTable[i,]$south)-abs(sortTable[i,]$north))/2

sortTable$westOver[i] = sortTable$west[i]-OverLopWestEast
sortTable$eastOver[i] = sortTable$east[i]+OverLopWestEast

sortTable$southOver[i] = sortTable$south[i]-OverLopSouthNorth
sortTable$northOver[i] = sortTable$north[i]+OverLopSouthNorth
}

out4=data.frame(X1= sortTable$westOver,
                X2 =sortTable$eastOver,
				X3 =sortTable$southOver,
				X4 =sortTable$northOver,
				X5 =sortTable$image,
				path=sortTable$fold)
				
	require(sp)
	srPolygons=list()
	srPolygonsData=list()
	for(i in 1:length(out4[,1]))
	{
		srPolygons[[i]]=Polygons(list(Polygon(cbind(as.numeric(c(out4[i,1],out4[i,1],out4[i,2],out4[i,2])),as.numeric(c(out4[i,3],out4[i,4],out4[i,4],out4[i,3]))))),paste0(out4[i,6],"/",out4[i,5]))
		srPolygonsData[[i]]=data.frame(dx=abs(as.numeric(out4[i,2])-as.numeric(out4[i,1])),dy=abs(as.numeric(out4[i,4])-as.numeric(out4[i,3])))
	}

SpP=SpatialPolygons(srPolygons)


	srPolygonsData=do.call(abind, c(srPolygonsData, list(along = 1)))
	rows=rownames(sp::coordinates(SpP))
	rownames(srPolygonsData)=rows
	srPolygonsData=as.data.frame(srPolygonsData)
	data1=as.data.frame(rownames(srPolygonsData))
	rownames(data1)=rows
	SpP_data1=SpatialPolygonsDataFrame(SpP,data1)
	SpP_data1=subset(SpP_data1,srPolygonsData[,1]<=quantile(srPolygonsData[,1],0.975))
	
	proj4string(SpP_data1) <- CRS(crs)
	SpP_data1=spTransform(SpP_data1,CRS(crs))
	SpP_data1
	
	sf =st_as_sf(SpP_data1)
	parts <- st_cast(st_union(sf),"POLYGON")
	
	st_write(parts , SavePTHpol, driver = "kml")

	
	
# writeOGR(i,  SavePTHpol, layer ="Error", driver="KML")       #ESRI Shapefile
 }



	
 
 ########################################################################################################## 
 #  dirToImg=paste0(labelInput,"\\Error\\Over\\Image")
 #  dirToMsk=paste0(labelInput,"\\Error\\Over\\Image")
 #  dir.create(paste0(labelInput,"\\Error\\Over"))
 #  dir.create(dirToImg)
 #  dir.create(dirToMsk)
# for (i in 1:length(OverImgList1)) {
# from=paste0(DirImgError,"\\",OverImgList1[i])
# to=paste0(dirToImg,"\\",OverImgList1[i])
# file.copy(from,to)
# }

##########################################################################################################################
# imgDirIrig="C:\\SSL_DB\\2019_138_OPP\\20190712\\Error\\Image"
# mskDirOrig="C:\\SSL_DB\\2019_138_OPP\\20190712\\Error\\Mask"
#  list.img=list.files(imgDirIrig)
#  list.msk=list.files(mskDirOrig)
#  
#  list.delete.msk=list.msk[!(list.msk %in% list.img)]
#  list.delete.img=list.img[!(list.img %in% list.msk)]
#   unlink(paste0(imgDirIrig,"\\",list.delete.img))
#   unlink(paste0(mskDirOrig,"\\",list.delete.msk))
# ################################################################################# 
# dir.over= "C:\\SSL_DB\\2019_138_OPP\\20190712\\Error\\Over"
# dir.ready.paint="C:\\SSL_DB\\2019_138_OPP\\20190712\\Error"
# 
#  dir.over.img=paste0(dir.over,"\\Image")
#  dir.over.msk=paste0(dir.over,"\\Mask")
#  dir.ready.paint.img=paste0(dir.ready.paint,"\\Image")
#  dir.ready.paint.msk=paste0(dir.ready.paint,"\\Mask")
#  listImgOver=list.files(dir.over.img)
#  listPresenceMsk=list.files(dir.ready.paint.msk)#

# Tpth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
# tableNearImg=read.csv(Tpth)
# tableNearImg$link=paste0(tableNearImg$date,"_", tableNearImg$link)
# tableNearImg=tableNearImg[tableNearImg$link %in% listImgOver,]
 
# for ( i in 1:length(listImgOver)) {
# row1=tableNearImg[tableNearImg$link==listImgOver[i],]
# ListNearImg=c(paste0(row1$date,"_",row1$leftName),paste0(row1$date,"_",row1$upName) ,paste0(row1$date,"_",row1$rightName),paste0(row1$date,"_",row1$downName))
# tblRef=data.frame(img=ListNearImg,ref=c("leftName","upName","rightName","downName"))
# PresenceMsk=listPresenceMsk[listPresenceMsk %in% ListNearImg]
#site1=tblRef$ref[tblRef$img ==PresenceMsk]
 
# }





