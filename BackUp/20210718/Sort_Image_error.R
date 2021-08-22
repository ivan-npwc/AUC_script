
Species
labelInput

ErrDir=paste0(labelInput, "\\Error_",Species)
if(dir.exists(ErrDir)==F){stop("No Error Points found")}

pointsPthDir=paste0(ErrDir,"\\Points")
pointsPth=list.files(pointsPthDir,full.names=T,recursive=T,pattern=".shp")

DirImgError=paste0(ErrDir,"\\Image")
DirMskError=paste0(ErrDir,"\\Mask")

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


pointsError=shapefile(pointsPth)
proj4string(pointsError) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
pointsError=data.frame(pointsError)
pointsError=data.frame(lat=pointsError$coords.x1,lon=pointsError$coords.x2)
table_img_pth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
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
 Tpth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
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
 unlink(paste0(DirImgError,"\\",OverImgList1))
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