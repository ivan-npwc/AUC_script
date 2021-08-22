
dir.create(paste0(labelInput, "\\Error\\"))
dir.create(paste0(labelInput, "\\Error\\Empty\\"))


dirEmptyErrorInput=paste0(labelInput, "\\Error\\Empty\\Input")
dirEmptyErrorMsk= paste0(labelInput, "\\Error\\Empty\\Mask")
dirEmptyErrorImg = paste0(labelInput, "\\Error\\Empty\\Image")
DirImgError=paste0(labelInput,"\\Error\\Image")


if (dir.exists(dirEmptyErrorMsk)==F) {dir.create(dirEmptyErrorMsk)}
if (dir.exists(dirEmptyErrorImg)==F) {dir.create(dirEmptyErrorImg)}
if (dir.exists(dirEmptyErrorInput)==F) {dir.create(dirEmptyErrorInput)}
if(dir.exists(DirImgError)==F) {dir.create(DirImgError)}


pointsPth<<- Pth_img_error


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
searchListImg=list.files(searchDir,full.names = T,recursive = T,include.dirs = F,pattern = ".png")
searchTableImg=data.frame(pth=searchListImg, img=basename(searchListImg))
searchErrorImg<<-searchTableImg[searchTableImg$img %in% listError,]
for (i in 1: length(searchErrorImg$pth)) {
  from=paste0(searchErrorImg$pth[i])
  to = paste0(DirImgError, "\\",searchErrorImg$img[i])
  file.copy(from, to)
}