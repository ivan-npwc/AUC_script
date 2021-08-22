library(magick)
################################
dirImgManiken="data/Mask auto creater/Image"
dirMskManiken="data/Mask auto creater/Mask"

 DirImgError=paste0(labelInput,"\\Error\\Image")
 DirMskError=paste0(labelInput,"\\Error\\Mask")
 dirEmptyErrorInput=paste0(labelInput, "\\Error\\Empty\\Input")
dirEmptyErrorMsk= paste0(labelInput, "\\Error\\Empty\\Mask")
dirEmptyErrorImg = paste0(labelInput, "\\Error\\Empty\\Image")
listImg=list.files(DirImgError,include.dirs=F)
listMsk=list.files(DirMskError,include.dirs=F)

if (length(listMsk)>0) {
listImg=listImg[!(listImg %in% listMsk)] }

listImgCopyFrom=paste0(DirImgError,"\\",listImg)
file.copy(listImgCopyFrom,dirEmptyErrorInput)

#############################################
randomHorizontalFlip <- function(img, mask) {
  w=sample(1:4)[1]
  if (w == 1) {a=(list(img = img, mask = mask))}
  if (w == 2) {a=(list(img = image_flop(img), mask = image_flop(mask))) }
  if (w == 3) {a=(list(img = image_flip(img), mask = image_flip(mask)))}
  if (w == 4) {a=(list(img = image_flop(image_flip(img)), mask = image_flop(image_flip(mask))))}
  return(a)  
}
####################################################


if (dir.exists(dirEmptyErrorMsk)==F) {dir.create(dirEmptyErrorMsk)}
if (dir.exists(dirEmptyErrorImg)==F) {dir.create(dirEmptyErrorImg)}

listImgEmptyError=list.files(dirEmptyErrorInput,full.names=T)
listImgManiken=list.files(dirImgManiken,full.names=T)
listMaskManiken=list.files(dirMskManiken,full.names=T)

for (i in 1:length(listImgEmptyError)) {
  pth_imgOrig=paste0(listImgEmptyError[i])
  imgOrig=image_read(pth_imgOrig)
  index=sample(1:length(listImgManiken))[1]
  imgManiken=image_read(listImgManiken[index])
  mskManiken=image_read(listMaskManiken[index])
  
   a=randomHorizontalFlip(img=imgManiken,mask=mskManiken)
#  a=(list(img = imgManiken, mask = mskManiken))
  
  d=image_flatten(c(imgOrig,a$img))
  imgSavePth=paste0(dirEmptyErrorImg,"\\",basename(pth_imgOrig))
  mskSavePth=paste0(dirEmptyErrorMsk,"\\",basename(pth_imgOrig))
  image_write(d,imgSavePth)
  image_write(a$mask,mskSavePth)
}
