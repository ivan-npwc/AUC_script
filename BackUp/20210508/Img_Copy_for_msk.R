imgDirFrom <<- paste0(labelInput,"\\Predict")
maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask")
to=paste0(labelInput,"\\", "Mask_Image", "\\","Image")
if(dir.exists(to)==F) {dir.create(to)}
mskList=list.files(maskDir)
mskList=gsub("png","jpg",mskList)
imgListAll=list.files(imgDirFrom,  full.names=T, recursive = T,include.dirs= F)
ImgTableAll=data.frame(pth=imgListAll, link=basename(imgListAll))
ImgForCopy=ImgTableAll[ImgTableAll$link %in% mskList,]
from=paste0(ImgForCopy$pth)
file.copy(from,to)
   
