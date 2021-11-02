library(EBImage)

genImgMskDir="D:\\PL_DB\\2021_3101_OPP\\20210725_084310\\Error_LRG"
imgDir=paste0(genImgMskDir,"\\Image")
mskDir=paste0(genImgMskDir,"\\Mask")
NewMskDir=paste0(genImgMskDir,"\\Mask1");dir.create(NewMskDir)

listMskFrom=list.files(mskDir, recursive=T,full.names=T, pattern="_TN")
listMskNew=gsub("_TN","",basename(listMskFrom))
listMskTo=paste0(NewMskDir,"\\",listMskNew)

file.copy(listMskFrom,listMskTo)

unlink(mskDir, recursive=T)


PresenceImgs=gsub("_TN","",basename(listMskFrom))
PresenceImgs1=gsub("png","jpg",basename(PresenceImgs))
listImgsALL=list.files(imgDir)
listDelete=listImgsALL[! listImgsALL %in% PresenceImgs1]
listDelete1=paste0(imgDir,"\\",listDelete)
unlink(listDelete1)


for (i in 1:length(listMskTo)) {
msk=readImage(listMskTo[i])
colorMode(msk)=Grayscale
msk1=msk[,,3]
writeImage(msk1,listMskTo[i])





}