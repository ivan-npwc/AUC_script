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


####################
if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
library(tools)
library(EBImage)


dirMsk= "D:\\AUC_data\\NFS_Pup\\NewData\\Mask_PUP"

listMskTo=list.files(dirMsk,full.names=T)



cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
   
    clusterEvalQ(cl, {
    library(tools)
     library(EBImage)	 
    })
    registerDoParallel(cl)
  foreach(i = 1:length(listMskTo)) %dopar% {	

#for (i in 1:length(listMskTo)) {
msk=readImage(listMskTo[i])
colorMode(msk)=Grayscale

if (dim(msk)[3]==4){msk1=msk[,,4];msk1=1-msk1}   

if (dim(msk)[3]==2){msk1=msk[,,2]}


writeImage(msk1,listMskTo[i])


}

 stopCluster(cl)