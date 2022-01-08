library(magick)


 labelInput="D:\\CU_DB\\2021_138_OPP\\20210727_074648_VES"
 date1=substr(basename(labelInput),1,15)
 Tpth=paste0(labelInput,"\\", date1, "_table.csv")
 BigImgDir=paste0(labelInput, "\\", "Predict", "\\","Haulout")
 SaveImgDir=paste0(labelInput, "\\", "Predict", "\\","PUP")

 if (dir.exists(SaveImgDir)==F) {dir.create(SaveImgDir)}
 BgCrdTbl=read.csv(Tpth)
 BgCrdTbl$link=paste0(BgCrdTbl$date ,"_", BgCrdTbl$link)
 listBigImgs=list.files(BigImgDir,full.names=T)
#####################
coordCrop1=paste0("512x512+128+128")
coordCrop2=paste0("512x512+384+128")
coordCrop3=paste0("512x512+128+384")
coordCrop4=paste0("512x512+384+384")
######################
  for (i in 1: length(listBigImgs)) {
  i=500
  imgP=listBigImgs[i]
  Nm=basename(imgP)
  BgImg=image_read(imgP)
  coord=BgCrdTbl[BgCrdTbl$link==Nm,]
  Crop1=image_crop(BgImg,coordCrop1)
  Crop2=image_crop(BgImg,coordCrop2)
  Crop3=image_crop(BgImg,coordCrop3)
  Crop4=image_crop(BgImg,coordCrop4)
  
  pthSave1=paste0(SaveImgDir,"\\1_",Nm)
  pthSave2=paste0(SaveImgDir,"\\2_",Nm)
  pthSave3=paste0(SaveImgDir,"\\3_",Nm)
  pthSave4=paste0(SaveImgDir,"\\4_",Nm)
  
  image_write(Crop1,pthSave1,format="jpg")
  image_write(Crop2,pthSave2,format="jpg")
  image_write(Crop3,pthSave3,format="jpg")
  image_write(Crop4,pthSave4,format="jpg")
  
  
 # Lonlim=c(coord$west, coord$east)
 # Latlim=c(coord$south, coord$north)
  
# Lonlimquantile1=c(0.125,0.875)
 
 
# Lonlim1=quantile(Lonlim,quantile1)
  
  
  
  
  

  }