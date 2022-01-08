library(magick)


 labelInput= "E:\\2021_19_OPP\\20210728_105310_DKV1996"
 date1=substr(basename(labelInput),1,15)
 Tpth=paste0(labelInput,"\\", date1, "_table.csv")
 BigImgDir=paste0(labelInput, "\\", "Predict", "\\","Haulout")
 SaveImgDir=paste0(labelInput, "\\", "Predict", "\\","PUP")
 NewCoord1=NULL
 PthTbSave=paste0(labelInput,"\\Predict\\Pup_Img_Ext_",date1,".csv")
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
  
 
 
# cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
#    clusterExport(cl)
#    clusterEvalQ(cl, {
#      library(magick)	 
 #   })
 #   registerDoParallel(cl)
 # foreach(i = 1:length(listBigImgs)) %dopar% {	
     for (i in 1: length(listBigImgs)) {
 
 
 Crop1=NULL
 Crop2=NULL
 Crop3=NULL
 Crop4=NULL
 
 
 
 
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
  
  ################################################## CROP 1
  NewCoord=NULL
  Lonlim=c(coord$west, coord$east)
  Latlim=c(coord$south, coord$north)
    
  LONlimquantile1=c(0.125,0.625)
  LATlimquantile1=c(0.625,0.125)  # becose magic from up to down
 
  Lonlim1=quantile(Lonlim,LONlimquantile1)
  Lattlim1=quantile(Latlim,LATlimquantile1)
  
  NewCoord=data.frame(west=Lonlim1[1],east=Lonlim1[2],south= Lattlim1[2], north= Lattlim1[1])
  
  rownames(NewCoord)=basename(pthSave1)
  
  NewCoord1=rbind(NewCoord1,NewCoord)
  ############################################CROP 2
   NewCoord=NULL
   LONlimquantile2=c(0.375,0.875)
   LATlimquantile2=c(0.625,0.125)  # becose magic from up to down
  
   Lonlim2=quantile(Lonlim,LONlimquantile2)
  Lattlim2=quantile(Latlim,LATlimquantile2)
  
  NewCoord=data.frame(west=Lonlim2[1],east=Lonlim2[2],south= Lattlim2[2], north= Lattlim2[1])
  
  rownames(NewCoord)=basename(pthSave2)
  
  NewCoord1=rbind(NewCoord1,NewCoord)
  ##########################################CROP 3
   NewCoord=NULL
   LONlimquantile3=c(0.125,0.625)
   LATlimquantile3=c(0.125,0.625) # becose magic from up to down
  
  Lonlim3=quantile(Lonlim,LONlimquantile3)
  Lattlim3=quantile(Latlim,LATlimquantile3)
  
  NewCoord=data.frame(west=Lonlim3[1],east=Lonlim3[2],south= Lattlim3[2], north= Lattlim3[1])
  
  rownames(NewCoord)=basename(pthSave3)
  
  NewCoord1=rbind(NewCoord1,NewCoord)
  
 ############################################## CROP 4
   NewCoord=NULL
   LONlimquantile4=c(0.375,0.875)
   LATlimquantile4=c(0.125,0.625) 
  
  Lonlim4=quantile(Lonlim,LONlimquantile4)
  Lattlim4=quantile(Latlim,LATlimquantile4)
  
  NewCoord=data.frame(west=Lonlim4[1],east=Lonlim4[2],south= Lattlim4[2], north= Lattlim4[1])
  
  rownames(NewCoord)=basename(pthSave4)
  
  NewCoord1=rbind(NewCoord1,NewCoord)

  }
  write.csv(NewCoord1,PthTbSave)