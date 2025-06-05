# erode

 library(EBImage)
 dirMsk ="C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\Unet_new data\\Mask_orig"
 SaveDir="C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\Unet_new data\\Mask"
 listMsk=list.files(dirMsk, full.names=T)
 
 for (i in 1:length(listMsk)) {
 
 pth = listMsk[i]
 bsn=basename(pth)
 SavePth=paste0(SaveDir,"\\",bsn)
 Msk = readImage(pth)
 colorMode(Msk)=Grayscale
 
 new= Msk[,,1]-Msk[,,2]
  new = erode(new, makeBrush(17, shape='disc'))
  display(new)
  
  writeImage(new, SavePth)
 
 }
 

 