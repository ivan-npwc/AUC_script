
#listOPP
ImhDirTo= "D:\\TRAIN_pv\\Unet_new data\\Image"
MskDirTo= "D:\\TRAIN_pv\\Unet_new data\\Mask"

for (i in 1:length(listOPP)) {

      opp=listOPP[i]
	  bsnm=basename(opp)
      labelInput=gsub(bsnm,"",opp)
	  maskDir=paste0(labelInput,"\\Mask_Image\\Mask")
	  imgDir=paste0(labelInput,"\\Mask_Image\\Image")
	  
	   mskLst=list.files(maskDir, full.names=T)
	   imgLst=list.files(imgDir, full.names=T)
    if (length(imgLst)>1){

  # file.copy(mskLst,MskDirTo)
   file.copy(imgLst,ImhDirTo)


}
}

a="D:\\PV_DB/2023_H0099_OPP/20230502_135032/20230502_135032_MINI3PRO_20m/\\Mask_Image\\Image/20230502_135032_i21010223.jpg"
 file.copy(a,ImhDirTo)