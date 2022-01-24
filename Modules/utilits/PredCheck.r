
library(EBImage)
library(magick)

labelInput="E:\\2021_19_OPP\\20210729_113132_DKV1996"

 predsDir=paste0(labelInput,"\\Predict\\Preds")
 IMgDir=paste0(labelInput,"\\Predict\\PUP")
 saveDir=paste0(labelInput,"\\Predict\\PredsCheck");dir.create(saveDir,showWarnings = F)
 listPreds=list.files(predsDir,full.names=T)
 
 for (f in 1:length(listPreds)) {
 
   PredsRDS=readRDS(listPreds[f])
   
   listImgs=PredsRDS$listImageBl
   preds=PredsRDS$preds
    
	 for (i in 1:length(listImgs)) {
            name=basename(listImgs[i])
			pred=preds[i, , ,]
			#pred=t(pred)
			pred2=as.Image(resize(pred,512,512))
			colorMode(pred2)="Color"
			
			pthImg=paste0(IMgDir,"\\",name)
			

   image=image_read(pthImg)
   mask=image_read(pred2)

   Check=image_composite(image,mask,operator = "blend", compose_args="40")
   
   PathCheckImg=paste0(saveDir,"\\",name)
   
   image_write(Check,PathCheckImg,format="jpg")
			
 }
 }
 #########################################################
 