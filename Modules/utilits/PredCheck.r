
library(EBImage)

labelInput="E:\\2021_19_OPP\\20210728_105310_DKV1996"

 predsDir=paste0(labelInput,"\\Predict\\Preds")
 IMgDir=paste0(labelInput,"\\Predict\\PUP")
 saveDir=paste0(labelInput,"\\Predict\\PredsCheck");dir.create(saveDir,showWarnings = F)
 listPreds=list.files(predsDir,full.names=T)
 
 for (f in 1:length(ListPreds)) {
 
   PredsRDS=readRDS(listPreds[f])
   listImgs=PredsRDS$listImageBl
   preds=PredsRDS$preds
    
	 for (i in 1:length(listImgs)) }
            name=listImgs[i]
			pred=preds[i, , ,]
			pred=as.Image(resize(pred,512,512))
			colorMode(pred)="Color"
			pthImg=paste0(IMgDir,"\\",name)
			img=readImage(pthImg)
			comby=abind(img,pred,along=1)
			
			
 
 
 
 
 
 
 }
 }
 
 