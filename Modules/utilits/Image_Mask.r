
		library(EBImage)
		library (magick)
		library(parallel)
		library(doParallel)
		library(foreach)

        labelInput

     
		ImagePath= paste0(labelInput,"\\Predict\\TilesOverlap")
		MaskPath=  paste0(labelInput,"\\Predict\\MaskPredicted") 
		PathCheck= paste0(labelInput,"\\Predict\\Image_Mask_pred")

		unlink(PathCheck, recursive=T);dir.create(PathCheck)
		MskList=list.files(MaskPath)
		ImgLst=list.files(ImagePath)
		
		prefix=substr(ImgLst[1],1,15)
        prefix
		

		cl <- makePSOCKcluster(4) 
		clusterEvalQ(cl, {
		library(EBImage)
		})
			
		registerDoParallel(cl)


		foreach(i = 1:length(MskList)) %dopar% {


			pth=MskList[i]
			
			mskP=paste0(MaskPath,"/",pth) 
			ImgP= paste0(ImagePath,"/", gsub("png","jpg",pth))#prefix,"_",
			
		if (file.exists(ImgP)){	
			img=readImage(ImgP)
			msk=readImage(mskP)
			  y1 = channel(msk, 'asred')
			  a= img+y1
			  a1=resize(a,1024,1024)
		   PathCheckImg=paste0(PathCheck,"/",pth)
		   writeImage(a1,PathCheckImg)
		   

		}
		}
