     
	
	 library(magick)
	 library(parallel)
	 library(doParallel)
	 library(foreach)
	 
     Batch=4000
	 crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
     date1=substr(basename(labelInput),1,15)
     Tpth = paste0(labelInput,"\\", date1, ".csv")
	 PredPolDir=paste0(labelInput, "\\Polygons\\Predict")
	 PredPolPth=list.files(PredPolDir, pattern="kml|shp", full.names=T)[1]
	 
	  KMLdir <- paste0(labelInput,"\\",date1)
	  kmlOPP <- list.files(labelInput,pattern="kmz", full.names=T)
	 
	 if (file.exists(Tpth)==F) {
	 source("Modules/01_Unzip.r")
	 print("Done Unzip extra")
	 source("Modules/02.0_KMLprepare.r")
	 print("Done KMLprepare extra")
	 }
	 
	 KMLDir = paste0(labelInput,"\\",date1)
     table=read.csv(Tpth)
     SaveDir=paste0(labelInput, "\\Predict\\TilesOverlap")
	 dir.create(paste0(labelInput, "\\Predict"),showWarnings=F); dir.create(SaveDir,showWarnings=F)
##############################################################################################
if (file.exists(PredPolPth) == T) {  
   CombinationOne=data.frame(lat= table$west, lon=table$south,imgName= table$imgName)
   CombinationTo=data.frame(lat= table$west, lon=table$north,imgName= table$imgName)
   CombinationThry=data.frame(lat= table$east, lon=table$south,imgName= table$imgName)
   CombinationFor=data.frame(lat= table$east, lon=table$south,imgName= table$imgName)
   PointsPorderImage=rbind(CombinationOne,CombinationTo,CombinationThry,CombinationFor)
    coords <- data.frame(lat= PointsPorderImage$lat, lon=PointsPorderImage$lon)   
    data   <- data.frame(imgName= PointsPorderImage$imgName)   # data
    
    PointsImg <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
									 
         PredPol=readOGR(PredPolPth) # need kml
		 proj4string(PredPol) <-crs	
		 Index= PointsImg %over% PredPol 
		 PointsImg$Index=Index
		 ImgIn = PointsImg$imgName[is.na(PointsImg$Index)==F]
	     ImgIn =	unique(ImgIn[is.na(ImgIn)==F])
 }
 if (file.exists(PredPolPth) == F) {ImgIn=table$imgName} 
 ##########################
######################################
   exsts=list.files(SaveDir)
   NeedMore = ImgIn[!ImgIn %in% exsts]
    print(paste0("Need prepare ", length(NeedMore), " Images from  ",  length(ImgIn)))
	
    if (length(NeedMore)>1) {

      Loops=round(length(NeedMore)/Batch)+1
      if (length(NeedMore) < Batch) {Loops=1}
	  
   for (r in 1: Loops) {
   
    cl <- makePSOCKcluster(detectCores (logical=FALSE)-2) 
    clusterEvalQ(cl, {library(magick)})
    registerDoParallel(cl)
   
    exsts=list.files(SaveDir)
    NeedMore1=ImgIn[!ImgIn %in% exsts]
	 if (length(NeedMore1)<Batch){Batch=length(NeedMore1)}
	
    foreach(i = 1:Batch) %dopar% {   #
  ################
 #for (i in 1:Batch){
  
	  selectRow <- table[table$imgName == NeedMore1[i],]
      ImgN=selectRow$imgName 	                                                               
      path<-paste0(KMLDir, "\\",ImgN)            
      CheckExists = paste0(SaveDir, "\\",ImgN)

      if(file.exists(CheckExists)==F & file.exists(path)) {                                                                   
        img=image_read(path)  
        ################################################################################## CENTRAL WITH left right EARS
       # lpi<-paste0(KMLDir,"\\",selectRow$leftName)
	   lpi<-paste0(KMLDir,"\\",selectRow$rightName)
        if (file.exists(lpi)==T) {left.img= image_read(lpi)} else {left.img= image_blank(512,512,color = "white")}

       rimg<-paste0(KMLDir,"\\",selectRow$leftName)
      #  rimg<-paste0(KMLDir,"\\",selectRow$rightName)
        if (file.exists(rimg)==T){right.img=image_read(rimg)} else {right.img=  image_blank(512,512,color = "white")}
        ########
        uimg<-paste0(KMLDir,"\\",selectRow$upName)                                                           # prepare up img for next up line level
        if (file.exists(uimg)==T) {up.img=image_read(uimg)} else{up.img= image_blank(512,512,color = "white")}  
        ##########
        dimg=paste0(KMLDir,"\\",selectRow$downName)                                                          # prepare down img for next down line level
        if (file.exists(dimg)==T) {down.img=image_read(dimg)} else {down.img=  image_blank(512,512,color = "white")}
        
        leftCrop=image_crop(left.img, "256x512+256")
        leftJoin=image_append(c(leftCrop,img))
        ###
        RightCrop=image_crop(right.img, "256x512") 
        leftRightJoin=image_append(c(leftJoin,RightCrop))
        ############################################################## UP LEVEL
        
        Uplevel= table[table$imgName==paste0(selectRow$upName),] 
        if (length(Uplevel$imgName)==0) {
          UpLeft= image_blank(512,512,color = "white")
          UpRight=  image_blank(512,512,color = "white")
        } else {
   
       #   ulimg = paste0(KMLDir,"\\",Uplevel$leftName) 
	       ulimg = paste0(KMLDir,"\\",Uplevel$rightName) 
          if (file.exists(ulimg)==T) {UpLeft=image_read(ulimg)} else{UpLeft= image_blank(512,512,color = "white")}  
      #    urimg=paste0(KMLDir,"\\",Uplevel$rightName)
	       urimg=paste0(KMLDir,"\\",Uplevel$leftName)
          if (file.exists(urimg)==T) {UpRight=image_read(urimg)} else{UpRight=  image_blank(512,512,color = "white")}
		  }
        UplevelImg=image_append(c(UpLeft,up.img,UpRight))
        UpCrop=image_crop(UplevelImg, "1536x256+0+256")
        UpCrop1=image_crop(UpCrop, "1280x256+256")
        UpCrop2=image_crop(UpCrop1, "1024x256")
        ###
        leftRightUpJoin=image_append(c(UpCrop2,leftRightJoin), stack = T)
        #############################################################################   DOWN LEVEL
        
        Downlevel= table[table$imgName==paste0(selectRow$downName),] 
        if (length(Downlevel$imgName)==0) {
          DownLeft=  image_blank(512,512,color = "white")
          DownRight=  image_blank(512,512,color = "white")
        } else {
      #    dlimg = paste0(KMLDir,"\\",Downlevel$leftName)  
	       dlimg = paste0(KMLDir,"\\",Downlevel$rightName) 
          if (file.exists(dlimg)==T) {DownLeft=image_read(dlimg)} else{DownLeft=  image_blank(512,512,color = "white")}
       #   drimg=paste0(KMLDir,"\\",Downlevel$rightName) 
	       drimg=paste0(KMLDir,"\\",Downlevel$leftName)
          if (file.exists(drimg)==T) {DownRight=image_read(drimg)} else {DownRight=  image_blank(512,512,color = "white")}
        }
        DownlevelImg=image_append(c(DownLeft,down.img,DownRight))
        DownCrop=image_crop(DownlevelImg, "1536x256")
        DownCrop1=image_crop(DownCrop, "1280x256+256")
        DownCrop2=image_crop(DownCrop1, "1024x256")
        ############
        leftRightUpDownJoin=image_append(c(leftRightUpJoin,DownCrop2), stack = T)
        image_write(leftRightUpDownJoin,CheckExists,format="jpg")
      }
	
	 
    }
 stopCluster(cl)
  } 
  }   








    #  unlink(KMLdir, recursive=T)
	#  unlink(kmlOPP, recursive=T) 