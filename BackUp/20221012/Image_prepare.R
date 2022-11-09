#Image_prepare=function (labelInput=labelInput) {

             Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout");if(dir.exists(Haulout_polygonDir)==F){Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Houlout")} 
			        if(dir.exists(Haulout_polygonDir)==F)  {stop("No Haulout polygon found")}
					#	if(dir.exists(Haulout_polygonDir)==F){Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")}
						Haulout_polygon1=list.files(Haulout_polygonDir,full.names=T,pattern=".shp")
						ExlPolPTH=list.files(paste0(labelInput, "\\Polygons\\Exclude"),full.names=T,pattern=".shp")

  library(spatialEco)
  date1=substr(basename(labelInput),1,15)
  
  Tpth=paste0(labelInput,"\\", date1, "_table.csv")
  
  table=read.csv(Tpth)

if (file.exists(Haulout_polygon1) == F) {stop("Please create polygon animal presence and place it  in your dir ~/Polygons/Haulout/Haulout.shp")}
  
   CombinationOne=data.frame(lat= table$west, lon=table$south,link= table$link)
   CombinationTo=data.frame(lat= table$west, lon=table$north,link= table$link)
   CombinationThry=data.frame(lat= table$east, lon=table$south,link= table$link)
   CombinationFor=data.frame(lat= table$east, lon=table$south,link= table$link)
   PointsPorderImage=rbind(CombinationOne,CombinationTo,CombinationThry,CombinationFor)
    coords <- data.frame(lat= PointsPorderImage$lat, lon=PointsPorderImage$lon)   
    data   <- data.frame(link= PointsPorderImage$link)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    PointsImg <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
         Haulout_polygon_poly=shapefile(Haulout_polygon1) # need kml
		 proj4string(Haulout_polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
		 Index= PointsImg %over% Haulout_polygon_poly 
		 PointsImg$Index=Index
		 
		 
		 if (length(ExlPolPTH)>0) {
		 EcludePol=shapefile(ExlPolPTH)
		 proj4string(EcludePol) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
		 IndexExcl= PointsImg %over% EcludePol 
		 PointsImg$IndexExcl=IndexExcl
		 resLink1=data.frame(link=unique(PointsImg$link[
		                                               is.na(PointsImg$Index)==F   & 
													   is.na(PointsImg$IndexExcl)==T
													   ]))
     } else {
	     resLink1=data.frame(link=unique(PointsImg$link[
		                                               is.na(PointsImg$Index)==F
	                                                   ])) 
	 }
 resLink= data.frame(link=  resLink1[is.na(resLink1$link)==F,])
 ############################################################################################################################# 
 #table=table[table$link %in% resLink$link,]


 KMLpath<<-paste0(labelInput,"\\",date1)
  ImgSave=paste0(labelInput, "\\", "Predict", "\\","Haulout")
   
  subDir=paste0(labelInput, "\\", "Predict")
 # unlink(subDir,recursive=T)
  if (dir.exists(subDir)==F)    {dir.create(subDir)}
  if (dir.exists(ImgSave)==F)    {dir.create(ImgSave)}
  Need=length(table$link)
  Presence=length(list.files(ImgSave))
  if (Need !=Presence) {
    
    cl <- makePSOCKcluster(detectCores (logical=FALSE)) 
    clusterExport(cl, "KMLpath")
    clusterEvalQ(cl, {
      library(magick)	 
    })
    registerDoParallel(cl)
  foreach(i = 1:length(resLink$link)) %dopar% {	
    #  	for (i in 1:length(resLink$link)) {
     # selectRow<-table[i,]	
	 # if (KMLfocusPocus==T) {
	  selectRow <- table[table$link== resLink$link[i],]
	  
	  
      ImgN=gsub("png", "jpg",selectRow$image)	     
                                                                # centr image
      path<-paste0(KMLpath, "\\",selectRow$fold,"\\",ImgN)            
      
      pthExpImg<-paste0(ImgSave, "\\",selectRow$date,"_", selectRow$link,  sep="")
	#  pthExpImg=gsub("png","jpg",pthExpImg)  #
      
      if(file.exists(pthExpImg)==F) {                                                                     #check
        img=image_read(paste0(path))    
        ############################################### LEFT
        lpi1<-strsplit(paste0(selectRow$leftName), split = "_")[[1]][2]
        lpiFold=strsplit(paste0(selectRow$leftName), split = "_")[[1]][1]
        lpi<-paste0(KMLpath,"\\",lpiFold,"\\",lpi1)
        
        if (file.exists(lpi)==T) {
          left.img= image_read(lpi)
        } else {
          left.img=image_blank(512,512,color = "black")
        }
        ###################################################### RIGHT
        rimg1= strsplit(paste0(selectRow$rightName), split = "_")[[1]][2]
        rimgFold= strsplit(paste0(selectRow$rightName), split = "_")[[1]][1]
        rimg<-paste0(KMLpath,"\\",rimgFold,"\\",rimg1)
        
        if (file.exists(rimg)==T) {
          right.img=image_read(rimg)
        } else 
        {
          right.img= image_blank(512,512,color = "black")
        }
        ########
        uimg1= strsplit(paste0(selectRow$upName), split = "_")[[1]][2]
        uimgFold=strsplit(paste0(selectRow$upName), split = "_")[[1]][1]
        uimg<-paste0(KMLpath,"\\",uimgFold,"\\",uimg1)
        
        
        if (file.exists(uimg)==T) { 
          up.img=image_read(uimg)
        } else
        {
          up.img=image_blank(512,512,color = "black")
        }  
        ##########
        dimg1= strsplit(paste0(selectRow$downName), split = "_")[[1]][2]
        dimgFold=strsplit(paste0(selectRow$downName), split = "_")[[1]][1]
        dimg<<-paste0(KMLpath,"\\",dimgFold,"\\",dimg1)
        
        if (file.exists(dimg)==T) {
          down.img=image_read(dimg)
        } else {
          down.img= image_blank(512,512,color = "black")
        }
        
        leftCrop=image_crop(left.img, "256x512+256")
        leftJoin=image_append(c(leftCrop,img))
        ###
        RightCrop=image_crop(right.img, "256x512") 
        leftRightJoin=image_append(c(leftJoin,RightCrop))
        ############################################################## UP LEVEL
        
        Uplevel= table[table$link==paste0(selectRow$upName),] 
        if (length(Uplevel$fLevelImgPath)==0) {
          UpLeft=image_blank(512,512,color = "black") 
          UpRight= image_blank(512,512,color = "black")
        } else {
          
          up.img.left=Uplevel$leftName
          up.img.right=Uplevel$rightName
          
          foldUpLeft=strsplit(paste0(up.img.left), split = "_")[[1]][1]
          ulimg1= strsplit(paste0(up.img.left), split = "_")[[1]][2]
          ulimg<<-paste0(KMLpath,"\\",foldUpLeft, "\\",ulimg1)
          
          if (file.exists(ulimg)==T) {
            UpLeft=image_read(ulimg)
          } else
          {
            UpLeft=image_blank(512,512,color = "black")
          }  
          ######
          foldUpRight=strsplit(paste0(up.img.right), split = "_")[[1]][1]
          urimg1= strsplit(paste0(up.img.right), split = "_")[[1]][2]
          urimg<<-paste0(KMLpath,"\\",foldUpRight, "\\",urimg1)
          
          if (file.exists(urimg)==T) {
            UpRight=image_read(urimg)
          } else
          {
            UpRight= image_blank(512,512,color = "black")
          }}
        UplevelImg=image_append(c(UpLeft,up.img,UpRight))
        UpCrop=image_crop(UplevelImg, "1536x256+0+256")
        UpCrop1=image_crop(UpCrop, "1280x256+256")
        UpCrop2=image_crop(UpCrop1, "1024x256")
        ###
        leftRightUpJoin=image_append(c(UpCrop2,leftRightJoin), stack = T)
        #############################################################################   DOWN LEVEL
        
        Downlevel= table[table$link==paste0(selectRow$downName),] 
        if (length(Downlevel$fLevelImgPath)==0) {
          DownLeft= image_blank(512,512,color = "black")
          DownRight= image_blank(512,512,color = "black")
        } else {
          down.img.left=Downlevel$leftName
          down.img.right=Downlevel$rightName
          
          ############ 
          FolderDownLeft= strsplit(paste0(down.img.left), split = "_")[[1]][1] 
          dlimg1= strsplit(paste0(down.img.left), split = "_")[[1]][2] 
          dlimg<<-paste0(KMLpath,"\\",FolderDownLeft, "\\",dlimg1)  
          
          if (file.exists(dlimg)==T) {
            DownLeft=image_read(dlimg)
          } else
          {
            DownLeft= image_blank(512,512,color = "black")
          }
          ###########
          FolderDownRight=strsplit(paste0(down.img.right), split = "_")[[1]][1]
          drimg1= strsplit(paste0(down.img.right), split = "_")[[1]][2]
          drimg=paste0(KMLpath,"\\",FolderDownRight, "\\",drimg1) 
          if (file.exists(drimg)==T) {
            DownRight=image_read(drimg)
          } else {
            DownRight= image_blank(512,512,color = "black")
          }
        }
        DownlevelImg=image_append(c(DownLeft,down.img,DownRight))
        DownCrop=image_crop(DownlevelImg, "1536x256")
        DownCrop1=image_crop(DownCrop, "1280x256+256")
        DownCrop2=image_crop(DownCrop1, "1024x256")
        ############
        leftRightUpDownJoin=image_append(c(leftRightUpJoin,DownCrop2), stack = T)
        image_write(leftRightUpDownJoin,pthExpImg,format="jpg")
      }
    }
 
  }
  showNotification("Done") 
     stopCluster(cl)
#}
#Image_prepare()