
                 date1=substr(basename(labelInput),1,15)
                   AnmlsMearDIR=paste0(labelInput,"\\Polygons\\Animal_measurements")
				 if (dir.exists(AnmlsMearDIR)==F){AnmlsMearDIR=paste0(labelInput,"\\Polygons\\Animal_measurments")}  
				   TilesTablePth=paste0(labelInput,"\\", date1,"_table.csv")
				   Species
			

 if (Species=="SSLAdult"){NanmlsOnTiles<<-1.65
                           Up=NanmlsOnTiles*1.01
	                      Down=NanmlsOnTiles*0.95}
 if (Species=="NFSAdult"){NanmlsOnTiles<<-2.3    # 2.9 - usualy
                          Up=NanmlsOnTiles*1.01
	                      Down=NanmlsOnTiles*0.95}					 
if (Species=="WLRS")     {NanmlsOnTiles<<-2.5    # 2.9- error 2.7 (over)  3.1 error under 30% 3.2 error under 56% ### 2.4- 28.5% under
                          Up=NanmlsOnTiles*1.01
	                     Down=NanmlsOnTiles*0.95}
						 
if (Species=="LRG")     {NanmlsOnTiles<<-1.65    # 
                          Up=NanmlsOnTiles*1.01
	                     Down=NanmlsOnTiles*0.95}						 
						 

if (Species=="NFSPup")     {NanmlsOnTiles <<- 4   #  4
                          Up=NanmlsOnTiles*1.01
	                     Down=NanmlsOnTiles*0.95}
	
		if (dir.exists(AnmlsMearDIR)){AnmlsMearPth=list.files(AnmlsMearDIR,full.names=T,pattern=".shp")}	 
		if (file.exists(AnmlsMearPth)==T & file.exists(TilesTablePth)==T) {
		
	TilesTable=read.csv(TilesTablePth)
	p1=data.frame(lon=TilesTable$west,lat=TilesTable$south)
	p2=data.frame(lon=TilesTable$east, lat=TilesTable$north)
	
	AnmlsMear=shapefile(AnmlsMearPth)
	AnmlsMear1=NULL
  for (i in 1:length(AnmlsMear$LAYER)) {
         row1=data.frame(sizeAnml=LinesLength(AnmlsMear@lines[[i]],longlat = T)*1000)  # size animal in meters
		 AnmlsMear1=rbind(row1,AnmlsMear1)
	   }
   meanSizeAnml=mean(AnmlsMear1$sizeAnml) # mean size of animal 
   tileSizeTable= pointDistance(p1,p2,lonlat=T)    # tiles size
   meanTilesSize=mean(tileSizeTable)
   NumberAnmlsOnTiles<-meanTilesSize/meanSizeAnml
   NumberAnmlsOnTiles1<-data.frame(date=date1,NumberAnmlsOnTiles=NumberAnmlsOnTiles)

   
       NumberAnmlsOnTiles1$Norm=NanmlsOnTiles
	   
	  
	  
     print(NumberAnmlsOnTiles1$NumberAnmlsOnTiles)
	 
       NumberAnmlsOnTiles1$Diff=  NumberAnmlsOnTiles1$NumberAnmlsOnTiles/NumberAnmlsOnTiles1$Norm
	NumberAnmlsOnTiles1<-NumberAnmlsOnTiles1
      if (NumberAnmlsOnTiles1$NumberAnmlsOnTiles > Up)  {stop(paste("Please_export_OPP", labelInput, "with pixel size= pixel size /",NumberAnmlsOnTiles1$Diff))}
      if (NumberAnmlsOnTiles1$NumberAnmlsOnTiles < Down)  {stop(paste("Please_export_OPP", labelInput, "with pixel size= pixel size /",NumberAnmlsOnTiles1$Diff))}
    ValueAction <<-NumberAnmlsOnTiles1$NumberAnmlsOnTiles
     print(NumberAnmlsOnTiles1)
	 NumberAnmlsOnTiles1
   
  } else {stop(paste("No Animal measurments dir found   ", labelInput))}
  
 # }
     # print(NormalizePxSize)
 			#print(NormalizePxSize())