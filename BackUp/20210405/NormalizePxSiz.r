
#NormalizePxSize=function (labelInput1=labelInput,
                         AnmlsMearDIR=paste0(labelInput,"\\Polygons\\Animal_measurments")
				           TilesTablePth=paste0(labelInput,"\\", basename(labelInput),"_table.csv")
						   Species
#				           NanmlsOnTiles1=NanmlsOnTiles
#				){
			

 if (Species=="SSLAdult"){NanmlsOnTiles<<-1.65
                           Up=NanmlsOnTiles*1.01
	                      Down=NanmlsOnTiles*0.9}
 if (Species=="NFSAdult"){NanmlsOnTiles<<-2.9
                          Up=NanmlsOnTiles*1.05
	                     Down=NanmlsOnTiles*0.95}
			
	#if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\animal_measurment")}
	#if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\animal_measurments")}
    #if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\Buls_measurement")}
	#if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\Animal_measurment")}
	#if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\Animal_mesurments")}
	#if(dir.exists(AnmlsMearDIR)==F)  {AnmlsMearDIR=paste0(labelInput,"\\Polygons\\Animal_measurments")}
	#if(dir.exists(AnmlsMearDIR)==F) {warning("No Animal_measurment found")}
	#if(dir.exists(AnmlsMearDIR)==T) {
	
		if (file.exists(AnmlsMearPth)==F){AnmlsMearPth=list.files(AnmlsMearDIR,full.names=T,pattern=".shp")}
		
		 
			 
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
   NumberAnmlsOnTiles1<-data.frame(date=basename(labelInput),NumberAnmlsOnTiles=NumberAnmlsOnTiles)

   
       NumberAnmlsOnTiles1$Norm=NanmlsOnTiles
	   
	  
	  
     print(NumberAnmlsOnTiles1$NumberAnmlsOnTiles)
	 
       NumberAnmlsOnTiles1$Diff=  NumberAnmlsOnTiles1$NumberAnmlsOnTiles/NumberAnmlsOnTiles1$Norm
	NumberAnmlsOnTiles1<<-NumberAnmlsOnTiles1
      if (NumberAnmlsOnTiles1$NumberAnmlsOnTiles > Up)  {stop(paste("Please_export_OPP", labelInput, "with pixel size= pixel size /",NumberAnmlsOnTiles1$Diff))}
      if (NumberAnmlsOnTiles1$NumberAnmlsOnTiles < Down)  {stop(paste("Please_export_OPP", labelInput, "with pixel size= pixel size /",NumberAnmlsOnTiles1$Diff))}
    ValueAction <<-NumberAnmlsOnTiles1$NumberAnmlsOnTiles
     print(NumberAnmlsOnTiles1)
	 NumberAnmlsOnTiles1
   
  }
  
 # }
     # print(NormalizePxSize)
 			#print(NormalizePxSize())