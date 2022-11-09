   library(spatialEco)
   library(dplyr)
   library(raster)
   library(rgdal)
   
   labelInput
   Species
   
   date1<<-substr(basename(labelInput),1,15)
   HaulAsModel
   ObserverErrCompensate=F
   kmlCheck=F
   
   
   ModelPoligonPTH=NULL
   HouloutPoligonPTH=NULL
   RookeryPolygonPTH=NULL
   ObserverPointPTH=NULL
   PredictPointPTH=NULL
   PredPTHr=""
   PredPTHh=""
                       AgeCategoryAcept=c("AN","TF","TN","F","U","SA","J","P","DP","An","Tf", "AF","Sa","p","OM","Bch")
                     
					 
					 
						ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                        HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Haulout")
                        RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer_count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", date1, ".csv")
						PredictPointPTH_kml = paste0(labelInput,"\\Predict\\", Species,"_", date1, ".kml")
                        PredictPointPTH_SSL =  paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AgeLatLon.csv")
                        PredictPointPTH_SSL_PUP = paste0(labelInput,"\\Predict\\SSLPup_", date1,".kml")
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
                        AnimalsDensPTH= paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AnimalDense.csv")
						save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
                       
						if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer count")}
						if(dir.exists(PredictPointPTH_SSL)==F){PredictPointPTH_SSL=PredictPointPTH}



  
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern="shp|kml")[1] 

    if (HaulAsModel==T) {ModelPoligonPTH=HouloutPoligonPTH}
    
  if (length(ModelPoligonPTH) == 1  & is.na(ObserverPointPTH) == F & file.exists(PredictPointPTH_kml)) {  # every thin is perfect
##################################################################################################################				
##############################################################################################################		
 if (Species %in% c("NFSadult","NFSAdult" ,"SSLAdult","NFSPup")) {		
		  ObserverPoint= readOGR(ObserverPointPTH)
          PredictPoint1=readOGR(PredictPointPTH_kml)
		  
		  ModelPoligon1=readOGR(ModelPoligonPTH)   
	if (length(RookeryPolygonPTH) !=0)	 {RookeryPol=readOGR(RookeryPolygonPTH);proj4string(RookeryPol) <- crs}
		  HouloutPol=readOGR(HouloutPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  proj4string(PredictPoint1) <- crs
		  
		  proj4string(HouloutPol) <- crs
		  
		 if(Species=="NFSPup"){PredictPoint1=PredictPoint1[PredictPoint1$Description=="P",]} 
		  
		  ObserverCount0 = ObserverPoint[!is.na(over(ObserverPoint,as(ModelPoligon1,"SpatialPolygons"))),]
		  ##############################################################################################
		   ObserverCount0$LAYER= gsub("CU_AN","AN",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_TN","TF",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_TF","TF",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_F","F",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_D","U",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_UNK","U",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_SA","Bch",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("CU_J","Bch",ObserverCount0$LAYER)
		  
		  ObserverCount0$LAYER= gsub("SSL_AN","AN",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_TN","TF",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_TF","TF",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_F","F",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_D","U",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_UNK","U",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_SA","SA",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_J","J",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("SSL_P","P",ObserverCount0$LAYER)
		  
		  ObserverCount0$LAYER= gsub("TN","TF",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("OM","AN",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("Sa","SA",ObserverCount0$LAYER)
		  ObserverCount0$LAYER= gsub("An","AN",ObserverCount0$LAYER)
		  ########################################################################################################### filter observer count by Haulout and Rookery Pol
		 if (ObserverErrCompensate==T){
		 
 	      RtoH=NULL
		  ObsrvrCtRFlt=NULL
		  ObsrvrCntH=NULL

		  ObserverCount01 = ObserverCount0[!is.na(over(ObserverCount0,as(HouloutPol,"SpatialPolygons"))),] # over Haulout 
          ObsrvrCntR=ObserverCount01[ObserverCount01$LAYER %in% c("TF","F"),]                              # Obs count age TF F
		  ObsrvrCntH=ObserverCount01[!ObserverCount01$LAYER %in% c("TF","F"),]                             # Obs Count age AN Bch
		
		if (length(RookeryPolygonPTH) !=0)	 { 
		 ObsrvrCtRFlt=  ObsrvrCntR[!is.na(over(ObsrvrCntR,as(RookeryPol,"SpatialPolygons"))),]             # TF F filter in ROKKERY Pol
		  RtoH= ObsrvrCntR[is.na(over(ObsrvrCntR,as(RookeryPol,"SpatialPolygons"))),]                       # TF F filter NOT in ROKKERY Pol
		
		  RtoH$LAYER= gsub("TF","AN",RtoH$LAYER)
		  RtoH$LAYER= gsub("F","Bch",RtoH$LAYER)
		  }
		  ObserverCount=rbind(RtoH,ObsrvrCtRFlt,ObsrvrCntH)
		  }
		  ########################################################################################################## 
		   
		   if (length(ObserverCount0)==0) {ObserverCount=data.frame(age=NA)}			  
			  
		  PredictCount= PredictPoint1[!is.na(over(PredictPoint1,as(ModelPoligon1,"SpatialPolygons"))),] 
		      
		
		  
		  PredictCount$age= PredictCount$Description 
		  PredictCount=data.frame(age=PredictCount$age)
		  ObserverCount=data.frame(ObserverCount0)
		  
		  if("LAYER" %in% colnames(ObserverCount)){ObserverCount=data.frame(age= ObserverCount$LAYER)}
		  if("Description" %in% colnames(ObserverCount)){ObserverCount=data.frame(age= ObserverCount$Description)}
		  
		 
	if (Species==  "NFSAdult") {
	 ObserverCount$age= gsub("SA","Bch",ObserverCount$age)
	  ObserverCount$age= gsub("J","Bch",ObserverCount$age)
	}
		  
		  
		  ObserverCount1=ObserverCount %>% filter (age %in% AgeCategoryAcept)
		   
		PredictCount1  = PredictCount %>% group_by(age) %>% summarise( count=n())
		ObserverCount2 = ObserverCount1 %>% group_by(age) %>% summarise( count=n())
		
		
	      Compare1=merge(PredictCount1,ObserverCount2,by="age",all=T)
		 names(Compare1)=c("age","AutoCount", "ObserverCount")
		  
	    Compare1$diffInd=Compare1$AutoCount-Compare1$ObserverCount
	    Compare1$diffPerc=Compare1$diffInd/Compare1$ObserverCount*100
	 
			 
			 observerCountAdult=sum(Compare1$ObserverCount[Compare1$age!= "P"],na.rm = T)
			 autocountAdult=sum(Compare1$AutoCount[Compare1$age!= "P"],na.rm = T)
			 TottalErrorIndivid=abs(observerCountAdult- autocountAdult)
			 TottalErrorPercent=TottalErrorIndivid/observerCountAdult*100
	
	

Report=list(date1=basename(labelInput),
             Species=Species,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
	WriteReport=data.frame(Report$Compare1,date1=basename(labelInput),Species=Species)	 
	write.csv(WriteReport, save_pth_check_diff,row.names=F)
print(Report)	

           
}
############################################
if (kmlCheck==T){
		    opp=labelInput
              ObserverPointPTH=list.files(paste0(opp,"\\Observer_count"),pattern="shp", full.names=T)
              ModelPoligonPTH=list.files(paste0(opp,"\\Polygons\\Model"),pattern="shp", full.names=T)
                 ObserverPoint= readOGR(ObserverPointPTH)
		         ObserverPoint=ObserverPoint[ObserverPoint$LAYER %in% c("AN","Bch"),]
		        ModelPoligon1=readOGR(ModelPoligonPTH)   	 
	        saveObserver=gsub(".shp",".kml",ObserverPointPTH)
           saveModel=gsub(".shp",".kml",ModelPoligonPTH)	
	  writeOGR(ObserverPoint,  saveObserver, layer=ObserverPoint$LAYER, "KML",overwrite_layer=TRUE)
	  writeOGR(ModelPoligon1,  saveModel, "HEXID", "KML",overwrite_layer=TRUE)	 
}   
###########################################################################################
if (Species %in% c("WLRS")  & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		



         ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  
		  ObserverPoint =spTransform(ObserverPoint,crs)
		  ModelPoligon1 = spTransform(ModelPoligon1,crs)
		  
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0=as.data.frame(ObserverPoint0)
         ObserverPoint0=ObserverPoint0[is.na(ObserverPoint0$LAYER.y) == F,]
		
        coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data

       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
		PredictPoints=	spTransform(PredictPoints,crs)

		
   ObserverPoint1=data.frame(lon=ObserverPoint0$coords.x1, lat= ObserverPoint0$coords.x2,   age= as.factor(ObserverPoint0$LAYER.x))
   
   
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())

    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	
	
	
	
	
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

Report<<-list(date1=basename(labelInput),
             Species=Species,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
			 
 write.csv(Compare1,save_pth_check_diff, row.names=F); print(Report)          
}

####################################################################################################### NO OBSERVER COUNT NO AUTO COUNT RESULTS= NO ANIMALS
}
  if (is.na(ObserverPointPTH) == T & file.exists(PredictPointPTH_kml)==F) {
   Compare1=data.frame(age=NA, AutoCount=0, ObserverCount=0, diffInd=0, diffPerc=0,date1=date1, Species=Species)
    if(dir.exists(paste0(labelInput,"\\Predict"))){ write.csv(Compare1,save_pth_check_diff, row.names=F)}
	print(Compare1)
}
 ########################################################################################### NO OBSERVER COUNT, BUT AUTO COUNT EXISTS
  if (is.na(ObserverPointPTH) == T & file.exists(PredictPointPTH_kml)) {
          PredictPoint1=readOGR(PredictPointPTH_kml)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)   #ModelPoligonPTH
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(PredictPoint1) <- crs
		  PredictCount= PredictPoint1[!is.na(over(PredictPoint1,as(ModelPoligon1,"SpatialPolygons"))),] 
   if (length(PredictCount$Name)>0)	{		    
			
		  PredictCount$age= PredictCount$Description 
		  PredictCount1=data.frame(age=PredictCount$age)
		  PredictCount2  = PredictCount1 %>% group_by(age) %>% summarise(count=n())
          Compare1=data.frame(age=PredictCount2$age, AutoCount=PredictCount2$count, ObserverCount=0, diffInd=PredictCount2$count, diffPerc=NA,date1=date1, Species=Species)
		  }
   if (length(PredictCount$Name)==0)	{
   
      Compare1=data.frame(age=NA, AutoCount=0, ObserverCount=0, diffInd=0, diffPerc=0,date1=date1, Species=Species)
}   
		  write.csv(Compare1,save_pth_check_diff, row.names=F) 
	       print(Compare1)
  
  }
 ################################################################################## 
  if (is.na(ObserverPointPTH) == F & file.exists(PredictPointPTH_kml)==F) {   #  observer count exists but no auto count 
  
          ObserverPoint= readOGR(ObserverPointPTH)
		  ModelPoligon1=shapefile(ModelPoligonPTH)   #ModelPoligonPTH
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  ObserverCount = ObserverPoint[!is.na(over(ObserverPoint,as(ModelPoligon1,"SpatialPolygons"))),] 
  if (length(ObserverCount$LAYER)>0)	{	
		  ObserverCount=data.frame(ObserverCount)
          ObserverCount1  = ObserverCount %>% group_by(age) %>% summarise(count=n())
          Compare1=data.frame(age=ObserverCount1$age, AutoCount=0, ObserverCount=ObserverCount1$count, diffInd=ObserverCount1$count, diffPerc=NA,date1=date1, Species=Species)
           }
 if (length(ObserverCount$LAYER)==0){                   # if NO points in haul pol
          Compare1=data.frame(age=NA, AutoCount=0, ObserverCount=0, diffInd=0, diffPerc=0,date1=date1, Species=Species)
         }
		   write.csv(Compare1,save_pth_check_diff, row.names=F) 
	       print(Compare1)
  }
 ##################################################################################################### err for each model pol as indepent 
 
#          ObserverPoint= readOGR(ObserverPointPTH)
#          PredictPoint1=readOGR(PredictPointPTH_kml)	
#		  ModelPoligon1=shapefile(ModelPoligonPTH)   #ModelPoligonPTH
		 
#		  proj4string(ModelPoligon1) <- crs
#		  proj4string(ObserverPoint) <- crs
#		  proj4string(PredictPoint1) <- crs
		  
#		  ObserverPoint =spTransform(ObserverPoint,crs)
#		  ModelPoligon1 = spTransform(ModelPoligon1,crs)
 
 #         polCount1=NULL
  
  #         for (i in 1:length(ModelPoligon1)){
	#	   pol=ModelPoligon1[i,]
		   
#		   ObserverCount = ObserverPoint[!is.na(over(ObserverPoint,as(pol,"SpatialPolygons"))),]
 #          if (length(ObserverCount)==0) { ObserverCount=data.frame(age= NA,type="Observer",pol=i)}
		    
			  
	#	  PredictCount= PredictPoint1[!is.na(over(PredictPoint1,as(pol,"SpatialPolygons"))),] 
		       if (length(PredictCount)==0) { PredictCount=data.frame(age= NA,type="Predict",pol=i)}
		
	
#		  PredictCount=data.frame(age=PredictCount$Description,type="Predict",pol=i)
#		  ObserverCount=data.frame(age= ObserverCount$LAYER,type="Observer",pol=i)
		   
#		   polCount=rbind(PredictCount,ObserverCount)
		   
#		  polCount1= polCount %>% group_by(age,type) %>% summarize(count=n())
		   
		   
		   
#		   res1=	polCount1 %>% spread (type,count)
		   
#		   res1$percent= (res1$Observer - res1$Predict)/res1$Observer*100
		   
#		   res1$pol=i
		   
		   
#		   }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  