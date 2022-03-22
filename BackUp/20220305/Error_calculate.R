   library(spatialEco)
   library(dplyr)
   library(raster)
   library(rgdal)
   
   labelInput
   Species
   date1<<-substr(basename(labelInput),1,15)
   useModelPol=F


   ModelPoligonPTH=NULL
   HouloutPoligonPTH=NULL
   RookeryPolygonPTH=NULL
   ObserverPointPTH=NULL
   PredictPointPTH=NULL
   PredPTHr=""
   PredPTHh=""

                     
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
    
	if (length(ModelPoligonPTH) == 1  & length(ObserverPointPTH) == 1) {
	
	



 
##################################################################################################################
				
##############################################################################################################		
 if (Species %in% c("NFSAdult" ,"SSLAdult")) {		
		  ObserverPoint= readOGR(ObserverPointPTH)
          PredictPoint1=readOGR(PredictPointPTH_kml)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)   #ModelPoligonPTH
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  proj4string(PredictPoint1) <- crs
		  
		  
		  ObserverCount = ObserverPoint[!is.na(over(ObserverPoint,as(ModelPoligon1,"SpatialPolygons"))),] 
		  PredictCount= PredictPoint1[!is.na(over(PredictPoint1,as(ModelPoligon1,"SpatialPolygons"))),] 
		      
		
		  
		  PredictCount$age= PredictCount$Description 
		  PredictCount=data.frame(age=PredictCount$age)
		  ObserverCount=data.frame(ObserverCount)
		  
		  if("LAYER" %in% colnames(ObserverCount)){ObserverCount=data.frame(age= ObserverCount$LAYER)}
		  if("Description" %in% colnames(ObserverCount)){ObserverCount=data.frame(age= ObserverCount$Description)}
		  
		  ObserverCount$age= gsub("CU_AN","AN",ObserverCount$age)
		  ObserverCount$age= gsub("CU_TN","TF",ObserverCount$age)
		  ObserverCount$age= gsub("CU_TF","TF",ObserverCount$age)
		  ObserverCount$age= gsub("CU_F","F",ObserverCount$age)
		  ObserverCount$age= gsub("CU_D","U",ObserverCount$age)
		  ObserverCount$age= gsub("CU_UNK","U",ObserverCount$age)
		  ObserverCount$age= gsub("CU_SA","Bch",ObserverCount$age)
		  ObserverCount$age= gsub("CU_J","Bch",ObserverCount$age)
		  
		PredictCount1  = PredictCount %>% group_by(age) %>% summarise( count=n())
		ObserverCount1 = ObserverCount %>% group_by(age) %>% summarise( count=n())
		
		
	      Compare1=merge(PredictCount1,ObserverCount1,by="age",all=T)
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
###########################################################################################
if (Species %in% c("NFSPup","WLRS")  & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		



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
			 
 write.csv(Compare1,save_pth_check_diff); print(Report)          
}

#######################################################################################################
} else {print ("Model Poygon or observer count is not 1")}
