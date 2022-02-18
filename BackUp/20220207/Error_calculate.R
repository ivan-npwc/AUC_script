   library(spatialEco)
   library(dplyr)
   library(raster)

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
                        ObserverPointDIR=paste0(labelInput,"\\Observer count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", date1, ".csv")
                        PredictPointPTH_SSL =  paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AgeLatLon.csv")
                        PredictPointPTH_SSL_PUP = paste0(labelInput,"\\Predict\\", "SSLPup","_", date1, ".csv")
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
                        AnimalsDensPTH= paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AnimalDense.csv")
						save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
                       
						if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}
						if(dir.exists(PredictPointPTH_SSL)==F){PredictPointPTH_SSL=PredictPointPTH}



  
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern=".shp")[1] 
    
	if (length(ModelPoligonPTH) == 1  & length(ObserverPointPTH) == 1) {
	



 
##################################################################################################################
  if (Species=="SSLPup" &  file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH_SSL_PUP)) {
         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
          PredictPoint=read.csv(PredictPointPTH)	
ObserverPoint1=data.frame(lon=ObserverPoint$coords.x1, lat= ObserverPoint$coords.x2,   age= ObserverPoint$LAYER)  # type
ObserverPoint2=ObserverPoint1[ObserverPoint1$age == "P",]



#ObserverPoint3=ObserverPoint2 %>% group_by(age) %>% summarise(ObserverCount=n())	
#PredictPoint1=PredictPoint %>% group_by(age) %>% summarise(PredictCount=n())	
 #     Compare1 = merge(ObserverPoint3, PredictPoint1, by="age",all=T)
#	Compare1$CountDiff= Compare1$PredictCount- Compare1$ObserverCount
#	Compare1$PercentDiff= abs(Compare1$CountDiff)/Compare1$ObserverCount *100
    
	TottalErrorCount=length(ObserverPoint2$age) - length(PredictPoint$age)
    percent=TottalErrorCount/length(ObserverPoint2$age)*100
		
	diffPup=data.frame(date=date1, ObserverCount=length(ObserverPoint2$age),AutoCount=length(PredictPoint$age),TottalErrorCount=TottalErrorCount,percent=percent)
	write.csv(diffPup, save_pth_check_diff	)	
	print(diffPup)
			
					}
##################################################################################################################
##########################################################################################################
 if (Species=="SSLAdult") {
  ObserverPoint=NULL
  ModelPoligon1=NULL
  PredictPoint1=NULL
  Compare1=NULL
  Report=NULL
 
         ObserverPoint= shapefile(ObserverPointPTH)
	if (file.exists(PredPTHr)){
	       PredRook=read.csv(PredPTHr)
           PredHaul=read.csv(PredPTHh)
           PredictPoint1=rbind(PredRook,PredHaul) # NEED ADD PUP HERE
	} else {
	       PredictPoint1=read.csv(PredictPointPTH_SSL)  
	}
	##########	 
  if (useModelPol==F)  {
  
	PredictPoint1$area=NULL
	PredictPoint1$X=NULL
	PredictPoint1$Type="predict"
	pts=data.frame(ObserverPoint)
    ObserverPoint1=data.frame(lat= pts$coords.x2,lon=pts$coords.x1,   age= as.factor(pts$LAYER),Type="observer")
	count=rbind(PredictPoint1,ObserverPoint1)
	count= count %>%  filter(age != "P" & age != "DP" & age != "Tr")
    count1= count %>% group_by(Type,age) %>% summarise(count=n())
	count2= count1 %>% group_by(Type) %>% summarise(count=sum(count))
	TottalErrorIndivid= abs(count2$count[1]-count2$count[2])
	TottalErrorPercent=TottalErrorIndivid/count2$count[2]*100
	          count1$date1=date1
	          Compare1=count1
			 
	
	
	} else {	
##########################	
		 ModelPoligon1=shapefile(HouloutPoligonPTH)  # we use ALL ROOKERY !
		 
		 proj4string(ModelPoligon1) <- crs
		 proj4string(ObserverPoint) <- crs
		  
		pts = data.frame(ObserverPoint[!is.na(over(ObserverPoint,as(ModelPoligon1,"SpatialPolygons"))),]) 
		ObserverPoint1=data.frame(lon=pts$coords.x1, lat= pts$coords.x2,   age= as.factor(pts$LAYER))
	#############	 
	
      PredPTHh=paste0(labelInput,"\\Predict\\",date1,"_SSLAdult_HAULOUT.csv")
      PredPTHr=paste0(labelInput,"\\Predict\\",date1,"_SSLAdult_ROOKERY.csv")
	
		
	
	
	PredictPoint1$age=gsub("AF","F",PredictPoint1$age)
    PredictPoint1$age=gsub("Sa","SA",PredictPoint1$age)
	PredictPoint1$age=gsub("An","AN",PredictPoint1$age)	  
		  
	#	if(file.exists(PredictPointPTH_SSL_PUP))  {PredictPointSSLPup=read.csv(PredictPointPTH_SSL_PUP);PredictPointSSLPup$area=NULL;# #PredictPoint1=rbind(PredictPoint1,PredictPointSSLPup)}									
   
   ObserverPoint1$age=gsub("AF","F",ObserverPoint1$age)
   ObserverPoint1$age=gsub("Sa","SA",ObserverPoint1$age)
   ObserverPoint1$age=gsub("An","AN",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","TF",ObserverPoint1$age)
   
 #  if(file.exists(PredictPointPTH_SSL_PUP)==F){ObserverPoint1=ObserverPoint1[ObserverPoint1$age != "P",] }
   
   
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())
   
  #ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="P",]
  #ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="DP",]
  ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="Tr",]

    coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
      
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
  
	
	PP2 = data.frame(PredictPoints[!is.na(over(PredictPoints,as(ModelPoligon1,"SpatialPolygons"))),]) 
	PredictPoint3=data.frame(lon=PP2$lon, lat= PP2$lat,   age= PP2$age)
	
  
	
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	Compare1$date=date1
	Compare1$species=Species
	
	Compare1=Compare1 %>% filter(age != "P" & age != "DP")
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
}	

Report=list(date1=date1,
             Species=Species,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 ) 
print(Report)


write.csv(Compare1,save_pth_check_diff,row.names=F)			 
}
				
##############################################################################################################		
 if (Species=="NFSAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		
		  ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0_1=as.data.frame(ObserverPoint0)
    if (length(ObserverPoint0_1$HEXID) !=0) { ObserverPoint0_2=ObserverPoint0_1[is.na(ObserverPoint0_1$HEXID) == F,]
       ObserverPoint1=data.frame(lon=ObserverPoint0_2$coords.x1, lat= ObserverPoint0_2$coords.x2,   age= as.factor(ObserverPoint0_2$LAYER))
	}
	if (length(ObserverPoint0_1$HEXID)==0) { ObserverPoint0_2=ObserverPoint0_1[is.na(ObserverPoint0_1$LAYER.y) == F,]
       ObserverPoint1=data.frame(lon=ObserverPoint0_2$coords.x1, lat= ObserverPoint0_2$coords.x2,   age= as.factor(ObserverPoint0_2$LAYER.x))	}
	
		  
   ObserverPoint1$age=gsub("Sa","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("J","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","An",ObserverPoint1$age)
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())
	ObserverPoint2=ObserverPoint2[ObserverPoint2$age != "Tr",]	 

		
        coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
 
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
 

    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
if (length(PredictPoint3$HEXID) !=0){	
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$HEXID) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1) 
	                             }

if (length(PredictPoint3$HEXID)==0){	
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1) 
	                             }

								 
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

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
