   library(spatialEco)
   library(dplyr)
   library(raster)

   labelInput
   Species


   ModelPoligonPTH=NULL
   HouloutPoligonPTH=NULL
   RookeryPolygonPTH=NULL
   ObserverPointPTH=NULL


                        Species1=Species
						ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                        HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Houlout")
                        RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", basename(labelInput), ".csv")
                        PredictPointPTH_SSL =  paste0(labelInput,"\\Predict\\",  basename(labelInput),"_",Species, "_AgeLatLon.csv")
                        PredictPointPTH_SSL_PUP = paste0(labelInput,"\\Predict\\", "SSLPup","_", basename(labelInput), ".csv")
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}



   if (dir.exists(ObserverPointDIR) & dir.exists(ModelPoligonDIR)) {
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern=".shp")[1] 
    
if (length(ModelPoligonPTH)>1 | length(HouloutPoligonPTH)>1 | length(RookeryPolygonPTH)>1 | length(ObserverPointPTH)>1) {
 stop("Only one shape file must by in one folder") }

save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species1,"_", basename(labelInput), ".csv")
 
##################################################################################################################
#  if (Species1=="SSLPup" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH)) {
#         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
#          PredictPoint=read.csv(PredictPointPTH)	
#ObserverPoint1=data.frame(lon=ObserverPoint$coords.x1, lat= ObserverPoint$coords.x2,   age= ObserverPoint$LAYER)  # type
#ObserverPoint2=ObserverPoint1[ObserverPoint1$age == "P",]

#ObserverPoint3=ObserverPoint2 %>% group_by(age) %>% summarise(ObserverCount=n())	
#PredictPoint1=PredictPoint %>% group_by(age) %>% summarise(PredictCount=n())	
#      Compare1 = merge(ObserverPoint3, PredictPoint1, by="age",all=T)
#	Compare1$CountDiff= Compare1$PredictCount- Compare1$ObserverCount
#	Compare1$PercentDiff= abs(Compare1$CountDiff)/Compare1$ObserverCount *100
#	TottalErrorCount=sum(Compare1$ObserverCount,na.rm = T) - sum(Compare1$PredictCount,na.rm = T)
#	TottalErrorPercent=TottalErrorCount/ sum(Compare1$ObserverCount,na.rm = T)  *100
#		Report<<-list(date=basename(labelInput),
#		            Species1=Species1,
 #                    Compare1,
#					 TottalErrorCount=TottalErrorCount,
#					TottalErrorPercent= TottalErrorPercent) 
#print(Report)
#Compare1$date=basename(labelInput)
	#write.csv(Compare1,save_pth_check_diff)					
#					}
##################################################################################################################
#######################################################################################################
#  if (Species1=="SSLAdult" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH_SSL)) {
#         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
#          PredictPoint=read.csv(PredictPointPTH_SSL)	#
		  
#ObserverPoint1=data.frame(lon=ObserverPoint$coords.x1, lat= ObserverPoint$coords.x2,   age= ObserverPoint$LAYER)  # type
#ObserverPoint2=ObserverPoint1[ObserverPoint1$age != "P" & ObserverPoint1$age != "DP",]
#ObserverPoint2$age=gsub("TN","TF",ObserverPoint2$age)
#ObserverPoint3=ObserverPoint2 %>% group_by(age) %>% summarise(ObserverCount=n())	
#PredictPoint1=PredictPoint %>% group_by(age) %>% summarise(PredictCount=n())	
#      Compare1 = merge(ObserverPoint3, PredictPoint1, by="age",all=T)
#	Compare1$CountDiff= Compare1$PredictCount- Compare1$ObserverCount
#	Compare1$PercentDiff= abs(Compare1$CountDiff)/Compare1$ObserverCount *100
#	TottalErrorCount=sum(Compare1$ObserverCount,na.rm = T) - sum(Compare1$PredictCount,na.rm = T)
#	TottalErrorPercent=TottalErrorCount/ sum(Compare1$ObserverCount,na.rm = T)  *100
#		Report<<-list(date=basename(labelInput),
#		            Species1=Species1,
 #                    Compare1,
	#				 TottalErrorCount=TottalErrorCount,
	#				TottalErrorPercent= TottalErrorPercent)            
	#				}
##########################################################################################################
 if (Species1=="SSLAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)  & file.exists(PredictPointPTH_SSL)) {
         ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH_SSL)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		if(file.exists(PredictPointPTH_SSL_PUP))  {PredictPointSSLPup=read.csv(PredictPointPTH_SSL_PUP);PredictPoint1=rbind(PredictPoint1,PredictPointSSLPup)}
		 
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0=as.data.frame(ObserverPoint0)
		 if (length(names(ObserverPoint0)) !=6){ ObserverPoint0=ObserverPoint0[is.na(ObserverPoint0$LAYER) == F,]
		ObserverPoint1=data.frame(lon=ObserverPoint0$coords.x1, lat= ObserverPoint0$coords.x2,   age= as.factor(ObserverPoint0$LAYER)) }
		if (length(names(ObserverPoint0)) ==6){ ObserverPoint0=ObserverPoint0[is.na(ObserverPoint0$LAYER.x) == F,]
		ObserverPoint1=data.frame(lon=ObserverPoint0$coords.x1, lat= ObserverPoint0$coords.x2,   age= as.factor(ObserverPoint0$LAYER.x))}
        
										
   
 #  ObserverPoint1$age=gsub("Sa","Bch",ObserverPoint1$age)
  # ObserverPoint1$age=gsub("J","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","TF",ObserverPoint1$age)
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())
   
 #  ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="P",]
  #ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="DP",]
ObserverPoint2=ObserverPoint2[ObserverPoint2$age !="Tr",]

coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
      
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
	if (exists("PredictPoint3$HEXID")){PredictPoint4=PredictPoint3[is.na(PredictPoint3$HEXID) == F,]}
	if (exists("PredictPoint3$HEXID")==F){PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]}
	
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	Compare1$date=basename(labelInput)
	
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

Report=list(date1=basename(labelInput),
             Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 ) 
print(Report)
Compare1$date=basename(labelInput)
Compare1$species=Species1
write.csv(Compare1,save_pth_check_diff)			 
					}			
##############################################################################################################		
 if (Species1=="NFSAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		
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
             Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
	WriteReport=data.frame(Report$Compare1,date1=basename(labelInput),Species=Species1)	 
	write.csv(WriteReport, save_pth_check_diff,row.names=F)
print(Report)	
           
}   
###########################################################################################
if (Species1 %in% c("WLRS","NFSPup")  & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		



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
             Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
			 
 write.csv(Compare1,save_pth_check_diff); print(Report)          
}					
########################################################################################################
###########################################################################################
} else {print("Data insufficient to Error calculate")}

