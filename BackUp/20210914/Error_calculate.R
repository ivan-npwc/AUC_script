   library(spatialEco)
   library(dplyr)
   library(raster)

   labelInput
   Species
   date1<<-substr(basename(labelInput),1,15)

   ModelPoligonPTH=NULL
   HouloutPoligonPTH=NULL
   RookeryPolygonPTH=NULL
   ObserverPointPTH=NULL


                     
						ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                        HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Houlout")
                        RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", date1, ".csv")
                        PredictPointPTH_SSL =  paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AgeLatLon.csv")
                        PredictPointPTH_SSL_PUP = paste0(labelInput,"\\Predict\\", "SSLPup","_", date1, ".csv")
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
                        AnimalsDensPTH= paste0(labelInput,"\\Predict\\",  date1,"_",Species, "_AnimalDense.csv")
if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}



   if (dir.exists(ObserverPointDIR) & dir.exists(ModelPoligonDIR)) {
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern=".shp")[1] 
    
if (length(ModelPoligonPTH)>1 | length(HouloutPoligonPTH)>1 | length(RookeryPolygonPTH)>1 | length(ObserverPointPTH)>1) {
 stop("Only one shape file must by in one folder") }

save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
 
##################################################################################################################
#  if (Species=="SSLPup" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH)) {
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
#		            Species=Species,
 #                    Compare1,
#					 TottalErrorCount=TottalErrorCount,
#					TottalErrorPercent= TottalErrorPercent) 
#print(Report)
#Compare1$date=basename(labelInput)
	#write.csv(Compare1,save_pth_check_diff)					
#					}
##################################################################################################################
#######################################################################################################
#  if (Species=="SSLAdult" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH_SSL)) {
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
#		            Species=Species,
 #                    Compare1,
	#				 TottalErrorCount=TottalErrorCount,
	#				TottalErrorPercent= TottalErrorPercent)            
	#				}
##########################################################################################################
 if (Species=="SSLAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {
 
         ObserverPoint= shapefile(ObserverPointPTH)
		 
	   ObserPTHD=paste0(labelInput,"\\Observer count"); ObserPTH=list.files(ObserPTHD, pattern=".shp", full.names=T);observC=data.frame(shapefile(ObserPTH))
      PredPTHh=paste0(labelInput,"\\Predict\\",date1,"_SSLAdult_HAULOUT.csv")
      PredPTHr=paste0(labelInput,"\\Predict\\",date1,"_SSLAdult_ROOKERY.csv")
	if (file.exists(PredPTHr)){
	 PredRook=read.csv(PredPTHr)
     PredHaul=read.csv(PredPTHh)
     PredictPoint1=rbind(PredRook,PredHaul)
	} else {
	PredictPoint1=read.csv(PredictPointPTH_SSL)
	}	  
	
	PredictPoint1$age=gsub("AF","F",PredictPoint1$age)
    PredictPoint1$age=gsub("Sa","SA",PredictPoint1$age)
	PredictPoint1$age=gsub("An","AN",PredictPoint1$age)
	
	
	
		  
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		if(file.exists(PredictPointPTH_SSL_PUP))  {PredictPointSSLPup=read.csv(PredictPointPTH_SSL_PUP);PredictPoint1=rbind(PredictPoint1,PredictPointSSLPup)}
		 
		  proj4string(ModelPoligon1) <- crs
		  proj4string(ObserverPoint) <- crs
		  pts = data.frame(ObserverPoint[!is.na(over(ObserverPoint,as(ModelPoligon1,"SpatialPolygons"))),])
		 
		ObserverPoint1=data.frame(lon=pts$coords.x1, lat= pts$coords.x2,   age= as.factor(pts$LAYER))
		
        
										
   
   ObserverPoint1$age=gsub("AF","F",ObserverPoint1$age)
   ObserverPoint1$age=gsub("Sa","SA",ObserverPoint1$age)
   ObserverPoint1$age=gsub("An","AN",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","TF",ObserverPoint1$age)
   
   if(file.exists(PredictPointPTH_SSL_PUP)==F){ObserverPoint1=ObserverPoint1[ObserverPoint1$age != "P",] }
   
   
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
	Compare1$date=date1
	Compare1$species=Species
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

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
#########################################################################################################	
#if (Species %in% c("WLRS")  & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {
#source("Modules/01_create_tile_polygons.r")
#library(dplyr)
#SpP_dataW=create_tile_polygons(labelInput)
#          ModelPoligon1=shapefile(ModelPoligonPTH)
#		  ObserverPoint=shapefile(ObserverPointPTH)
#          PredictPoint1=read.csv(PredictPointPTH)	
#		  
#		  proj4string(ModelPoligon1) <- crs
#		  proj4string(ObserverPoint) <- crs
#		  ##########
#coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
#data   <- data.frame(age= PredictPoint1$age)   # data
#PredictPoints <- SpatialPointsDataFrame(coords = coords,
#                                        data = data, 
#                                        proj4string = crs)
#		  ##################
#		  ObserverPoint =spTransform(ObserverPoint,crs)
#		  ModelPoligon1 = spTransform(ModelPoligon1,crs)
#		  PredictPoints = spTransform(PredictPoints,crs)
#		  #####################
 #         
#          proj4string(SpP_dataW) <- crs
#   AutoModelCount =  point.in.poly(PredictPoints,ModelPoligon1) #But that's not what next9 # please write what will next
#   AutoModelCount=AutoModelCount[is.na(AutoModelCount$FID)==F,]
#   ObserverModelCount = point.in.poly(ObserverPoint,ModelPoligon1)
#   ObserverModelCount=ObserverModelCount[is.na(ObserverModelCount$FID.y)==F,]
   
#  ObserverCount= data.frame(point.in.poly(ObserverPoint,SpP_dataW))
#   AutoCount= data.frame(point.in.poly(PredictPoints,SpP_dataW))
#   AutoModelCount= data.frame(point.in.poly(ObserverModelCount,SpP_dataW))
   
   
   ######################################################################
#AnimalsDens =NULL   
#   imgs=SpP_dataW$`rownames(srPolygonsData)`
   
 #    for (i in 1:length(imgs)) {
#   img=imgs[i]
#   area=SpP_dataW[SpP_dataW$`rownames(srPolygonsData)`== img,]
#   area1=areaPolygon(area)
   
#   PredIN =  PredictPoints %over% area
#   TotalPredCount= length(PredIN[,1][is.na(PredIN[,1])==F])
   
#   ObsIn= ObserverModelCount %over% area
#   ObsIncount= length(ObsIn[,1][is.na(ObsIn[,1])==F])
   
   
#   PredModelIN =  AutoModelCount %over% area
#   ModalPredCount= length(PredModelIN[,1][is.na(PredModelIN[,1])==F])
   
#   row1=data.frame(img=img, area=area1, ObserverCount=ObsIncount,TotalPredCount=TotalPredCount,ModalPredCount=ModalPredCount)
   
#   AnimalsDens=rbind(AnimalsDens,row1)  	
#}
###########################################################
#AnimalsDens$labelinput=labelinput
#write.csv(AnimalsDens,AnimalsDensPTH,row.names=F)

#AnimalsDens1$Myerr=abs(AnimalsDens1$TotalPredCount - AnimalsDens1$ModalPredCount)
#AnimalsDens1$diff=  as.numeric(abs(AnimalsDens1$ModalPredCount - AnimalsDens1$ObserverCount))
#AnimalsDens1$err= AnimalsDens1$diff/AnimalsDens1$ObserverCount*100
#AnimalsDens2=AnimalsDens1[!(AnimalsDens1$err %in% c("NaN", "Inf")),]
#AnimalsDens2$TotalPredCount=as.numeric(AnimalsDens2$TotalPredCount)
#hist(AnimalsDens2$diff ~ AnimalsDens2$TotalPredCount)

#pth="C:\\Users\\usato\\Documents\\AnimalsDens2.csv"
#tbl=read.csv(pth)
#tbl=as_tibble(tbl)

#tbl=tbl[tbl$TotalPredCount>0,] # WE USE FOR CORRECTION ONLY TILES WITH ANIMALS
#tbl=tbl[tbl$Myerr == 0,]      # WE USE ONLY TILES WERE MODEL COUNT=FUL COUNT # TO PREVENT SITUATION WHEN MODEL COUNT ~1, BUT TILE VERY DENSE AGREGATION
#tbl$diff=  tbl$ModalPredCount -tbl$ObserverCount # TO FINDE DIFFERENCE
#boxplot(tbl$diff~tbl$TotalPredCount)      # PLOT ERROR VIA DANSE
#abline(h = 0, col = "red")
#a=boxplot(tbl$diff~tbl$TotalPredCount,plot=F)
#median =c(data.frame(a$stats)[3,])   # FINDE MEDIAN ERROR VIA DENSE
#coorectTable=data.frame(TotalPredCount=as.numeric(a$names),correct=median)  # CREATE CORRECT TABLE VIA DENSE


#tbl=read.csv(pth)                                                         # READ ORIG FULL TABLE AGAIN
#tbl1=left_join(tbl,coorectTable, by="TotalPredCount")

#tbl1$CorrectCount = tbl1$TotalPredCount-tbl1$correct                   # ESRIMATE CORECTED COUNT ON EACH TILES BASED ON DANSE

#sum(tbl1$CorrectCount)
#sum(tbl1$TotalPredCount)



############################
#}				
########################################################################################################
###########################################################################################
} else {print("Data insufficient to Error calculate")}

