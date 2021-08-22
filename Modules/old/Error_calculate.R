library(spatialEco)
library(dplyr)
date1=Sys.Date()

 
save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",SpeciesManager,"_", basename(labelInput), ".csv")
#ModelPoligon=paste0(labelInput,"\\Polygons\\Model\\Model.shp")
ModelPoligon=paste0(labelInput,"\\Polygons\\Houlout\\Houlout.shp")
ObserverPoint=paste0(labelInput,"\\Observer count\\",basename(labelInput),".shp")
PredictPoint=paste0(labelInput,"\\Predict\\",  SpeciesManager,"_",basename(labelInput), ".csv")

if(file.exists(ObserverPoint)==T) {


ModelPoligon1 = shapefile(ModelPoligon)
ObserverPoint1= shapefile(ObserverPoint)
PredictPoint1=read.csv(PredictPoint)


proj4string(ModelPoligon1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"		
proj4string(ObserverPoint1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	

coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
data   <- data.frame(age= PredictPoint1$age)   # data
crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)

ObserverPoint2 <- point.in.poly(ObserverPoint1,ModelPoligon1)
ObserverPoint3=as.data.frame(ObserverPoint2)
ObserverPoint4=ObserverPoint3[is.na(ObserverPoint3[,3]) == F,]
ObserverPoint5=data.frame(type="Observer",age=ObserverPoint4$LAYER.x,lat=ObserverPoint4$coords.x2,lon=ObserverPoint4$coords.x1) 
 
  if (SpeciesManager =="SSLAdult")   {ObserverPoint5=ObserverPoint5[ObserverPoint5$age != "P",]}
  if (SpeciesManager =="SSLPup")   {ObserverPoint5=ObserverPoint5[ObserverPoint5$age == "P",]}



PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
PredictPoint3=as.data.frame(PredictPoint2)
PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
PredictPoint5=data.frame(type="Predict",age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  

ObserverPoint6=ObserverPoint5 %>% group_by(age,type) %>% summarise(n())
PredictPoint6=PredictPoint5 %>% group_by(age,type) %>% summarise(n())
resultDiff=rbind(ObserverPoint6,PredictPoint6)
resultDiff=data.frame(resultDiff)

resultDiff$age=gsub("Sa","Bch",resultDiff$age)
resultDiff$age=gsub("J","Bch",resultDiff$age)
resultDiff1= data.frame(resultDiff %>%  group_by(type,age) %>% summarise(sum=sum(n..)))

resultDiff$age=gsub("Sa","Bch",resultDiff$age)
resultDiff$age=gsub("J","Bch",resultDiff$age)
resultDiff$age=gsub("TN","An",resultDiff$age)
resultDiff= data.frame(resultDiff %>%  group_by(type,age) %>% summarise(sum=sum(n..)))
resultDiff$sum=as.numeric(resultDiff$sum)

observer=resultDiff[resultDiff$type=="Observer",]
predict=resultDiff[resultDiff$type=="Predict",]

#resultDiff=data.frame(age=observer$age,observer=observer$sum,predict=predict$sum)

#resultDiffHoul=resultDiff[resultDiff$age %in% c("An" ,"Bch"),]

#resultDiff$diffPercent=(abs(resultDiff$observer-resultDiff$predict))/resultDiff$observer*100
 
 #for (i in 1:length(resultDiff$age)) {
 #if (resultDiff$observer[i] < resultDiff$predict[i]) {resultDiff$Error[i]= "Over"}
 #if (resultDiff$observer[i] > resultDiff$predict[i]) {resultDiff$Error[i]= "Under"}
 #if (resultDiff$observer[i] == resultDiff$predict[i]) {resultDiff$Error[i]= "Perfect"}
 #}
absolurError=abs(sum(ObserverPoint6[,3])-sum(PredictPoint6[,3]))/sum(ObserverPoint6[,3])*100
if (sum(ObserverPoint6[,3]) < sum(PredictPoint6[,3])) {AbsolutErrorDirection="Over"} else { AbsolutErrorDirection="Under"}

#HaulotError=abs(sum(resultDiffHoul$observer)-sum(resultDiffHoul$predict))/sum(resultDiffHoul$observer)*100
#if (sum(resultDiffHoul$observer) < sum(resultDiffHoul$predict)) {HaulotError1="Over"} else {HaulotError1="Under"}
HaulotError1="NO"
HaulotError="NO"
resultDiff="NO"

Report=list(date1=basename(labelInput),
                 resultDiff,
             AbsolutError=absolurError, 
             AbsolutErrorDirection=AbsolutErrorDirection,
             HaulotError=HaulotError,
			 HaulotErrorDirection=HaulotError1
			 )
             print(Report)

write.csv(Report,save_pth_check_diff)
}
ModelPoligon="_"
ObserverPoint="_"
PredictPoint="_"
Sys.sleep(5) 