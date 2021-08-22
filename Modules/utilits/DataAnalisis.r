
library(dplyr)
TableCount=NULL
pthDir="D:\\NFS\\2019_138_OPP"
listSubdir=list.files(pthDir,full.names = T)
#################################################################   count
for (i in 1:length(listSubdir)) {
  NameDir=paste0(listSubdir[i])
  date1=basename(NameDir)
pth=paste0(listSubdir[i],"\\Predict\\NFSAdult_",date1,".csv")

if (file.exists(pth)==T) {

tblMY=read.csv(pth)
tblMY=tblMY[,-1]
tblMY$date=date1

tblMY1=tblMY %>%
 # select(age,date)%>%
  group_by(age,date) %>%
  summarise(n())

tblMY1=data.frame(tblMY1)
TableCount=rbind(tblMY1,TableCount)
}}
##############################################   diff
########################################################################################
library(raster)
library(spatialEco)
library(dplyr)

SummaryTable=NULL
pthFolders="D:\\NFS\\2019_138_OPP"
listFolder=list.files(pthFolders,full.names=T)


for (i in 1:length(listFolder)) {

date1=basename(listFolder[i])
PthModelCount=paste0(pthFolders,"\\",date1,"\\Observer count\\",date1,".shp")   
if (file.exists(PthModelCount)==T) {            
ModelPoligon=paste0(pthFolders,"\\",date1,"\\Polygons\\Model\\Model.shp")
PredictPoint=paste0(pthFolders,"\\",date1,"\\Predict\\NFSAdult_",date1,".csv")
save_pth_check_diff=paste0(pthFolders,"\\",date1,"\\Predict\\Check_difference", date1, ".csv")

ModelPoligon1 = shapefile(ModelPoligon)
ObserverPoint1= shapefile(PthModelCount)
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

PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
PredictPoint3=as.data.frame(PredictPoint2)
PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
PredictPoint5=data.frame(type="Predict",age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  

ObserverPoint6=data.frame(ObserverPoint5 %>% group_by(age,type) %>% summarise(n()))
allObserverRow=data.frame(age="all",type="Observer",n..=sum(ObserverPoint6$ n..))
PredictPoint6=data.frame(PredictPoint5 %>% group_by(age,type) %>% summarise(n()))
allPredictRow=data.frame(age="all",type="Predict",n..=sum(PredictPoint6$ n..))

resultDiff=rbind(ObserverPoint6,PredictPoint6,allObserverRow,allPredictRow)
resultDiff=data.frame(resultDiff)
resultDiff$date=date1

SummaryTable=rbind(SummaryTable,resultDiff)
}}

SummaryTable$age=gsub("Sa","Bch",SummaryTable$age)
SummaryTable$age=gsub("J","Bch",SummaryTable$age)
SummaryTable$age=gsub("TN","An",SummaryTable$age)
SummaryTable= data.frame(SummaryTable %>%  group_by(type,age,date) %>% summarise(sum=sum(n..)))

SummaryTable$link=paste0(SummaryTable$date,"_", SummaryTable$age)


observerTable=data.frame(SummaryTable[SummaryTable$type=="Observer",])
predictTable= data.frame(SummaryTable[SummaryTable$type=="Predict",])

ResDiff=left_join(predictTable,observerTable,by="link")

ResDiff$diffAnimals= ResDiff$sum.x-ResDiff$sum.y
ResDiff$diffPercent=   abs(ResDiff$diffAnimals)/ResDiff$sum.y*100
#################################################################################################################
################################################################################################################

pthFolders="D:\\NFS\\2019_138_OPP"
listFolder=list.files(pthFolders,full.names=T)
ResErrorCount=NULL

for (i in 1:length(listFolder)) {

date1=basename(listFolder[i])
PthErrorCount=paste0(pthFolders,"\\",date1,"\\Points\\Error\\Error.shp")   
if (file.exists(PthErrorCount)==T) {  
ErrorCount = shapefile(PthErrorCount)
ErrorCount=data.frame(ErrorCount)
ErrorCount$date=date1
ErrorCount1=ErrorCount %>%
           # select(LAYER,date) %>%
			group_by(LAYER,date) %>%
			summarise(n=n())
ErrorCount1=data.frame(ErrorCount1)
ResErrorCount =rbind(ErrorCount1,ResErrorCount)
}}

ResErrorCount$LAYER=gsub("Error_Anoyher_Species","Error_Another_Species",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_NFS_pup","NFS_pup",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_Double_Animal","Double_click",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_Another_Species","SSL",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_Texture","Background",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_Split_Animals","Split_Animal",ResErrorCount$LAYER)
ResErrorCount$LAYER=gsub("Error_No_Point","No_Point",ResErrorCount$LAYER)


ResErrorCount1 =ResErrorCount %>%
               select(Type_error =LAYER,n) %>%
			   group_by(Type_error) %>%
			   summarise(Count_error =sum(n))
ResErrorCount1 =data.frame(ResErrorCount1[order(ResErrorCount1$Count_error),])	
############################################################################################GRAFF
library(ggplot2)
ResErrorCount1$fill1=c("Over","Over","Over","Over","Over","Under","Under")
ggplot(ResErrorCount1, aes(x= reorder(Type_error,Count_error)   , y= Count_error)) +
     geom_bar(stat="identity", fill="lightblue", colour="black")+
      xlab("Type error")
#################################################################		   
for (i in 1:length(ResDiff$type.y)) {
  
  if (ResDiff$diff[i] <0 ) {# ResDiff$DiffPercentMinus[i]=  0- ResDiff$diffPercent[i]
                                    ResDiff$fill[i]=  "Uder"               
  } else  { #ResDiff$DiffPercentMinus[i]= ResDiff$diffPercent[i]
                                    ResDiff$fill[i]=  "Over"   }
  
}


AllError=ResDiff[ResDiff$age.y=="Bch",]

AllError=ResDiff[ResDiff$age.y %in% c("Bch","An"),]

AllError= AllError %>%
        #  select(date.x,sum.x,sum.y) %>%
		  group_by(date.x) %>%
		  summarise(PredictCount=sum(sum.x),
		            ObsercerCount=sum(sum.y),
		            diff1=ObsercerCount-PredictCount
					)
										

AllError$Diffpercent= abs(AllError$diff1)/AllError$ObsercerCount*100

for (i in 1:length(AllError$date.x)) {
  if (AllError$diff1[i] <0 ) { AllError$DiffPercentMinus[i]=  0- AllError$Diffpercent[i]
  AllError$fill[i]=  "Uder"               
  } else  { AllError$DiffPercentMinus[i]= AllError$Diffpercent[i]
  AllError$fill[i]=  "Over"   }
}
		

TableCount=AllError[AllError$date.x %in% c("20190615","20190707","20190801"),]


		
AllError=AllError[AllError$date.x !="20190727",]					
					

ggplot(ResDiff, aes(x=date.x, y=DiffPercentMinus,fill=fill)) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
    theme(axis.text.x= element_text(angle=90,hjust=1))+
    xlab("Date")+
    ylab("Difference (%)")+
    ggtitle("Female count on model sites")
	#annotate(geom="text",x=43,y=33,label="Houlout",color="red",size=13)
########################################################################################################################
TableCount=as_tibble(TableCount)

TableCount$age=gsub("An","Bch",TableCount$age)

TableCount1=TableCount[TableCount$age =="Bch",]
TableCount1=TableCount1[!(TableCount1$date %in% c("20190727","20190808")),]

PlotTableCount=TableCount1 %>%
          #     select (n..,date) %>%
               group_by(date) %>%
			   summarise(TottalCount=sum(n..))

ggplot(PlotTableCount, aes(x=date, y=TottalCount,fill="red")) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
    theme(axis.text.x= element_text(angle=90,hjust=1))+
    xlab("Date")+
    ylab("Count, individual")+
    ggtitle("TF count")
#######################################################################################
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
###########
####################################################################################
library(raster)
library(spatialEco)
library(dplyr)

SummaryTable=NULL
pthFolders="D:\\NFS\\2019_138_OPP"
listFolder=list.files(pthFolders,full.names=T)


for (i in 1:length(listFolder)) {

date1=basename(listFolder[i])
PthModelCount=paste0(pthFolders,"\\",date1,"\\Observer count\\",date1,".shp")   
if (file.exists(PthModelCount)==T) {            
ModelPoligon=paste0(pthFolders,"\\",date1,"\\Polygons\\Model\\Model.shp")
PredictPoint=paste0(pthFolders,"\\",date1,"\\Predict\\NFSAdult_",date1,".csv")
save_pth_check_diff=paste0(pthFolders,"\\",date1,"\\Predict\\Check_difference", date1, ".csv")

ModelPoligon1 = shapefile(ModelPoligon)
ObserverPoint1= shapefile(PthModelCount)
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
ObserverPoint5= ObserverPoint4 %>%
               select(ModelPoligon=FID.y,sex=LAYER.x) %>%
               group_by(ModelPoligon,sex)    %>%
               summarise(Count=n())
ObserverPoint5$Type="Observer"

PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
PredictPoint3=as.data.frame(PredictPoint2)
PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
PredictPoint5= PredictPoint4 %>%
               select(ModelPoligon=FID,sex=age) %>%
               group_by(ModelPoligon,sex)    %>%
               summarise(Count=n())
PredictPoint5$Type="Model"
resultDiff=rbind(ObserverPoint5,PredictPoint5)
 resultDiff$date=date1
 
SummaryTable=rbind(SummaryTable,resultDiff)
}}


SummaryTable$sex=gsub("Sa","Bch",SummaryTable$sex)
SummaryTable$sex=gsub("J","Bch",SummaryTable$sex)
SummaryTable$sex=gsub("TN","An",SummaryTable$sex)
SummaryTable1= SummaryTable %>%  group_by(Type,sex,date,ModelPoligon) %>% summarise(sum=sum(Count))





SummaryTable1$link=paste0(SummaryTable1$date,"_", SummaryTable1$sex, "_",SummaryTable1$ModelPoligon)


observerTable=SummaryTable1[SummaryTable1$Type=="Observer",]
predictTable= SummaryTable1[SummaryTable1$Type=="Model",]

ResDiff=left_join(predictTable,observerTable,by="link")

ResDiff$diffAnimals= ResDiff$sum.x-ResDiff$sum.y
ResDiff$diffPercent=   abs(ResDiff$diffAnimals)/ResDiff$sum.y*100




