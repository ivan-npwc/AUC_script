

AGE="Bch"

library(ggplot2)
library(dplyr)
TableCount=NULL
pthDir = "D:\\NFS_DB\\2024_138_OPP"
listSubdir=list.files(pthDir,full.names = T)
CheckDifFin=NULL
for (i in 1:length(listSubdir)) {
    date1= basename(listSubdir[i])
	date1=substr(date1,1,15)
    CheckDiffPth=paste0(listSubdir[i],"\\Predict\\Check_differenceNFSAdult_",date1,".csv")
    CheckDif=read.csv(CheckDiffPth)
	CheckDifFin=rbind(CheckDif,CheckDifFin)
	}

unique(CheckDifFin$age)
CheckDifFin$Rookery=""
CheckDifFin$Rookery[CheckDifFin$age=="AN"]="Haulout"
CheckDifFin$Rookery[CheckDifFin$age=="Bch"]="Haulout"
CheckDifFin$Rookery[CheckDifFin$age=="TF"]="Rookey"
CheckDifFin$Rookery[CheckDifFin$age=="F"]="Rookey"

#daysErr=c("20240703_084344_s","20240703_152135_s","20240708_161228_t","20240709_092002_tf","20240711_084405_st","20240713_122840_r")



stat= CheckDifFin %>%
      group_by(date1)%>%
	  summarise(AutoCount = sum(AutoCount),ObserverCount = sum(ObserverCount))
	  
	  
  stat$diffInd =  stat$AutoCount - stat$ObserverCount
  stat$diffPerc	= stat$diffInd /stat$ObserverCount * 100




	#stat1=stat[stat$Rookery=="Rookey",]  
	  
	ggplot(stat, aes(x=date1, y=diffPerc)) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
    theme(axis.text.x= element_text(angle=90,hjust=1))+
    xlab("Date")+
    ylab("Difference (%)")+
    ggtitle("Count on model sites")
###########	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  summarise(med=median(diffPerc,na.rm=T),
	              min=min(diffPerc,na.rm=T),
				  max=max(diffPerc,na.rm=T),
	            Q0025 = quantile(diffPerc,0.025,na.rm=T),
				Q0975 = quantile(diffPerc,0.975,na.rm=T))


 AgeError= CheckDifFin %>% filter(age==AGE)
 #AgeError= AgeError %>% filter(abs(diffInd) > 10 )
 AgeError=as_tibble(AgeError)
										


for (i in 1:length(AgeError$diffPerc)) {
  if (AgeError$diffPerc[i] <0 ) { AgeError$DiffPercentMinus[i]=  0- AgeError$diffPerc[i]
  AgeError$fill[i]=  "Uder"               
  } else  { AgeError$DiffPercentMinus[i]= AgeError$diffPerc[i]
  AgeError$fill[i]=  "Over"   }
}
		
				

ggplot(AgeError, aes(x=date1, y=diffPerc ,fill=fill)) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
    theme(axis.text.x= element_text(angle=90,hjust=1))+
    xlab("Date")+
    ylab("Difference (%)")+
    ggtitle("Count on model sites")

######################################################

="C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\GR_2024\\PRB_analis\\BlobTableOrig.csv"
PredTblPth="C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\GR_2024\\PRB_analis\\BlobTablePred.csv"


OrigTbl=read.csv(OrigTblPth)
PredTbl=read.csv(PredTblPth)

OrigTbl$bsname=basename(OrigTbl$img_pth)
PredTbl$bsname=basename(PredTbl$img_pth)

PredTbl$bsname=gsub("Pred_","",PredTbl$bsname)
fin=left_join(PredTbl,OrigTbl, by="bsname")

fin$img_pth.x=NULL
fin$bsname = NULL
names(fin)=c("PredCount","OrigCount", "pth")

fin$difIndiv=fin$PredCount- fin$OrigCount
fin$percentdiff= abs(fin$difIndiv/fin$OrigCount*100)

plot(fin$percentdiff~fin$difIndiv)


head(fin)

length(fin$percentdiff)

length(fin$percentdiff[fin$percentdiff>20])
























oldCode=function(){
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
library(rgdal)
library(ggplot2)

SummaryTable=NULL
pthFolders="D:\\NFS_DB\\2024_138_OPP"
listFolder=list.files(pthFolders,full.names=T)


for (i in 1:length(listFolder)) {

date1=basename(listFolder[i])

DirModelCount=paste0(pthFolders,"\\",date1,"\\Observer_count")
 PthModelCount=list.files(DirModelCount, pattern="kml|shp",full.names=T)[1]
if (file.exists(PthModelCount)==T) { 
 ModelPoligonDir=  paste0(pthFolders,"\\",date1,"\\Polygons\\Model")        
 ModelPoligon=list.files(ModelPoligonDir, , pattern="kml|shp",full.names=T)[1]
 PredictPoint=paste0(pthFolders,"\\",date1,"\\Predict\\NFSAdult_",date1,".kml")
  save_pth_check_diff=paste0(pthFolders,"\\",date1,"\\Predict\\Check_difference", date1, ".csv")

ModelPoligon1 = readOGR(ModelPoligon)
ObserverPoint1= readOGR(PthModelCount)
PredictPoint1=readOGR(PredictPoint)

proj4string(ModelPoligon1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"		
proj4string(ObserverPoint1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
proj4string(PredictPoint1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	


ObserverPoint2 <- point.in.poly(ObserverPoint1,ModelPoligon1)
ObserverPoint3=as.data.frame(ObserverPoint2)
ObserverPoint4=ObserverPoint3[is.na(ObserverPoint3[,3]) == F,]
ObserverPoint5=data.frame(type="Observer",age=ObserverPoint4$LAYER,lat=ObserverPoint4$coords.x2,lon=ObserverPoint4$coords.x1)  

PredictPoint2 <- point.in.poly(PredictPoint1,ModelPoligon1)
PredictPoint3=as.data.frame(PredictPoint2)
PredictPoint4=PredictPoint3[is.na(PredictPoint3$Name.y) == F,]
PredictPoint5=data.frame(type="Predict",age=PredictPoint4$Description.x,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  

ObserverPoint6=data.frame(ObserverPoint5 %>% group_by(age,type) %>% summarise(n()))
allObserverRow=data.frame(age="all",type="Observer",n..=sum(ObserverPoint6$ n..))
PredictPoint6=data.frame(PredictPoint5 %>% group_by(age,type) %>% summarise(n()))
allPredictRow=data.frame(age="all",type="Predict",n..=sum(PredictPoint6$ n..))

resultDiff=rbind(ObserverPoint6,PredictPoint6,allObserverRow,allPredictRow)
resultDiff=data.frame(resultDiff)
resultDiff$date=date1

SummaryTable=rbind(SummaryTable,resultDiff)
}}
#########################################################################################
unique(SummaryTable$age)
SummaryTable$age=gsub("Sa","Bch",SummaryTable$age)
SummaryTable$age=gsub("J","Bch",SummaryTable$age)
SummaryTable$age=gsub("TN","An",SummaryTable$age)
SummaryTable$age=gsub("AN","An",SummaryTable$age)
SummaryTable= data.frame(SummaryTable %>%  group_by(type,age,date) %>% summarise(sum=sum(n..)))

SummaryTable$link=paste0(SummaryTable$date,"_", SummaryTable$age)


observerTable=data.frame(SummaryTable[SummaryTable$type=="Observer",])
predictTable= data.frame(SummaryTable[SummaryTable$type=="Predict",])

ResDiff=left_join(predictTable,observerTable,by="link")

ResDiff$diffAnimals= ResDiff$n...x-ResDiff$n...y
ResDiff$diffPercent=   abs(ResDiff$diffAnimals)/ResDiff$n...y*100
#################################################################################################################
################################################################################################################

############################################################################################GRAFF

#################################################################		   
for (i in 1:length(ResDiff$n...y)) {
  
  if (ResDiff$diffAnimals[i] <0 ) {# ResDiff$DiffPercentMinus[i]=  0- ResDiff$diffPercent[i]
                                    ResDiff$fill[i]=  "Uder"               
  } else  { #ResDiff$DiffPercentMinus[i]= ResDiff$diffPercent[i]
                                    ResDiff$fill[i]=  "Over"   }
  
}
###############################################################################################################
unique(ResDiff$age.y)
AgeError=ResDiff[ResDiff$age.y=="F",]

AgeError= AgeError %>%
        #  select(date.x,sum.x,sum.y) %>%
		  group_by(date.x) %>%
		  summarise(PredictCount=sum(n...x),
		            ObsercerCount=sum(n...y),
		            diff1=ObsercerCount-PredictCount
					)
										

AgeError$Diffpercent= abs(AgeError$diff1)/AgeError$ObsercerCount*100

for (i in 1:length(AgeError$date.x)) {
  if (AgeError$diff1[i] <0 ) { AgeError$DiffPercentMinus[i]=  0- AgeError$Diffpercent[i]
  AgeError$fill[i]=  "Uder"               
  } else  { AgeError$DiffPercentMinus[i]= AgeError$Diffpercent[i]
  AgeError$fill[i]=  "Over"   }
}
		

#TableCount=AgeError[AgeError$date.x %in% c("20190615","20190707","20190801"),]		
#AgeError=AgeError[AgeError$date.x !="20190727",]					
					

ggplot(AgeError, aes(x=date.x, y=DiffPercentMinus ,fill=fill)) +
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

}


