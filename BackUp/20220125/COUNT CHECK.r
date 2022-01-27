 library(dplyr)
library(tidyr) 
library(RSQLite)
library(raster)
SQLite_path="C:\\Users\\Admin\\Desktop\\NFS20202019201820172016_SSL2020201920182017_proccesing.db"
sqlite    <- dbDriver("SQLite")
SSL <- dbConnect(sqlite, SQLite_path)


PolSitePTH="C:\\Users\\Admin\\Desktop\\138\\138.shp"


#####################################################
CREATE TABLE photo_count (r_year  INT, site INT, r_date  DATE, time_start TIME, observer TEXT, local_site TEXT, animal_type TEXT, iLeft INT, iTop INT, datecreated  TEXT DEFAULT CURRENT_TIMESTAMP, file_name TEXT, species  TEXT, GPSLatitude  TEXT, GPSLongitude TEXT, GPSAltitude  TEXT, 
PRIMARY KEY (r_year, site, r_date, time_start, iLeft, iTop, file_name, species,observer), 
FOREIGN KEY (r_year, site, r_date, time_start)REFERENCES photo_count_list (r_year, site, r_date, time_start) ON DELETE CASCADE ON UPDATE CASCADE);
##########
CREATE TABLE photo_count_list (r_year  INT, site    INT, r_date  DATE, time_start TIME, observer TEXT, comments TEXT, visibility  TEXT, rain  TEXT, distance  TEXT, type  TEXT, quality TEXT, splash  TEXT, datecreated TEXT DEFAULT CURRENT_TIMESTAMP, species  TEXT, type_count  TEXT, 
PRIMARY KEY (r_year, site, r_date, time_start, species, type_count, observer));
###
CREATE TABLE photo_count_files (r_year INT, site INT, r_date DATE, time_start  TIME, observer TEXT, comment TEXT, datecreated TEXT, file_name TEXT, latitude DECIMAL, longitude DECIMAL,altitude TEXT);

#####################################################################DELETE MODEL POLYGON IN PHOTO_COUNT TABLE

#photo_count=dbReadTable(SSL,"photo_count")	
#dbSendQuery(SSL, "delete from photo_count;")
#photo_count=photo_count[photo_count$animal_type != "MP",]
#dbWriteTable(SSL,"photo_count", photo_count,   append =T)
 ############################################################################CREATE TABLE MODEL Polygon
 dirOPP= "E:\\NFS_2020\\2020_138_OPP\\Pup"
 site="138"
 species="CU"
 listOPP=list.files(dirOPP,full.names=T)
 polygons_model_sites=NULL
 for (i in 1:length(listOPP)) {
 MdlPolPth=paste0(listOPP[i],"\\Polygons\\Model\\Model.shp")
  if (file.exists(MdlPolPth)){
  
 rd= substr(basename(listOPP[i]),1,8)
 ts1=substr(basename(listOPP[i]),10,15)
 r_date=paste0(substr(rd,1,4),"-",substr(rd,5,6),"-",substr(rd,7,8))
 time_start=paste0(substr(ts1,1,2),":",substr(ts1,3,4))
 
             ModelPol=shapefile(MdlPolPth)
		     projection= proj4string(ModelPol)
             NSubPol=length(ModelPol@polygons)
             DF=NULL
			 
         for (i in 1:NSubPol) {
           SubPol1=ModelPol@polygons[i]
           Coords=SubPol1[[1]]@Polygons[[1]]@coords
		   
      SubDF=data.frame(id=i,order_point=c(1:length(Coords[,1])), latitude=Coords[,1],longitude=Coords[,2])
             SubDF$site=site
			 SubDF$species=species 
			 SubDF$r_date=r_date
			 SubDF$time_start=time_start
             DF<-rbind(SubDF,DF)		   
}
polygons_model_sites=rbind(polygons_model_sites,DF)
}

}
polygons_model_sites$projection=projection



for (i in 1:length(polygons_model_sites$time_start)) {
check=substr(polygons_model_sites$time_start[i],0,1)
if (check=="0") {
polygons_model_sites$time_start[i]=substr(polygons_model_sites$time_start[i],2,5)
}
}

dbSendQuery(SSL,"delete from polygons_model_sites;")

if (dbExistsTable(SSL,"polygons_model_sites")==F) {
query=paste0("CREATE TABLE polygons_model_sites (id TEXT, 
                                                 order_point INT,
												 latitude DECIMAL,
												 longitude  DECIMAL, 
												 site  TEXT,
												 species   TEXT,
												 r_date  TEXT,
												 time_start   TEXT,
												 projection TEXT,
												 PRIMARY KEY (site, r_date, time_start, species,latitude,longitude,id,order_point));")
dbExecute(SSL,query)}

dbWriteTable(SSL, "polygons_model_sites", polygons_model_sites, append=T)

#####################################################################################################LOCAL SITE POLYGON 
if (dbExistsTable(SSL,"polygons_local_sites")==F) {
query=paste0("CREATE TABLE polygons_local_sites (site  INT, id  TEXT, latitude   DECIMAL, longitude  DECIMAL, order_point   INT, projection TEXT, 
FOREIGN KEY (site, id) REFERENCES id_local_site (site, id) ON DELETE CASCADE ON UPDATE CASCADE);")
dbExecute(SSL,query)}

         ModelPol=shapefile(PolSitePTH)
		projection= proj4string(ModelPol)
		 listLocalSite=ModelPol$LAYER
	 DF=NULL	 
		  for (i in 1:length(listLocalSite)) {
		localSite= listLocalSite[i]
		SubPol1=ModelPol[ModelPol$LAYER==localSite,]@polygons
		Coords=SubPol1[[1]]@Polygons[[1]]@coords
		 
		 SubDF=data.frame( 
		  site="138",
		  id=localSite,
		  latitude=Coords[,1],
		  longitude=Coords[,2],
		 order_point =c(1:length(Coords[,1])),
		  projection=projection)
		     DF<<-rbind(SubDF,DF)
			 
		  }	 
		  dbSendQuery(SSL,"delete from polygons_local_sites;")
		 dbWriteTable(SSL, "polygons_local_sites", DF, append=T)
#########################################################################
library(dplyr)
library(tidyr) 
SeasonTable=NULL
oppDir="H:\\SSL_DB\\2020_138_OPP"
listOPP=list.files(oppDir, full.names=T)


for (i in 1 :length(listOPP)) {
  NameOPP=listOPP[i]
  InfoTablePTH=list.files(NameOPP,pattern="Agisoft",full.names=T)[1]
 InfoTable1=read.csv(InfoTablePTH)
 InfoTable2=InfoTable1[InfoTable1$Property %in% c("Exif/GPSLongitude","Exif/GPSLatitude"),]
 

 listImgs=unique(InfoTable2$IMGbasename)
 DayTable=NULL
   for (y in 1:length(listImgs)){
   imgN=listImgs[y]
   tbl=InfoTable2[InfoTable2$IMGbasename==imgN,]
   lat=tbl$Value[tbl$Property =='Exif/GPSLatitude'][1]
   lon=tbl$Value[tbl$Property =='Exif/GPSLongitude'][1]
   preTb=data.frame(Img=imgN,latitude=lat,longitude=lon)
   DayTable=rbind(preTb,DayTable)
   }
   
   DayTable$NameOPP=basename(NameOPP)
 SeasonTable=rbind(DayTable,SeasonTable)
}

#write.csv(SeasonTable,"SeasonInfoTable_138_2020.csv",row.names=F)
#SeasonTable=read.csv("SeasonInfoTable_138_2020.csv")



photo_count_files1$NameOPP=NULL
photo_count_files1$file_name=photo_count_files1$Img
photo_count_files1$Img=NULL

dbSendQuery(SSL,"delete from photo_count_files;")
#dbSendQuery(SSL,"CREATE TABLE photo_count_files (r_year INT, site INT, r_date DATE, time_start  TIME, observer TEXT, comment TEXT, datecreated TEXT, file_name TEXT, latitude DECIMAL, longitude DECIMAL);")
dbWriteTable(SSL, "photo_count_files", photo_count_files1, append=T)
length(photo_count_files1$r_year[is.na(photo_count_files1$longitude)==F])
#############################################################################################################################
###########################################################################################################
########################################################################
library(dplyr)
library(tidyr) 
library(RSQLite)
library(sp)



SQLite_path="C:\\Users\\Admin\\Desktop\\NFS20202019201820172016_SSL2020201920182017_proccesing.db"


  
sqlite    <- dbDriver("SQLite")
SSL <- dbConnect(sqlite, SQLite_path)
photo_count=dbReadTable(SSL,"photo_count")	
photo_count_files=dbReadTable(SSL,"photo_count_files")
photo_count_list=dbReadTable(SSL,"photo_count_list")
local_site_polygons=dbReadTable(SSL,"polygons_local_sites")	 
polygons_model_sites=dbReadTable(SSL,"polygons_model_sites")	 
##########################################################################
#photo_count_list=photo_count_list[photo_count_list$r_year=="2020" & photo_count_list$species=="EJ",]
model_version="SSLAdult86val20200707#Val_0.85_epoch_03_AgeSSL#SSL_Pup_20200420_val0"
#photo_count_list=photo_count_list[photo_count_list$type_count=="Manual_model_count",]
##########################################################################
#########################################################################################
TottalCountOverSite=NULL
SeasonModelCount=NULL
lspSort=NULL
   for (i in 1:length(photo_count_list[,1])) {
  pcl= photo_count_list[i,]
  type_count=pcl$type_count
  observer=   pcl$observer
  time_start=   pcl$time_start
  r_date  =pcl$r_date
  site  =pcl$site
  r_year=  pcl$r_year
  species =pcl$species 
  observer=  pcl$observer
  ###################################################FILER DATA ACCODIND INFO IN COUNT_LIST
photo_countSort=photo_count[photo_count$species == species & photo_count$r_date== r_date & photo_count$time_start  == time_start &  photo_count$observer==observer,]
pcfSort=photo_count_files[photo_count_files$r_date == r_date & photo_count_files$time_start %in% photo_countSort$time_start,]
lspSort=local_site_polygons[local_site_polygons$site==site,]
lspSort=lspSort[order(lspSort$order_point),]
PRJ=lspSort$projection[1]
##################################################################  CREATE SITE POLYGONS
 coordinates(lspSort) <- ~ latitude+longitude
    srPolygons=list()
	ListPol=unique(lspSort$id)
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(lspSort[lspSort$id==ListPol[s],])),paste0(ListPol[s]))}
SpP=SpatialPolygons(srPolygons);proj4string(SpP) <-PRJ
plot(SpP)
################################################################################  CREAYE COUNT SP POINTS
PHcoords <- data.frame(lat=photo_countSort$iLeft,lon= photo_countSort$iTop)   
PHdata   <- data.frame(animal_type= photo_countSort$animal_type)   #
	  PH <- SpatialPointsDataFrame(coords = PHcoords,
                                        data = PHdata, 
                                        proj4string = CRS(PRJ))
										#######
PHFcoords <- data.frame(lat=pcfSort$longitude,lon= pcfSort$latitude)  #  IMAGE SP POINTS   # INVERT COORD !! ERROR IN COORD PHOTO_COUNT_LIST
PHFdata   <- data.frame(file_name= pcfSort$file_name)   #
	  PHF <- SpatialPointsDataFrame(coords = PHFcoords,
                                        data = PHFdata, 
                                        proj4string = CRS(PRJ))

#PH   photo count
#PHF  photo count list
#SpP  site
#      model polygons 
#################################################################LOOP FRO SITES TO COUNT seals and images IN
if (type_count !="opp_manual_model_count") {
NoAnmInLocSite1=NULL
CountOverSite3=NULL
for (y in 1:length(SpP)) {             #####count images in the  local site
local_site=names(SpP[y])
 if (local_site != "All"){
ImgsOverSite =    PHF   %over%    SpP[y] 
ImgsOverSite1=length(ImgsOverSite[is.na(ImgsOverSite)==F])

comments=paste0(ImgsOverSite1," Images over ",local_site," local site")
#########################################################
index= PH  %over%   SpP[y] 
index1=data.frame(index)
index2=row.names(index1)[is.na(index1$index)==F]
 if (length(index2) !=0){

CountOverSite1=PH[index2,]
CountOverSite2=data.frame(CountOverSite1)
CountOverSite2$local_site=local_site
CountOverSite2$comments=comments
CountOverSite2$optional=NULL
 CountOverSite2$lat =NULL    
 CountOverSite2$lon =NULL
CountOverSite3=rbind(CountOverSite2,CountOverSite3)
} else {
NoAnmInLocSite=data.frame(animal_type="Empty",local_site =local_site,comments=comments)
CountOverSite3=rbind(CountOverSite3,NoAnmInLocSite)
}
}
}
################################### count animals outside any local site
index= PH  %over%   SpP
index1=data.frame(index)
index2=row.names(index1)[is.na(index1$index)==T]
 if (length(index2) !=0){

CountNoSite1=PH[index2,]
CountNoSite2=data.frame(CountNoSite1)
CountNoSite2$optional=NULL
 CountNoSite2$lat =NULL    
 CountNoSite2$lon =NULL  
CountNoSite2$local_site="U"
CountNoSite2$comments=""
CountOverSite3=rbind(CountOverSite3, CountNoSite2)
}
 ###@##########################
#CountOverSite3$Empty=NULL			
CountOverSite3$r_date=r_date
CountOverSite3$time_start=time_start
CountOverSite3$observer=observer
CountOverSite3$species=species		   
CountOverSite3$type_count=type_count
TottalCountOverSite=rbind(TottalCountOverSite,CountOverSite3)
}
#####################################################################################  MODEL COUNT
if(type_count =="opp_manual_model_count") {
PMSsort=polygons_model_sites[polygons_model_sites$species==species &    polygons_model_sites$r_date==r_date & polygons_model_sites$time_start==time_start,]
 coordinates(PMSsort) <- ~ latitude+longitude
    srPolygons=list()
	ListPol=unique(PMSsort$id)
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(PMSsort[PMSsort$id==ListPol[s],])),paste0(ListPol[s]))}
ModepPol=SpatialPolygons(srPolygons);proj4string(ModepPol) <-PRJ
plot(ModepPol)

##########count seals in model polygons
DayModelCount=NULL
Empty1=NULL
 for (y in 1:length(ModepPol)) {
 MODEL_site=names(ModepPol[y])
 index= PH  %over%   ModepPol[y] 
index1=data.frame(index)
index2=row.names(index1)[is.na(index1$index)==F]
    if(length(index2) !=0) {
	CountOverSite1=PH[index2,]
	CountOverSite3=data.frame(animal_type=CountOverSite1$animal_type)
	CountOverSite3$local_site=MODEL_site
	CountOverSite3$comments=""
	CountOverSite3$r_date=r_date
    CountOverSite3$time_start=time_start
	CountOverSite3$observer=observer
    CountOverSite3$species=species
	CountOverSite3$type_count=type_count		
	
	
	
    
    DayModelCount=rbind(CountOverSite3,DayModelCount)
	} else {
	Empty=data.frame(animal_type="Empty",local_site=MODEL_site, comments="", r_date =r_date,time_start=time_start,observer=observer ,species=species,type_count=type_count)
	Empty1=rbind(Empty,Empty1)
	}
 }
 DayModelCount1=rbind(DayModelCount,Empty1)  # rbind empty and non empty local site
 
# DayModelCount2=DayModelCount1 %>% 
#   dplyr::    select( animal_type, model_site, time_start, r_date, species) %>%
#          	group_by(animal_type, model_site, time_start, r_date, species) %>%
#            summarise(n=n()) %>%			
#            spread(animal_type,n)

			
#	DayModelCount2$Empty=NULL		
	
	SeasonModelCount=rbind(SeasonModelCount,DayModelCount1)		
	}
			
	}	
#####################	
	countSum=rbind(SeasonModelCount,TottalCountOverSite)		
			
countSum1=countSum %>%
        dplyr::    select(animal_type, local_site, comments,r_date,time_start,observer,species,type_count) %>%    
         	group_by(animal_type, local_site, comments,r_date,time_start,observer,species,type_count) %>%
            summarise(n=n())  %>%			
            spread(animal_type,n)		
			
	countSum1$Empty=NULL		
			
			
			