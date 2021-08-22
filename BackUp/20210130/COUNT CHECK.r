 
SQLite_path="C:\\Users\\Admin\\Desktop\\NFS20202019201820172016_SSL2020201920182017_proccesing.db"
PolSitePTH="C:\\Users\\Admin\\Desktop\\138\\138.shp"


library(dplyr)
library(tidyr) 
library(RSQLite)
#####################################################################DELETE MODEL POLYGON IN PHOTO_COUNT TABLE

photo_count=dbReadTable(SSL,"photo_count")	
dbSendQuery(SSL, "delete from photo_count;")
photo_count=photo_count[photo_count$animal_type != "MP",]
dbWriteTable(SSL,"photo_count", photo_count,   append =T)
 ############################################################################CREATE TABLE MODEL Polygon
 dirOPP="H:\\SSL_DB\\2020_138_OPP"
 site="138"
 species="EJ"
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

################################## 
sqlite    <- dbDriver("SQLite")
SSL <- dbConnect(sqlite, SQLite_path)

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

write.csv(SeasonTable,"SeasonInfoTable_138_2020.csv",row.names=F)
SeasonTable=read.csv("SeasonInfoTable_138_2020.csv")

photo_count_files=dbReadTable(SSL,"photo_count_files")
photo_count_files$Img=basename(photo_count_files$file_name)


photo_count_files1$NameOPP=NULL
photo_count_files1$file_name=photo_count_files1$Img
photo_count_files1$Img=NULL

dbSendQuery(SSL,"drop table photo_count_files;")
dbSendQuery(SSL,"CREATE TABLE photo_count_files (r_year INT, site INT, r_date DATE, time_start  TIME, observer TEXT, comment TEXT, datecreated TEXT, file_name TEXT, latitude DECIMAL, longitude DECIMAL);")
dbWriteTable(SSL, "photo_count_files", photo_count_files1, append=T)
length(photo_count_files1$r_year[is.na(photo_count_files1$longitude)==F])
#############################################################################################################################
###########################################################################################################
########################################################################
library(dplyr)
library(tidyr) 
library(RSQLite)

SQLite_path="C:\\Users\\Admin\\Desktop\\NFS20202019201820172016_SSL2020201920182017_proccesing.db"




  
sqlite    <- dbDriver("SQLite")
SSL <- dbConnect(sqlite, SQLite_path)
photo_count=dbReadTable(SSL,"photo_count")	
photo_count_files=dbReadTable(SSL,"photo_count_files")
photo_count_list=dbReadTable(SSL,"photo_count_list")
local_site_polygons=dbReadTable(SSL,"polygons_local_sites")	 
polygons_model_sites=dbReadTable(SSL,"polygons_model_sites")	 
##########################################################################
photo_count_list=photo_count_list[photo_count_list$r_year=="2020" & photo_count_list$species=="EJ",]
photo_count_list=photo_count_list[photo_count_list$type_count=="Manual_model_count",]
##########################################################################
   for (i in 1:length(photo_count_list)) {
  pcl= photo_count_list[i,]
  type_count=pcl$type_count
  observer=   pcl$observer
  time_start=   pcl$time_start
  r_date  =pcl$r_date
  site  =pcl$site
  r_year=  pcl$r_year
  species =pcl$species 
  observer=  pcl$observer
  #####
photo_countSort=photo_count[photo_count$species == species & photo_count$r_date== r_date & photo_count$time_start  == time_start &  photo_count$observer==observer,]
pcfSort=photo_count_files[photo_count_files$r_date == r_date & photo_count_files$time_start %in% photo_countSort$time_start,]
local_site_polygons=local_site_polygons[local_site_polygons$site==site,]
local_site_polygons=local_site_polygons[order(local_site_polygons$order_point),]
PRJ=local_site_polygons$projection[1]
 #####################################################MODEL POLYGONS

################################################################## SITE POLYGONS
 coordinates(local_site_polygons) <- ~ latitude+longitude
    srPolygons=list()
	ListPol=unique(local_site_polygons$id)
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(local_site_polygons[local_site_polygons$id==ListPol[s],])),paste0(ListPol[s]))}
SpP=SpatialPolygons(srPolygons);proj4string(SpP) <-PRJ
plot(SpP)
################################################################################  COUNT
PHcoords <- data.frame(lat=photo_countSort$iLeft,lon= photo_countSort$iTop)   
PHdata   <- data.frame(animal_type= photo_countSort$animal_type)   #
	  PH <- SpatialPointsDataFrame(coords = PHcoords,
                                        data = PHdata, 
                                        proj4string = CRS(PRJ))
										#######
PHFcoords <- data.frame(lat=pcfSort$longitude,lon= pcfSort$latitude)  #  IMAGE    # INVERT COORD !! ERROR IN COORD PHOTO_COUNT_LIST
PHFdata   <- data.frame(file_name= pcfSort$file_name)   #
	  PHF <- SpatialPointsDataFrame(coords = PHFcoords,
                                        data = PHFdata, 
                                        proj4string = CRS(PRJ))

#PH   photo count
#PHF  photo count list
#SpP  site
#      model polygons 
#################################################################LOOP FRO SITES TO COUNT seals and images IN
for (y in 1:length(SpP)) {
ImgsOverSite=    PHF   %over%    SpP[y] 
ImgsOverSite1=length(ImgsOverSites[is.na(ImgsOverSites)==F])
Site=names(SpP[y])
########
index= PH  %over%   SpP[y] 
index1=data.frame(index)
index2=row.names(index1)[is.na(index1$index)==F]
CountOverSite1=PH[index2]
CountOverSite2=data.frame(CountOverSite1)
CountOverSite3=CountOverSite2 %>%
        dplyr::    select(animal_type) %>%    #  MAY BE ERROR IF IN THE SITE ONLY FEW ANIMAL CATEGORT PRESENCE 
          	group_by(animal_type) %>%
            summarise(n=n())  %>%			
            spread(animal_type,n)
CountOverSite3$local_site=Site
}
#####################################################################################  MODEL COUNT
if(type_count =="Manual_model_count") {
PMSsort=polygons_model_sites[polygons_model_sites$species==species &    polygons_model_sites$r_date==r_date & polygons_model_sites$time_start==time_start,]
 coordinates(PMSsort) <- ~ latitude+longitude
    srPolygons=list()
	ListPol=unique(PMSsort$id)
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(PMSsort[PMSsort$id==ListPol[s],])),paste0(ListPol[s]))}
ModepPol=SpatialPolygons(srPolygons);proj4string(ModepPol) <-PRJ
plot(ModepPol)
}
######
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
	CountOverSite3$model_site=MODEL_site
    CountOverSite3$time_start=time_start
    CountOverSite3$r_date=r_date
    CountOverSite3$species=species
    DayModelCount=rbind(CountOverSite3,DayModelCount)
	} else {
	Empty=data.frame(model_site=MODEL_site,time_start=time_start,r_date =r_date,species=species)
	Empty1=rbind(Empty,Empty1)
	
	}
 }
 
 
 DayModelCount1=DayModelCount %>% 
   dplyr::    select( animal_type, model_site, time_start, r_date, species) %>%
          	group_by(animal_type, model_site, time_start, r_date, species) %>%
            summarise(n=n()) %>%			
            spread(animal_type,n)
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			