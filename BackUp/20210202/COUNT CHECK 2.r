#################################################################################
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
#photo_count_list=photo_count_list[photo_count_list$r_year=="2020" & photo_count_list$species=="SSL",]#& photo_count_list$r_date== "2020-06-21",]
#model_version="SSLAdult86val20200707#Val_0.85_epoch_03_AgeSSL#SSL_Pup_20200420_val0"
##########################################################################
#########################################################################################
##########################################################################################
##########################################################################################
lspSort=NULL
SeasonFullCount=NULL
SeasonModelCount=NULL
   for (i in 1:length(photo_count_list[,1])) {
   SpP=NULL
   PH=NULL
   PHF=NULL
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
SeasonFullCount=rbind(SeasonFullCount,CountOverSite3)
}
#####################################################################################  MODEL COUNT
if(type_count =="opp_manual_model_count") {
PMSsort=NULL
ModepPol=NULL
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
 
	
	
	SeasonModelCount=rbind(SeasonModelCount,DayModelCount1)		
	}
#####################################################				
	}	
##################################################
################################################
	
countSum=rbind(SeasonModelCount,SeasonFullCount)	### SUM MODEL COUNT AND FULL COUNT 
			
countSum1=countSum %>%
        dplyr::    select(animal_type, local_site, comments,r_date,time_start,observer,species,type_count) %>%    
         	group_by(animal_type, local_site, comments,r_date,time_start,observer,species,type_count) %>%
            summarise(n=n())  %>%			
            spread(animal_type,n)		
			
	countSum1$Empty=NULL		
			cs=countSum1
#################################
if (dbExistsTable(SSL,"count")==F) {
query=paste0("CREATE TABLE count (type_count TEXT,r_year INT, site INT, r_date DATE, time_start TIME, observer TEXT, local_site TEXT, 
                                                                                                     TN INT, 
                                                                                                     TF INT,
																									   AN INT,
																									   SA INT,
																									   F INT, 
																									   J INT, 
																									   P INT,
																									   U INT,
																									   D INT, 
																									   DP INT, 
																									   Tr INT,
																									   species TEXT,
																									   datecreated TEXT, 
																									   comments TEXT,
PRIMARY KEY (r_year, site, r_date, time_start, observer, local_site,species), 
FOREIGN KEY (r_year, site, r_date, time_start, observer, species) 
REFERENCES photo_count_list (r_year, site, r_date, time_start, observer, species));")
dbExecute(SSL,query)}	
#######################################################
count=data.frame(type_count= cs$type_count,
                 r_year= substr(cs$r_date,1,4),
                 site= "138",
                 r_date =  cs$r_date,
				 time_start  =cs$time_start,
				 observer = cs$observer,
				 local_site =cs$local_site,
TN =cs$TN,
TF =cs$TF,
AN =cs$AN,
SA =cs$SA,
F =cs$F,
J =cs$J,
P =cs$P,
U =cs$U,
D =cs$D,
DP =cs$DP,
Tr =cs$Tr,	
           species	=cs$species,
           datecreated=	 format(Sys.time(), " %X %Y %m %d"),
           comments= cs$comments	   
)
dbWriteTable(SSL,"count",count,append=T)		   