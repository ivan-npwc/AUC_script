	 library(RSQLite)
     library(dplyr)

	
	DBto= "E:\\Google Drive (ivan@npwc.us)\\DATA\\SSL\\SSL_2017_Access_new.db"
    photo_countP= "C:\\Users\\Admin\\Documents\\photo_count.txt"
    type_count="Access_Full_Count"
    checkCategory=c("J",  "Sa", "An", "U",  "TF", "P",  "Tr", "TN", "DP", "D",  "F")
	
    Sys.setlocale("LC_ALL","Russian_Russia.20866")
    sqlite    <- dbDriver("SQLite")
 
    ToDB<- dbConnect(sqlite,   DBto)
	
    photo_count <<- read.csv(photo_countP,sep=";")
  #  photo_count_list <<- dbGetQuery(FromDB, paste0("SELECT  *  FROM  photo_count_list;"))
  #  photo_count_files <<- dbGetQuery(FromDB, paste0("SELECT  *  FROM  photo_count_files;"))
		
	
	photo_count$file_name=gsub("G","D",photo_count$file_name)
	photo_count$species="EJ"
	photo_count$label= NULL
	photo_count$site="138" 
	datecreated=Sys.time()
	photo_count$animal_type=gsub("AmnoT","An",photo_count$animal_type)
	photo_count$animal_type=gsub("AMnoT","An",photo_count$animal_type)
	photo_count$animal_type=gsub("Om","An",photo_count$animal_type)
	
	photo_count$animal_type=gsub("Tf","TF",photo_count$animal_type)
	photo_count$animal_type=gsub("SA","Sa",photo_count$animal_type)
	photo_count$animal_type=gsub("Tn","TN",photo_count$animal_type)
	
	photo_count$animal_type=gsub("AF","F",photo_count$animal_type)
	
	
	photo_count$animal_type=gsub("UNK","U",photo_count$animal_type)
	photo_count$animal_type=gsub("Unk","U",photo_count$animal_type)
	
	photo_count$animal_type=gsub("Traum","Tr",photo_count$animal_type)
	photo_count$animal_type=gsub("Travm","Tr",photo_count$animal_type)
	PresencCategory=unique(photo_count$animal_type) 
	
	ErrorCateg=PresencCategory[!(PresencCategory %in% checkCategory)]
	if (length(ErrorCateg)>0) {stop(ErrorCateg)} else {
	
	
	photo_count=photo_count %>% 
	  group_by(observer,r_year, site, r_date, time_start,iLeft,iTop,file_name,species,animal_type) %>%
	  summarise(dubl=n())
    photo_count$dubl=NULL

	photo_count$GPSLongitude= photo_count$iLeft
	photo_count$GPSLatitude=photo_count$iTop
	photo_count$GPSAltitude="25"
	photo_count$local_site="No information"
	photo_count$datecreated= datecreated
	
	###################NEW PHOTO_COUNT_FILES
	linkPthoto_count=	photo_count %>%
	                 #  select(r_date,site,r_year,species,time_start,observer,file_name) %>%
					   group_by(r_date,site,r_year,species,time_start,observer,file_name) %>%
					   summarise(count=n())
##### 
	photo_count_files=data.frame (
	r_year=linkPthoto_count$r_year,
	site=linkPthoto_count$site,
	r_date= linkPthoto_count$r_date,
	time_start=linkPthoto_count$time_start,
	observer=linkPthoto_count$observer,
	comment="Corrected 20200207",
	datecreated=datecreated,
	file_name=linkPthoto_count$file_name,
	type_count=type_count
	)
########################################################NEW PHOTO_COUNT_LIST
linkPthoto_countLISt=	photo_count %>%
	                 #  select(r_date,site,r_year,species,time_start,observer,file_name) %>%
					   group_by(r_date,site,r_year,species,time_start,observer) %>%
					   summarise(count=n())

photo_count_list   = data.frame(
	r_year=linkPthoto_countLISt$r_year,
	site=linkPthoto_countLISt$site,
	r_date= linkPthoto_countLISt$r_date,
	time_start= linkPthoto_countLISt$time_start,
	observer=linkPthoto_countLISt$observer,
	comments="Corrected 20200330",
	visibility="More then 50",
	rain= "No",
	distance="25",
	type=type_count,
	quality="Not  bad Count",
	splash="Moderate splash",
	datecreated=datecreated,
	species= linkPthoto_countLISt$species,
	type_count= type_count          # вид учета состоит из двух - это на отдельных снимках или агисофт. Так же модельный и полный
	)

					   
	
	
###############################################	  photo_count_list
  photo_count<<-data.frame(r_year=photo_count$r_year,
                           site=photo_count$site,
                           r_date=photo_count$r_date,
                           time_start=photo_count$time_start,
                           observer=photo_count$observer,
                           local_site=photo_count$local_site,
                           animal_type=photo_count$animal_type,
                           iLeft=photo_count$iLeft,
                            iTop=photo_count$iTop,
                          datecreated=photo_count$datecreated,
						  file_name=photo_count$file_name,
						  species=photo_count$species,
						  GPSLongitude=photo_count$GPSLongitude,
						  GPSLatitude=photo_count$GPSLatitude,
						  GPSAltitude=photo_count$GPSAltitude)
						  
  photo_count_list <<- data.frame(r_year=photo_count_list$r_year,
                                  site=photo_count_list$site, 
								  r_date=photo_count_list$r_date,
								  time_start=photo_count_list$time_start,
								  observer=photo_count_list$observer,
								  comments=photo_count_list$comments,
                                  visibility=photo_count_list$visibility,
								  rain=photo_count_list$rain,
								  distance=photo_count_list$distance,
								  type=photo_count_list$type,
								  quality=photo_count_list$quality,
								  splash=photo_count_list$splash,
								  datecreated=photo_count_list$datecreated,
								  species=photo_count_list$species,
								  type_count=photo_count_list$type_count)
								  
  photo_count_files<<-data.frame(r_year=photo_count_files$r_year,
                                  site=photo_count_files$site,
								  r_date=photo_count_files$r_date,
								  time_start=photo_count_files$time_start,
								  observer=photo_count_files$observer,
								  comment=photo_count_files$comment,
								  datecreated=photo_count_files$datecreated,
								  file_name=photo_count_files$file_name)
  ################################################################	
	
	
	
  dbWriteTable(ToDB, "photo_count_files", photo_count_files, append=T)
  dbWriteTable(ToDB, "photo_count_list", photo_count_list, append=T)
  dbWriteTable(ToDB, "photo_count", photo_count, append=T)	
  
	dbDisconnect(ToDB)
	
	
	}
	
	
	
	