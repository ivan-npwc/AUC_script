 
 SQLiteWriteFunction= function(

  #############################################   photo_count
  site,
  r_year,
  r_date,
  time_start,
  observer,
  animal_type,
  iLeft,
  iTop,
  file_nameOPP,
  GPSAltitude,
  ########################################   photo_count_list
  type_count,
  species,
 # visibility,
 # CommentsPCL,
 # rain,
 # distance,
 # type,
 # quality,
 # splash,
  #######################################   photo_count_files
  latitude,
  longitude,
  altitude,
  CommentsPCF,     
  file_name,
  SQLite_path1=SQLite_path,
  local_site
  )  {
   library(dplyr)
    library(tidyr)
  sqlite    <- dbDriver("SQLite")
  SSL <- dbConnect(sqlite,   SQLite_path1)
  ########################################### photo_count
#  local_site="No_info"
  datecreated <<- format(Sys.time(), " %X %Y %m %d")
  GPSLongitude=iLeft
  GPSLatitude=iTop
  ###############################################	  photo_count_list
  photo_count<<-data.frame(r_year,site,r_date,time_start,observer,local_site,animal_type,iLeft,iTop,
                          datecreated,file_name=file_nameOPP,species=species,GPSLongitude=GPSLongitude,
						  GPSLatitude=GPSLatitude,GPSAltitude=GPSAltitude,creator=observer,type="Acount")
				  
  photo_count_list <<- data.frame(r_year,site, r_date,time_start,observer,comments="data from R",
                            type="Acount",datecreated,species,type_count)
							 
						 
 # count_effort<<- data.frame(r_year,site,r_date, time_start, type="Acount",local_site,creator=observer, comments=CommentsPCL, # NOW WE USE EFFORT ONLY FROM DB
 #                             rain,distance, splash,quality, species,count_type = type_count,datecreated)
 							 
  photo_count_files<<-data.frame(r_year,site,r_date,time_start,observer,comment=CommentsPCF,datecreated,file_name,latitude,longitude,altitude)
  
  ################################################################
  dbWriteTable(SSL, "photo_count_files", photo_count_files, append=T)
  dbWriteTable(SSL, "photo_count_list", photo_count_list, append=T)
  dbWriteTable(SSL, "photo_count", photo_count, append=T)
 # dbWriteTable(SSL, "count_effort", count_effort, append=T)
  
  
  ############
#if (dbExistsTable(SSL,"photo_count_pivot")==F) {
#query=paste0("CREATE TABLE photo_count_pivot (species TEXT, r_year  INT, site INT, r_date  DATE, time_start TIME, observer TEXT, local_site TEXT,TN INT,TF INT,An INT,Sa INT,F INT,J INT,P INT,U INT, D INT, DP INT, Tr INT, Bch INT,
#PRIMARY KEY (r_year, site, r_date, time_start, species,observer));")
#dbExecute(SSL,query)
#}
#   photo_count0<<-photo_count[photo_count$animal_type != "MP",]
#	photo_count1<<-photo_count0 %>%
#	             select(r_year,site,r_date,time_start,observer,local_site,species,animal_type)%>%
#	             group_by(r_year,site,r_date,time_start,observer,local_site,species,animal_type)%>%
#				 summarize(count=n())		 
#	photo_count_r<<-photo_count1 %>% spread(animal_type,count)	
#	dbWriteTable(SSL, "photo_count_pivot",photo_count_r , append=T)
#
  
#}
dbDisconnect(SSL)
 }
 