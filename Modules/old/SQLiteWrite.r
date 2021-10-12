 
 SQLiteWriteFunction= function(
  #############################################   photo_count
  site,
  r_year,
  r_date,
  time_start,
  creator,
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
  datecreated <<- paste0(format(Sys.time(), " %Y-%m-%d %H:%M:%S")," ", observer)
  
  GPSLongitude=iLeft
  GPSLatitude=iTop
  ###############################################
  photo_count<<-data.frame(r_year,site,r_date,time_start,observer=creator,local_site,animal_type,iLeft,iTop,
                          datecreated,file_name=file_nameOPP,species=species,GPSLongitude=GPSLongitude,
						  GPSLatitude=GPSLatitude,GPSAltitude=GPSAltitude,creator=creator,type=type_count)
	




	
 photo_count_list <<- data.frame(r_year,site, r_date,time_start,observer,comments="data from R",
                            type=type_count,datecreated,species,type_count="opp_count")
							 
						 

 							 
  photo_count_files<<-data.frame(r_year,site,r_date,time_start,observer,comment=CommentsPCF,datecreated,file_name,latitude,longitude,altitude)
  
  ################################################################
  dbWriteTable(SSL, "photo_count_files", photo_count_files, append=T)
  dbWriteTable(SSL, "photo_count_list", photo_count_list, append=T)
  dbWriteTable(SSL, "photo_count", photo_count, append=T)
 ##################################################################################
################################################################################### COMPLETENESS
 where = paste0("WHERE ",
                   "type= ","'", type_count,"'",
		      " AND site= ","'", site,"'",
		      " AND species= ","'", species,"'",
			  " AND r_date= ","'", r_date,"'",
		      " AND time_start= ","'", time_start,"'")                                                											 
  cf=as_tibble(dbGetQuery(SSL,paste0("select * from count_effort ",where)))
 
 
  if (exists("completenes") & length(cf$local_site)>0) {
  for (i in 1:length(cf$local_site)){
  lcst1=cf$local_site[i]
  cf$completeness[cf$local_site==lcst1]= round(completenes$coverage[completenes$local_site==lcst1])
 }
 dbSendQuery(SSL,paste0("DELETE FROM count_effort ",where)) 
 dbWriteTable(SSL, "count_effort", cf, append=T)
  }
 #######################################################################
dbDisconnect(SSL)
 }
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 