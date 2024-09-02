  library(RSQLite)
  library(dplyr)
  library(rgdal)
  library(tools)
  library(tidyr)
  
  
#  source("Modules/COMPLETENESS.r")
  
   labelInput
   Species
   Count_type
   site
   SQLite_path
   System_data
  
###############################################################################
    CheckSubFold1=dir.exists(paste0(labelInput,"\\Predict"));CheckSubFold2=dir.exists(paste0(labelInput,"\\Observer_count"))
    if (CheckSubFold1==F & CheckSubFold2==F) {SubFold=T} else {SubFold=F} 
      if(SubFold==T) {listSubfold=list.dirs(labelInput,full.names=T,recursive=F)}
      if(SubFold==F) {listSubfold=labelInput} 
  #################################################### FULL COUNT collect data in sub fold   
    photo_count2=NULL
    photo_count_list2=NULL
    photo_count_files2=NULL
   for (e in 1:length (listSubfold)) {
      labelinput1<<-listSubfold[e]
	   source("Modules\\PredsPrepareToSQL.R")#
	photo_count2=rbind(photo_count2,photo_count1)
    photo_count_list2=rbind(photo_count_list2,photo_count_list1)
    photo_count_files2=rbind(photo_count_files2,photo_count_files1)
     } 
photo_count_list3 <<-	photo_count_list2 %>%
                    group_by(across())%>%
                    summarize(n=n())
photo_count_list3$n=NULL	

  sqlite    <- dbDriver("SQLite")
  SSL <- dbConnect(sqlite,   SQLite_path)
############################################################  SQLITE QUERY 

#ALTER TABLE photo_count_files ADD COLUMN latitude TEXT; 
#ALTER TABLE photo_count_files ADD COLUMN longitude TEXT; 
#ALTER TABLE photo_count_files ADD COLUMN altitude TEXT;

#ALTER TABLE photo_count ADD COLUMN creator TEXT;
#ALTER TABLE photo_count ADD COLUMN type TEXT;
#CREATE TABLE polygons_model_sites (
#id          TEXT,
#order_point TEXT,
#latitude    TEXT,
#longitude   TEXT,
#site        TEXT,
#species     TEXT,  
#r_date      TEXT,
#time_start  TEXT,
#projection  TEXT,
#PRIMARY KEY (id, order_point, latitude, longitude, site, species,r_date,time_start));

  ################################################################
  dbWriteTable(SSL, "photo_count_files", photo_count_files2, append=T)
  dbWriteTable(SSL, "photo_count_list", photo_count_list3, append=T)
  dbWriteTable(SSL, "photo_count", photo_count2, append=T)
 ##################################################################################
################################################################################### COMPLETENESS
# where = paste0("WHERE ",
#                   "type= ","'", Count_type,"'",
#		      " AND site= ","'", site,"'",
#		      " AND species= ","'", SpeciesSQL,"'",
#			  " AND r_date= ","'", r_date1,"'",
#		      " AND time_start= ","'", time_start,"'")                                                											 
#  cf=as_tibble(dbGetQuery(SSL,paste0("select * from count_effort ",where)))
# 
# 
#  if (exists("completenes") & length(cf$local_site)>0) {
#  for (i in 1:length(cf$local_site)){
#  lcst1=cf$local_site[i]
#  cf$completeness[cf$local_site==lcst1]= round(completenes$coverage[completenes$local_site==lcst1])
# }
# dbSendQuery(SSL,paste0("DELETE FROM count_effort ",where)) 
 #dbWriteTable(SSL, "count_effort", cf, append=T)
 # }
 #######################################################################
dbDisconnect(SSL)
 
   
  

