


pth=   "C:\\Users\\Admin\\Desktop\\138_2016-2020 NFS SSL.db"
library(dplyr)
  library(RSQLite)
  sqlite    <- dbDriver("SQLite")
  SSL <- dbConnect(sqlite,   pth)
 

photo_count=dbGetQuery(SSL,"select * from  photo_count;")
photo_count_list=dbGetQuery(SSL,"select * from  photo_count_list;")
photo_count_files=dbGetQuery(SSL,"select * from  photo_count_files;")

count=dbGetQuery(SSL,"select * from  count;")

 photo_count$species[photo_count$species=="EJ"]="SSL"
 
 photo_count$local_site[photo_count$local_site=="No information"]="U"
 photo_count$local_site[photo_count$local_site=="No_info"]="U"
 
 photo_count$animal_type[photo_count$animal_type=="Sa"]="SA"
 photo_count$animal_type[photo_count$animal_type=="An"]="AN"
 photo_count$animal_type[photo_count$animal_type=="Tr"]="TR"
  ###############################################################
photo_count_list$species[photo_count_list$species=="EJ"]="SSL"
  photo_count_list$visibility="L100"
  photo_count_list$distance="L100"
  photo_count_list$rain= "Nrain"


photo_count_list$quality[photo_count_list$quality=="Not bad Count"]="NBquality"
photo_count_list$quality[photo_count_list$quality=="Not  bad Count"]="NBquality"

photo_count_list$quality[photo_count_list$quality=="Best Count"] ="Bquality"
photo_count_list$quality[photo_count_list$quality=="Terrible Count"]="Tquality"  
  
  photo_count_list$splash[photo_count_list$splash=="Heavy splash"]="Hsplash"
  photo_count_list$splash[photo_count_list$splash=="Moderate splash"]="Msplash"

 
  ###################################################################
  photo_count_list$type[photo_count_list$type== "Access_Full_Count"]="Acount"
 
 photo_count_list$type[photo_count_list$type== "Agisoft_Ful_count"]="opp_manual_full_count"
 photo_count_list$type[photo_count_list$type== "Agisoft_Full_Count"]="opp_manual_full_count"
 photo_count_list$type[photo_count_list$type== "Manual_full_count"]="opp_manual_full_count"
 photo_count_list$type[photo_count_list$type== "Agisoft"]="opp_manual_full_count"
 

photo_count_list$type[photo_count_list$type== "Agisoft_Model_count"]="opp_manual_model_count"
photo_count_list$type[photo_count_list$type== "Manual_model_count"]="opp_manual_model_count"


photo_count_list$type[photo_count_list$type== "Agisoft_Auto_count"]="opp_auto_count"
photo_count_list$type[photo_count_list$type== "Auto_Ful_count"]="opp_auto_count"
photo_count_list$type[photo_count_list$type== "Auto_count"]="opp_auto_count"
 
photo_count_list$type_count[photo_count_list$type_count== "Manual_full_coun"]="opp_manual_full_count"
photo_count_list$type_count[photo_count_list$type_count== "Manual_full_count"]="opp_manual_full_count"
 photo_count_list$type_count[photo_count_list$type_count== "Agisoft_Full_Count"]="opp_manual_full_count"
 
 photo_count_list$type_count[photo_count_list$type_count== "Manual_model_count"]="opp_manual_model_count"
 photo_count_list$type_count[photo_count_list$type_count== "Agisoft_manual_count"]="opp_manual_model_count"
 
 photo_count_list$type_count[photo_count_list$type_count== "Auto_count"]="opp_auto_count"
 photo_count_list$type_count[photo_count_list$type_count== "Access_Full_Count"]="Acount"
 
 photo_count_list$type_count=photo_count_list$type
 
 #################################################################
 
  count$coverage=count$comments
 
  count$model_version[count$species=="SSL" & count$type_count=="opp_auto_count" & count$r_year=="2020"]= "SSLAdult86val20200707#Val_0.85_epoch_03_AgeSSL#SSL_Pup_20200420_val0"
  count$model_version[count$species=="CU" & count$type_count=="opp_auto_count" & count$r_year=="2020"]="NFSAdult86val20200709"
  count$model_version[count$species=="CU" & count$type_count=="opp_auto_count" &count$r_year=="2019"]="NoInfo"
 dbSendQuery(SSL, "delete from count;")
 dbWriteTable(SSL, "count", count, append=T)
########################################################################
dbSendQuery(SSL, "delete from photo_count;")
dbSendQuery(SSL, "delete from photo_count_list;")
dbSendQuery(SSL, "delete from photo_count_files;")



  dbWriteTable(SSL, "photo_count_files", photo_count_files, append=T)
  dbWriteTable(SSL, "photo_count_list", photo_count_list, append=T)
  dbWriteTable(SSL, "photo_count", photo_count, append=T)
  

