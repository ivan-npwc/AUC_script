Get_data=function (SQLite_path1=SQLite_path,
                   Species1=SpeciesG,
				   Site1=SiteG,
				   Year1=YearG) {
				   
                   Species1<<-SpeciesG
				   Site1<<-SiteG
				   Year1<<-YearG				  
				   
library(RSQLite)				   
#library(sparklyr)
library(dplyr)
library(tidyr)
library(writexl)

				   
    Sys.setlocale("LC_ALL","Russian_Russia.20866")
    sqlite    <- dbDriver("SQLite")
    SSL <- dbConnect(sqlite,   SQLite_path1)
  #  sc <- spark_connect(master = "local")	

    photo_count <<- dbGetQuery(SSL, paste0("SELECT  *  FROM  photo_count;"))
    photo_count_list <<- dbGetQuery(SSL, paste0("SELECT  *  FROM  photo_count_list;"))
    photo_count_files <<- dbGetQuery(SSL, paste0("SELECT  *  FROM  photo_count_files;"))
		
		
   # photo_count <- copy_to(sc, photo_count)			
   # photo_count_list <- copy_to(sc, photo_count_list)		
   # photo_count_files <- copy_to(sc, photo_count_files)
    
	if (is.null(Site1)==F) {photo_count=photo_count %>% filter(site %in% Site1)}
	if (is.null(Year1)==F) {photo_count=photo_count %>% filter(r_year %in% Year1)}
	if (is.null(Species1)==F & is.null(photo_count$species)==F) {photo_count=photo_count %>% filter(species %in% Species1)}
	
	
	                            photo_count_sc1=photo_count %>% 
	                                   select (r_date,observer,animal_type,time_start,species)  %>%
									   group_by(r_date,observer,animal_type,time_start,species) %>%
									   summarize(count=n())	
    if (is.null(Site1)==F) {photo_count_list=photo_count_list %>% filter(site %in% Site1)}
    if (is.null(Year1)==F) {photo_count_list=photo_count_list %>% filter(r_year %in% Year1)}
	if (is.null(Species1)==F) {photo_count_list=photo_count_list %>% filter(species %in% Species1)}
		
	
#	photo_count_files_sc1=photo_count_files %>% filter(r_year %in% Year1, site %in% Site1,species %in% Species1)
	#photo_countJoin=photo_count_sc1 %>% left_join(photo_count_list, 
	 #               by = c("r_date" = "r_date","observer"="observer","time_start"="time_start","species"="species"))
	photo_count1=data.frame(photo_count_sc1)
	photo_count_r=photo_count1 %>% spread(animal_type,count)	
	
	  
	  
	  
	    
  savePth=paste0("Output/Count_", Species1[1],Site1[1],Year1[1]   ,".xlsx")
  write_xlsx(x = list(photo_count=photo_count_r, photo_count_list=photo_count_list), path = savePth, col_names = T)	
  print(photo_count_r)
  print(paste0("Data also is located in ",savePth))
  }
	
	Get_data()