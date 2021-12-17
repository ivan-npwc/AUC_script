Delete=function (SQLite_path1=SQLite_path,
                   Species1=SpeciesG,
                   Site1=SiteG,
                   Year1=YearG) {
  
  Species1<<-SpeciesG
  Site1<<-SiteG
  Year1<<-YearG				  
  
  library(RSQLite)				   
 # library(sparklyr)
  library(dplyr)
  library(tidyr)
  library(writexl)
  
  print("Delete started")
  Sys.setlocale("LC_ALL","Russian_Russia.20866")
  sqlite    <- dbDriver("SQLite")
  SSL <- dbConnect(sqlite,   SQLite_path1)
  #  sc <- spark_connect(master = "local")	
  
  photo_count <<- dbExecute(SSL, paste0("DELETE    FROM  photo_count;"))
  photo_count_list <<- dbExecute(SSL, paste0("DELETE   FROM  photo_count_list;"))
  photo_count_files <<- dbExecute(SSL, paste0("DELETE   FROM  photo_count_files;"))
  photo_count_pivot <<- dbExecute(SSL, paste0("DELETE   FROM  photo_count_pivot;"))
  print("Delete finished")
}

Delete()