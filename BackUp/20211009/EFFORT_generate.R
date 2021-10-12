library(tools)
library(
labelInput
System_data
Species
site
SQLite_path


DirLocSites=paste0(System_data,"\\polygons_local_sites\\",site);pthLcSt=list.files(DirLocSites,full.names=T,pattern = "kml|shp")
locSite=readOGR(pthLcSt)
  sqlite    <- dbDriver("SQLite")
  SSL <- dbConnect(sqlite,   SQLite_path)
################################################################################ EFFORT COPY TO DATA FOLDER

#######################################
lcst=data.frame(locSite)[,2]

sitedayEffort=NULL
for (i in 1:length(locSite)) {
  for (y in 1:length(listOPP)) {
    site=lcst[i]
      OPP=listOPP[y]
	  r_date1=paste0(substr(OPP,0,4),"-",substr(OPP,5,6),"-",substr(OPP,7,8))
	  time_start1=paste0(substr(OPP,10,11),":",substr(OPP,12,13),":",substr(OPP,14,15))
	
	sitedayEffort1=data.frame(
	               id="",
	               r_year=2021,
	               site=30,
	               r_date=r_date1,
	               time_start=time_start1,
	               type="manual_count_full",
	               local_site=site,
	               creator="UIA2010",
	               comments="Auto effort generated",
	               rain="UNKR",
               	  distance="L100",
	              splash="UNKS",
	              quality="UNKQ",
	              species="SSL",
	              count_type="Count",
				 datecreated= "2021-10-08 19:50:53 UIA2010")
				  
	sitedayEffort=rbind(sitedayEffort1,sitedayEffort)          
}
}

sitedayEffort$id=c(1:length(sitedayEffort$id))
  dbWriteTable(SSL, "count_effort", sitedayEffort, append=T)



















