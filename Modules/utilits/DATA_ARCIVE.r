siteDirFrom= "A:\\"
To="F:\\SSL"

listSite=list.files(siteDirFrom, full.names=T)

for (i in 2:length(listSite)){
site=listSite[i]
NmSite=basename(site)
ToSite=paste0(To,"\\",NmSite)
dir.create(ToSite)

listOPPSite=list.files(site, full.names=T)

  for (u in 1: length(listOPPSite)) {
  oppdir=listOPPSite[u]
  NmOpp=basename(oppdir)
  date1=substr(NmOpp,1,15)
  TuOPP=paste0(ToSite,"\\",NmOpp)
  dir.create(TuOPP)
  
  
  kml=list.files(oppdir,full.names=T, pattern="kmz")
  polygons=	paste0(oppdir,"\\Polygons")
  observerCount= 	paste0(oppdir,"\\Observer_count")
   if (dir.exists(observerCount)==F){
  
  ObserverCountDirOLD=paste0(oppdir,"\\Observer count")

listFiles=list.files(ObserverCountDirOLD, full.names=T)
dir.create(observerCount)
file.copy(listFiles,observerCount)
 
  }
  
  predict1= paste0(oppdir,"\\Predict\\SSLAdult_",date1,".kml")
  
  
  file.copy(kml,TuOPP)
  file.copy(polygons,TuOPP, recursive=T)
  file.copy(observerCount,TuOPP,recursive=T)
  file.copy(predict1,TuOPP)

  
  
  
  
  
  
  
  }
  }
