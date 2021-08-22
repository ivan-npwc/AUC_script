library(spatialEco)


if (listOPP1 == "All") {listOPP2=listOPP} else {listOPP2=listOPP1}
    nchaBName=nchar(basename(pathPredict))+1
    pthOPP<<-substr(pathPredict,0, nchar(pathPredict)-nchaBName)
listOPP2=paste0(pthOPP,"\\",listOPP2)


source("Modules/KMLwrite_function.r")
for (k in 1:length(listOPP2)) {
  OPP= basename(listOPP2[k])
  Points1=OPP_PointsTable[OPP_PointsTable$date1==OPP,]
  Effort1=Effort[Effort$FoldName==OPP,]
  Start1=  format(as.POSIXct(strptime(Effort1$Start, "%Y:%m:%d %H:%M:%S")),"%Y%m%d_%H%M%S")
  Img3=data.frame(age=Points1$label,lon=Points1$Lat,lat=Points1$Lon)
  kmlPathSave=paste0(listOPP2[k],"\\",Start1,".kml")
  KMLwrite(Img3,kmlPathSave)
}