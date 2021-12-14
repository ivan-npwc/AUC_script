###########################################################################################################################
library(sp)
library(spatialEco)
library(raster)
library(dplyr)
library(geosphere)
#########################################################################################################
 date1=basename(labelInput)
  date= date1 #substr(date1,1,nchar(date1)-4)
pthTable=paste0(labelInput,"\\",date, "Points.csv")
pthTableNew=paste0(labelInput,"\\",date, "_CountDist.csv")
table1= read.csv(pthTable)
table2<<-data.frame(sex=table1$sex, lat=table1$lat,lon=table1$lon,link=table1$link, west=table1$west,east=table1$east,south=table1$south,north=table1$north, north50=table1$north50,south50=table1$south50,west50=table1$west50,east50=table1$east50,date=table1$date)
imgList=unique(table2$link)
####################################################################################################################
for (i in 1:length(imgList))  {
  options(warn=-1)
  img=paste0(imgList[i])
  listAnimals = table2[table2$link== img,] 
  #table2$F1024=0
  #table2$F512=0
  Limit512=unique(data.frame(west=listAnimals$west,east=listAnimals$east,south=listAnimals$south,north=listAnimals$north))
  Poligon512 <- as(raster::extent(Limit512$west,Limit512$east,Limit512$south,Limit512$north), "SpatialPolygons")
  proj4string(Poligon512) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  ###########################################################################################
  Limit1024=unique(data.frame(west50=listAnimals$west50,east50=listAnimals$east50,south50=listAnimals$south50,north50=listAnimals$north50))
  Poligon1024 <- as(raster::extent(Limit1024$west50,Limit1024$east50,Limit1024$south50,Limit1024$north50), "SpatialPolygons")
  proj4string(Poligon1024) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  ###########################################################################################
  coords <- data.frame(lat= listAnimals$lat, lon=listAnimals$lon)   # coordinates
  data   <- data.frame(sex= listAnimals$sex)   # data
  crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") # proj4string of coords
  Points <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)
  ###############################################################  FILTER POINTS FOR THE IMAGE

  
  ModelIN <- point.in.poly(Points,Poligon512) 
 
  MaskPoint=as.data.frame(ModelIN)
  imagePoint=MaskPoint[is.na(MaskPoint[,2]) == FALSE,]
  imagePoint=data.frame(sex=imagePoint$sex,lat=imagePoint$coords.x1,lon=imagePoint$coords.x2)
  SexList=as.vector(unique(imagePoint$sex))
  if (length(SexList) > 0 ) {
   table2$All512[table2$link==imgList[i]]=length(imagePoint$sex)
  #  CountPointF= imagePoint %>% 
  #    filter(sex== "F")%>%
  #    group_by(sex) %>%
  #    summarise(count=n())
  #  CountPointF=data.frame(CountPointF)
  #  if (length(CountPointF$sex) != 0) {
  #    table2$F512[table2$link==imgList[i]]=  as.numeric(CountPointF$count)
  #  }
  }
  #######################################################################################

  ModelIN <- point.in.poly(Points,Poligon1024) 
 
  MaskPoint=as.data.frame(ModelIN)
  imagePoint=MaskPoint[is.na(MaskPoint[,2]) == FALSE,]
  imagePoint=data.frame(sex=imagePoint$sex,lat=imagePoint$coords.x1,lon=imagePoint$coords.x2)
  SexList=as.vector(unique(imagePoint$sex))
  if (length(SexList) > 0 ) {
    table2$All1024[table2$link==imgList[i]]=length(imagePoint$sex)
    #  CountPointF= imagePoint %>% 
    #  filter(sex== "F")%>%
    #  group_by(sex) %>%
    #  summarise(count=n())
    #  CountPointF=data.frame(CountPointF)
    #  if (length(CountPointF$sex) != 0) {
    #  table2$F1024[table2$link==imgList[i]]=  as.numeric(CountPointF$count)
  #  }
    }
  ###################################################################################
  mdist <- distm(Points)
  minMdist = min(mdist[mdist !=0])   
  table2$MinDist[table2$link==imgList[i]]= minMdist
  #############################################
}
table3=data.frame(date=table2$date,  link=table2$link,All1024=table2$All1024,All512=table2$All512, MinMdist=table2$MinDist) #F512=table2$F512, F1024=table2$F1024
table4=unique(table3)
write.csv(table4, pthTableNew)
options(warn=0)
