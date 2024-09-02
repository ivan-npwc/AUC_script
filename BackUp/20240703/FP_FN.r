#https://stackoverflow.com/questions/73325202/create-circular-areas-around-a-coordinate

# Библиотеки
library(sf)
library(tidyverse)
library(rgdal)

# Входные данные
    crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    labelInput # ="H:\\2024_138_OPP\\20240622_085946"
    date1 <- substr(basename(labelInput),1,15)
    Species <- "NFSAdult"
    savePth <-paste0(labelInput,"\\",basename(labelInput),"_FP-FN.csv")
	
	ModelPoligonDir=paste0(labelInput,"\\Polygons\\Model");ModelPoligonPTH=list.files(ModelPoligonDir, full.names=T, ,pattern="shp|kml")[1]
	
    ObserverPointP=list.files(paste0(labelInput,"\\Observer_count"), full.names=T, pattern="kml|shp")[1]
				if (is.na(ObserverPointP)==T){ ObserverPointP=list.files(paste0(labelInput,"\\Polygons\\All_layers"), full.names=T, pattern="kml|shp")[1]}
				predPth = paste0(labelInput,"\\Predict\\",Species,"_",date1,".kml")

# Чтение данных
obs <- readOGR(ObserverPointP)
prd <- readOGR(predPth)
MdlPol <-readOGR(ModelPoligonPTH)

 proj4string(obs) <- crs
 proj4string(prd) <- crs
 proj4string(MdlPol) <- crs
 
 obs =spTransform(obs,CRS(crs))
 prd =spTransform(prd,CRS(crs))
 MdlPol =spTransform(MdlPol,CRS(crs))
 
  prd = prd[!is.na(over(prd,as(MdlPol,"SpatialPolygons"))),]
  obs = obs[!is.na(over(obs,as(MdlPol,"SpatialPolygons"))),]
# Преобразование в sf
OBS_CORD <- st_as_sf(obs)
PRED_CORD <- st_as_sf(prd)



# Создание полигона вокруг точек ручного учёта
OBSER_POL <- st_buffer(x = OBS_CORD, dist=0.35)
# Нахождение  точек авто учёта внутри полигонов ручного учета
OVERLAPPING_POINTS <- st_intersection(PRED_CORD, OBSER_POL)

# Расчет TP, FN и FP
TP_obs <- length(OVERLAPPING_POINTS$Name)
obs <- length(obs)
prd <- length(prd)
FN <- obs - TP_obs; FN=-FN
#FP <- prd - TP

########################
#Создание полигона вокруг точек авто учёта
AU_POL <- st_buffer(x = PRED_CORD, dist=0.35)
# Нахождение  точек ручного учёта внутри полигонов авто учета
OVERLAPPING_POINTS_AU <- st_intersection(OBS_CORD, AU_POL)
#подсчет числа полигонов AU с точками ручного учёта
TP_au <- length(OVERLAPPING_POINTS_AU$Name)
#подсчет числа полигонов AU без точек ручного учёта
FP_au = prd - TP_au



# Сохранение результатов
report <- as.tibble(data.frame(OPP=labelInput,TP_obs=TP_obs,FP_au=FP_au,FN=FN, obs=obs,prd=prd))
print(report)
write.csv(report,savePth)



#plot(st_geometry(OBSER_POL))
#st_write(OBSER_POL,"x.kml")