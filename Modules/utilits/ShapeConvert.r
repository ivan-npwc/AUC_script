source("Modules/KMLwrite_function.r")
library(sp)
from= "D:\\US_Walrus\\Agisoft\\2018_WLR_OPP\\20180907_02\\Observer count\\20180907_02.shp"
kmlPathSave="D:\\US_Walrus\\Agisoft\\2018_WLR_OPP\\20180907_02\\Observer count\\Chenged.kml"


walrus=rgdal::readOGR(from)
walrus_=spTransform(walrus,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
walrus=data.frame(walrus)


 dat3 <<- data.frame(lat=walrus$coords.x2,   lon=walrus$coords.x1 ,  age="U")
 
 
 KMLwrite(Img3=dat3,kmlPathSave=kmlPathSave)
 
