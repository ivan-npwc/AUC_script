
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
####################################################
  date=basename(labelInput)
  #############################################################
  pth_Points=paste0(labelInput,"\\Observer count\\", date, ".shp") 
  pth_Table_ref=paste0(labelInput,"\\", date,"_table.csv")
  pth_save_Tbl=paste0(labelInput,"\\Predict\\", date,"Points.csv")
  Table_ref=read.csv(pth_Table_ref)
  Points <-shapefile(pth_Points) 
  ############################################################
    NSdif50=(Table_ref$north-Table_ref$south)/2 
    Table_ref$north50=Table_ref$north+NSdif50  
    SNdif50=(Table_ref$north-Table_ref$south)/2
    Table_ref$south50=Table_ref$south-SNdif50 
    WEdiff50= (Table_ref$east-Table_ref$west)/2
    Table_ref$west50=Table_ref$west-WEdiff50 
    EWdiff50= (Table_ref$east-Table_ref$west)/2 
    Table_ref$east50=Table_ref$east+EWdiff50 
  ####################################################################################################
  table2= data.frame(link=Table_ref$link,west=Table_ref$west, east=Table_ref$east,south=Table_ref$south, north=Table_ref$north,  north50=  Table_ref$north50, south50= Table_ref$south50,west50= Table_ref$west50, east50= Table_ref$east50,date=Table_ref$date)
  ################################################################################################################################################
  cl <- makePSOCKcluster(4) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)					  
  })
  registerDoParallel(cl) 
  TableAnimalPoints <- foreach(y = 1:length(table2[,1]), .combine=rbind) %dopar% {	 
    selectRow=table2[y,]
    Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
    proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    ModelIN <- point.in.poly(Points,Poligon) # 
    MaskPoint <-  as.data.frame(ModelIN)
    imagePoint=MaskPoint[is.na(MaskPoint[,3]) == FALSE,]  
    if (length(imagePoint[,1]) != 0) {                
     imagePoint1=data.frame(sex=  imagePoint$LAYER, lat= imagePoint$coords.x1, lon= imagePoint$coords.x2)					
      PointTable2=cbind(imagePoint1,selectRow)
    }
  }
  ##################################
  stopCluster(cl)
  write.csv(TableAnimalPoints, pth_save_Tbl)
  
