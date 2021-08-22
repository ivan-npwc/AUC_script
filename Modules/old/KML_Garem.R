library(spatialEco)
#################################
sex="Garem"
BtchStep=10000
Num_Clasts=2
ydim=5
xdim=5
pth_polygon=Rookery_polygon
######################################################################################
  date=basename(labelInput)
  kmlPathSave=paste0(labelInput,"\\Predict\\",sex,"_PointsPredict_", date, ".kml")   
  pathTablePoints=paste0(labelInput, "\\Predict\\", sex, "_BlobTable_GEO_",date, ".csv")
  dat<<-read.csv(pathTablePoints)
  if (length(dat$lon) < 30 ) {ydim=2;xdim=2}
  step1<<-  round(length(dat[,1])/BtchStep)
  if (step1*BtchStep < length(dat[,1])) {step1=step1+1}
  ####################
    for (i in 1: step1) {
      if (length(dat[,1]) <10000) {
        batch=dat
        
      }  else {
        
        if (i==1) {
          start=1
          stop=start+10000			  
        } 
        if (i>1 & i != step1) {
          start=stop
          stop=start+10000
        } 
        if (i==step1) {
          start=stop
          stop= length(dat[,1])
        }
        batch=dat[start:stop,]
      }
      x <- batch$lon  
      y <- batch$lat     
      xy <- SpatialPointsDataFrame(
        matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
        proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      mdist <- distm(xy)
      hc <- hclust(as.dist(mdist), method="complete")
      batch$clust <- cutree(hc, h=0.3)   # define the distance threshold, in this case 0.30 m DISTANCE BEETWIN ANIMAL POINTS
      dat1= batch %>%
        # select(m.majoraxis,m.eccentricity,m.theta,s.area,s.perimeter,s.radius.mean,s.radius.sd,s.radius.min,s.radius.max,lat,lon,clust)%>%
        group_by(clust) %>%
        summarise(#m.majoraxis=mean(m.majoraxis),
          #m.eccentricity=mean(m.eccentricity),
          #m.theta=mean(m.theta),
          s.area=mean(s.area),
          #s.perimeter=mean(s.perimeter),
          #s.radius.mean=mean(s.radius.mean),
          #s.radius.sd=mean(s.radius.sd),
          # s.radius.min=mean(s.radius.min),
          s.radius.max=mean(s.radius.max),
          lat=first(lat),
          lon=first(lon),
          count=length(clust)) %>%
        filter(count>0)          
      dat2=data.frame(dat1)
      if (i==1) {
        dat2.01=dat2
      } else {
        dat2.01=rbind(dat2.01,dat2)
      }}
  ##############################################################################################
  data_train <- dat2.01[,1:5]
  data_train_matrix <- as.matrix(scale(data_train))
  som_grid <- somgrid(xdim = xdim, ydim=ydim, topo="hexagonal")
  som_model <- som(data_train_matrix,
                   grid=som_grid, rlen=300, alpha=c(0.05,0.01),
                   keep.data = TRUE)
  #plot(som_model, type = "changes")
  ####################
  mydata <- as.matrix(som_model$codes[[1]])
  som_cluster <- cutree(hclust(dist(mydata)), Num_Clasts) # 4 clasters
  cluster_assignment <- som_cluster[som_model$unit.classif] # vector clasters
  dat2.01$age <- cluster_assignment
  dat3=data.frame(lat=dat2.01$lon,lon=dat2.01$lat, age=dat2.01$age)   # reverse coordinate !!!
  ###############################################################################################
    coords <- data.frame(lat= dat3$lat, lon=dat3$lon)   
    data   <- data.frame(age= dat3$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
  #####
   polygon_poly=shapefile(pth_polygon)
   proj4string(polygon_poly) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"					 
    Img0 <- point.in.poly(Points,polygon_poly) 
    Img=as.data.frame(Img0)
    Img2=Img[is.na(Img[,2]) == FALSE,]
    Img3=data.frame(age=Img2$age,lat=Img2$coords.x2,lon=Img2$coords.x1)  # reverse coordinate !!!
 ############################################
 	if (file.exists(Exlude_polygon)==T) {
	Img3=NULL
	coords <- data.frame(lat= Img2$coords.x1, lon=Img2$coords.x2)   
    data   <- data.frame(age= Img2$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
    polygon_exlude_polygon=shapefile(Exlude_polygon)
    proj4string(polygon_exlude_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"					 
    PointsExlude <- point.in.poly(Points,polygon_exlude_polygon)
	
	
	 PointsExlude1=as.data.frame(PointsExlude)
    PointsExlude2=PointsExlude1[is.na(PointsExlude1[,2]) == T,]
	Img3=data.frame(age=PointsExlude2$age,lat=PointsExlude2$coords.x2,lon=PointsExlude2$coords.x1)  
	
 }
  #################################################################################### KML WRITE
  sink(kmlPathSave)
  cat(paste0("<?xml version='1.0' encoding='UTF-8'?>
             <kml xmlns='http://www.opengis.net/kml/2.2'>
             <Document>
             <Style id='default'>
             <LineStyle>
             <color>7F0000FF</color>
             <width>3</width>
             </LineStyle>
             <PolyStyle>
             <color>990000FF</color>
             <fill>false</fill>
             </PolyStyle>
             </Style>
             <Folder>
             <name>Point Features</name>
             <description>Point Features</description>"))
   
   for (i in 1:length(Img3[,1])) {
     if (i==1) {
       a=paste0("<Placemark>
                <description>",
                Img3$age[i],
                "</description>
                <Point>
                <coordinates>",
                Img3$lon[i], ",",
                Img3$lat[i],
                "</coordinates>
                </Point>
                <styleUrl>#default</styleUrl>
                </Placemark>")
     } else {
       b=paste0("<Placemark>
                <description>",
                Img3$age[i],
                "</description>
                <Point>
                <coordinates>",
                Img3$lon[i], ",",
                Img3$lat[i],
                "</coordinates>
                </Point>
                <styleUrl>#default</styleUrl>
                </Placemark>") 
       a=paste0(a,b)
     }}
   cat(a)
   
   cat(paste0("  
              </Folder>
              </Document>
              </kml>"))
   sink()