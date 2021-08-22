#' A function prdicting branded SSL presence on TLC image, and generats frame boxes for each potential finding.
#'
#' @param begin A file index begin with
#' @param files A vector of files to be evaluated
#' @param source_drive A drive that contained TLC map files (SSL DB organized file/folder structure)
#' @param destination_drive A drive that contained data files (SSL DB organized file/folder structure)
#' @param model_name A UNET model to be used for evaluation
#' @param skip_crop Obsolete
#' @param img_class A tile segmentation class. At the present only 256 px size supproted.
#' @param supress_messages Supress detailed progress messages
#' @param save_prediction_image Save image with predicted boxes (only use for model diagnostics)
#' @param multi_gpu number of GPU to be used, NULL - use first available. Default NULL
#' @param tf_batch_size a batch size i.e. number of tiles to be passed to GPU default is 32
#' @return A dataframe of predictions identifying branded animals presense on the image. Prediction data is stored as CSV files in Drive:/SSL_DB/...Data/Predictions
#' @examples

#' @export execution_function_tf
#' @importFrom foreach "%dopar%"
#' @importFrom foreach "%do%"
#' @import sp

#support functions


execution_function_tf=function(begin=1,files,source_drive,destination_drive,model_name,skip_crop=FALSE,img_class=256,supress_messages=TRUE,save_prediction_image=FALSE,multi_gpu=NULL,tf_batch_size=32L,...)
{
  poly1=NULL;overrides=NULL;treshlevels=c(0.99975,0.9975);treshhold=0.99975;save.mask=FALSE
  poly1=NULL;overrides=NULL;treshlevels=c(0.999975,0.975);treshhold=0.999975;save.mask=FALSE

  #load unet
  model=TLC:::load_unet(paste0(model_name,".h5"),multi_gpu=multi_gpu)

  if(TLC:::get_os()=="unix")
  {
    #cl <- parallel::makeForkCluster(parallel::detectCores(logical=FALSE)-1)
    doMC:::registerDoMC(parallel::detectCores(logical=FALSE)-1)
  }else if(TLC:::get_os()=="win"){
    cl <- parallel::makePSOCKcluster(parallel::detectCores(logical=FALSE)-1)
    parallel::clusterExport(cl=cl, list("img_class"),envir=environment())
    doParallel::registerDoParallel(cl)
  }




  for(f in begin:length(files)) #length(files_path)
  {
    t1=Sys.time()
    filename=basename(files[f])
    file_path=files[f]
    file_info=TLC::extract_data_from_path(file_path)

    site=file_info$site
    camId=file_info$camId

    croop_path=paste(destination_drive,"/SSL_DB/",substr(filename,1,4),"_", site,  "_MapCroop",sep="")
    save_path=paste(destination_drive,"/SSL_DB/",substr(filename,1,4),"_", site,  "_Prediction",sep="")

    data_path=paste(destination_drive,"/SSL_DB/",substr(filename,1,4),"_", site,  "_Data/Predictions",sep="")
    if(!dir.exists(data_path)) dir.create(data_path,recursive=TRUE)

    #if(file.exists(paste(data_path,"/prediction",".csv",sep="")))
    #{
    #  prediction.out=read.csv(paste(data_path,"/prediction",".csv",sep=""),stringsAsFactors=FALSE)
    #  file.copy(paste(data_path,"/prediction",".csv",sep=""), paste(data_path,"/prediction_bak",".csv",sep=""),overwrite=TRUE)
   # }else{
   #   prediction.out=NULL
   # }
    prediction.out=NULL

    img1=try(magick::image_read(file_path),silent=TRUE)
    if(!"try-error"%in%is(img1))
    {
      img1.=try(magick::as_EBImage(img1),silent=TRUE)
      if(!"try-error"%in%is(img1.))
      {

        #img1=magick::image_read(file_path)
        height=magick::image_info(img1)$height

        #poly1_full=return_camera_polygon(paste("site_",site, sep=""), paste("poly_", gsub("-","_", camId),"_",height,"_2013",sep=""))
        if(img_class==256) poly1_full=TLC::get_camera_tiles(site=paste("site_",site,sep=""),camera=camId) #,"_2014"

        #poly1_full=return_camera_polygon(site=paste("site_",0,sep=""),camera=paste("poly_",0,"_",height,sep=""))

        if(img_class==1024) stop("There is no realization for 1024 image resolution")
        if(is.null(poly1_full)) stop(paste0("No tile markup found:\n",files[f]))
        poly1=poly1_full
        #prediction starts here
        #t2=Sys.time()
        #
        #crplist=croop_all_images(poly1=poly1_full,image_path=file_path,folder_path=croop_path,res=img_class,returnlist=TRUE)
        #
        #t3=Sys.time()
        #
        path=dirname(file_path)
        filenames=basename(file_path)
        filename=substr(filenames,1,nchar(filenames)-4)

        if(!is.null(overrides)&"list"%in%is(overrides)) for(i in 1:length(overrides)) assign(names(overrides)[i],overrides[[i]])


        img.tmp=img1
        maxhv=magick::image_info(img.tmp)[[1,3]]
        maxw=magick::image_info(img.tmp)[[1,2]]

        t4=Sys.time()
        rows=unique(substr(names(poly1_full),1,1))
        tile_placment=list()
        for(k in 1:length(rows))
        {

          tile_placment[[k]]=names(poly1_full)[substr(names(poly1_full),1,1)==rows[k]]
          if(k==1) tile_list=tile_placment[[k]] else tile_list=c(tile_list,tile_placment[[k]])
        }

        #path_splited=paste(croop_path,"/Splited","/", substr(filename, 1, 8),"/",filename,"_",tile_list,".jpg",sep="")

        t5=Sys.time()
        #predict batch here
        ###################
        #parallel procedure extracted from here
        #######################################################################################

        if(!supress_messages)
        {
          message(paste("   Image processing started:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
          flush.console()
        }

        poly.=poly1
        crplist=list()
        nPolygons=length(poly.)
        res=img_class
        #img1 = image_read(image_file)


        crops <- foreach::foreach(k = 1:nPolygons) %do% {
          cc=poly.[k]@polygons[[1]]@Polygons[[1]]@coords
          dx=as.integer(max(cc[,1])-min(cc[,1]))
          dy=as.integer(max(cc[,2])-min(cc[,2]))
          x=as.integer(min(cc[,1]))
          y=as.integer(min(cc[,2]))
          file_path=file_path
          data.frame(file_path,y,x,dy,dx)
        }
        crops <- do.call(abind::abind, c(crops, list(along = 1)))
        #crops[,"x"]
        #crops=crops[349:350,]
        crops=data.frame(crops)
        crops$file_path=as.character(crops$file_path)
        crops$x=as.integer(as.numeric(as.character(crops$x)))
        crops$y=as.integer(as.numeric(as.character(crops$y)))
        crops$dx=as.integer(as.numeric(as.character(crops$dx)))
        crops$dy=as.integer(as.numeric(as.character(crops$dy)))

        data <- tibble::tibble(
          img=as.character(crops$file_path),
          x=as.integer(crops$x),
          y=as.integer(crops$y),
          dx=as.integer(crops$dx),
          dy=as.integer(crops$dy)
        )
        #data=data[1:2,]
        batch_size=tf_batch_size


        dataset <- data %>%
          tfdatasets::tensor_slices_dataset() %>%
          tfdatasets::dataset_map(function(dat)
          {
            img = tfdatasets::tf$image$decode_and_crop_jpeg(tfdatasets::tf$io$read_file(dat$img),channels=3,crop_window=tfdatasets::tf$convert_to_tensor((c(dat$y,dat$x,dat$dy,dat$dx))))
            img = tfdatasets::tf$image$convert_image_dtype(img, dtype = tfdatasets::tf$float32)
            img = tfdatasets::tf$image$resize(img, size = tfdatasets::shape(img_class, img_class))
          })

        dataset <- dataset %>%
          tfdatasets::dataset_batch(batch_size)

        dataset %>%
          tfdatasets::dataset_map(unname)

        #example <- dataset %>% as_iterator() %>% iter_next()
        #example[1,,,] %>% as.array() %>% as.raster() %>% plot()


        if(!supress_messages)
        {
          message(paste("   Prediction started:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
          flush.console()
        }
        ########################################################################



        #batch <- dataset %>% as_iterator() %>% iter_next()
        #predictions <- predict(model, dataset)
        prediction=keras:::predict.keras.engine.training.Model(model,dataset)
        #prediction=predictions
        #prediction=predict(model,batch)

        ######################################################################################################################################################################stopCluster(cl)
        if(!supress_messages)
        {
          message(paste("   Prediction ended:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
          flush.console()
        }
        t6=Sys.time()

        Sys.time()
        t7=Sys.time()

        columns_placment=sapply(tile_placment,function(X) length(X))
        #prediction=example
        strips.=foreach::foreach(i = length(tile_placment):1) %do%
          {
            i=i
            strips.l= foreach::foreach (l = columns_placment[i]:1) %do%
              {
                if(i==1) k.ind=l else k.ind=sum(columns_placment[1:(i-1)])+l
                t(prediction[k.ind,,,])
              }
            tmp1. <- do.call(abind::abind, c(strips.l, list(along = 1))) #[length(strips.l):1]
            tmp1.=t(tmp1.)

            poly.tmp=poly1_full[names(poly1_full)==tile_placment[[i]][[columns_placment[i]]]]

            coord.tmp=poly.tmp@polygons[[1]]@Polygons[[1]]@coords
            x.d=max(coord.tmp[,1])-min(coord.tmp[,1])
            y.d=max(coord.tmp[,2])-min(coord.tmp[,2])
            tmp1.rturn=EBImage::resize(tmp1.,w=y.d)
            tmp1.rturn=tmp1.rturn[,(length(tmp1.rturn[1,])-maxw+1):length(tmp1.rturn[1,])]
            tmp1.rturn
          }

        tmp1 <- do.call(abind::abind, c(strips., list(along = 1)))
        #tmp1 %>% as.array() %>% as.raster() %>% plot()
        Sys.time()

        #Assign lables to hotspot using an EBImage
        t8=Sys.time()
        tmp2=tmp1

        tmp2=tmp1
        q1=max(quantile(tmp1[tmp1>0],prob=treshlevels[1]),treshhold)
        tmp2[tmp1<q1]=0
        tmp2[tmp1>=q1]=1

        #precise marks
        z1=EBImage::bwlabel(tmp2)

        #make dummy matrices
        dummyz1=matrix(1,ncol=dim(z1)[2],nrow=dim(z1)[1])
        dummyz1[z1==0]=0



        if(!supress_messages)
        {
          message(paste("   Tiles processed:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
          flush.console()
        }

        if(save.mask)
        {
          mask.path=paste(croop_path,"/masks/",filenames,sep="")
          if(!isTRUE(file.info(dirname(mask.path))$isdir)) dir.create(dirname(mask.path),recursive=TRUE)
          writeImage(t(dummyz1),mask.path)
        }

        labels.1=unique(z1[z1!=0])
        extents=NULL
        #Procedures below calculating an overlap and combining blobs if they are overlaping #may be required refinng
        #message(paste("labels: ",length(labels.1)),appendLF=TRUE)
        #stop("Execution halted!")


        #generate and filter proposed polygons
        scale1=1.2
        scale2=3.333
        trsh_level=20 #(assuming 20 pixels is to small to contain readable brand)

        #TOTAL SCALE IS scale1*scale2

        if(length(labels.1)>0)
        {
          #t.v.1=Sys.time()
          mat.rows=matrix(rep(1:length(z1[,1]),length(z1[1,])),nrow=length(z1[,1]),ncol=length(z1[1,]))
          mat.cols=matrix(rep(1:length(z1[1,]),length(z1[,1])),byrow=TRUE, nrow=length(z1[,1]),ncol=length(z1[1,]))
          #b1.l=foreach::foreach(l=1:length(labels.1)) %dopar%
          b1.l=list()
          for(l in 1:length(labels.1))
          {
            max.x=max(mat.cols[z1==labels.1[l]])
            min.x=min(mat.cols[z1==labels.1[l]])
            max.y=max(mat.rows[z1==labels.1[l]])
            min.y=min(mat.rows[z1==labels.1[l]])
            x=(max.x+min.x)/2
            y=(max.y+min.y)/2
            dx=scale1*max(abs(max.x-min.x),abs(max.y-min.y))/2

            x1=c(x-dx,x+dx,x+dx,x-dx)
            y1=c(y-dx,y-dx,y+dx,y+dx)

            xy.points.=rbind(data.frame(x=seq(x-dx,x+dx,by=1),y=y-dx),
                             data.frame(x=seq(x-dx,x+dx,by=1),y=y+dx),
                             data.frame(x=x-dx,y=seq(y-dx,y+dx,by=1)),
                             data.frame(x=x+dx,y=seq(y-dx,y+dx,by=1)))

            xy.points.lab.=data.frame(LAYER=rep(labels.1[l],dim(xy.points.)[1]))

            if(l==1) xy.points=xy.points. else xy.points = rbind(xy.points,xy.points.)
            if(l==1) xy.points.lab=xy.points.lab. else xy.points.lab = rbind(xy.points.lab,xy.points.lab.)

            b1.l[[l]]=Polygons(list(Polygon(cbind(x1,y1))), labels.1[l])
          }

          b1=SpatialPolygons(b1.l, labels.1)
          b1.=SpatialPolygonsDataFrame(b1, data=data.frame(LAYER=labels.1))
          spp=SpatialPointsDataFrame(xy.points, data=xy.points.lab)
          crt=spp%over%b1.
          crt.data=cbind(spp@data,crt)
          crt.data=unique(crt.data[crt.data[,1]!=crt.data[,2],])

          #png("test3.png",width=maxw,height=maxhv)
          #	plot(spp,pch=16,col="orange")
          #	plot(b1.,add=TRUE)
          #dev.off()

          unque.polygons=unique(crt.data[,2])
          for(p in 1:length(unque.polygons))
          {
            select_crt=crt.data[crt.data[,2]==unque.polygons[p],1]
            spp@data[,1][spp@data[,1]%in%select_crt]=unque.polygons[p]
          }



          #get_extents

          labels.2=unique(spp@data[,1])
          spp.coords=coordinates(spp)
          extents.poly.=list()
          for(lab in 1:length(labels.2))
          {
            spp.coords.subset=spp.coords[spp@data[,1]==labels.2[lab],]
            max.x=max(spp.coords.subset[,1])
            min.x=min(spp.coords.subset[,1])
            max.y=max(spp.coords.subset[,2])
            min.y=min(spp.coords.subset[,2])
            x=(max.x+min.x)/2
            y=(max.y+min.y)/2
            dx=scale2*max(abs(max.x-min.x),abs(max.y-min.y))/2

            x1=c(x-dx,x+dx,x+dx,x-dx)
            y1=c(y-dx,y-dx,y+dx,y+dx)
            dx.=data.frame(dx=dx*2)

            if(lab==1) dx.out=dx. else dx.out=rbind(dx.out,dx.)
            extents.poly.[[lab]]=Polygons(list(Polygon(cbind(x1,y1))), labels.2[lab])
            extents.=data.frame(x1=x-dx,x2=x+dx,y1=y-dx,y2=y+dx)
            if(lab==1) extents=extents. else extents=rbind(extents,extents.)

          }

          extents.poly=SpatialPolygons(extents.poly.)
          rownames(dx.out)=labels.2
          extents.poly.data=SpatialPolygonsDataFrame(extents.poly, data=dx.out)

          #plot(subset(extents.poly.data,extents.poly.data@data[,1]>trsh_level*scale2))

          extents=extents[extents.poly.data@data[,1]>trsh_level*scale2,]


          #png("test3.png",width=maxw,height=maxhv)
          #	plot(spp,pch=16,col="orange")
          #	plot(b1.,add=TRUE)
          #	plot(extents.poly,col="blue",add=TRUE)
          #dev.off()


          if(length(extents[,1])<1) extents=NULL

        }else{
          extents=NULL
        }




        t9=Sys.time()

        if(!supress_messages)
        {
          message(paste("   Mask processed:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
          flush.console()
        }
        #extents returning data
        if(is.null(extents))
        {
          prediction_1=data.frame(animal_name="Unknown",x1=NA,x2=NA,y1=NA,y2=NA,file=paste0(tools::file_path_sans_ext(basename(file_path)),"_res_",1:length(extents[,"x1"]),".",tools::file_ext(basename(file_path))),filename=basename(file_path),model=model_name)
        }else{
          prediction_1=data.frame(animal_name="Unknown",x1=extents[,"x1"],x2=extents[,"x2"],y1=extents[,"y1"],y2=extents[,"y2"],file=paste0(tools::file_path_sans_ext(basename(file_path)),"_res_",1:length(extents[,"x1"]),".",tools::file_ext(basename(file_path))),filename=basename(file_path),model=model_name)
        }

        if(is.null(prediction.out)) prediction.out=prediction_1 else prediction.out=rbind(prediction.out,prediction_1)

        if(!dir.exists(data_path)) dir.create(data_path,recursive=TRUE)

        if(!file.exists(paste(data_path,"/prediction.csv",sep="")))
        {
          write.csv(prediction.out,paste(data_path,"/prediction.csv",sep=""),row.names=FALSE)
        }else{
          lines=apply(prediction.out,1,function(X) paste(X,collapse = ","))
          #write(lines, file="prediction.csv",append = TRUE)
          write(lines, file=paste(data_path,"/prediction.csv",sep=""),append = TRUE)
        }


        prediction=list(file_path,extent_list=extents)
        Sys.time()
        #Below is the monitoring function. Have to be rivesed
        if(!is.null(prediction[[2]]))
        {
          if(save_prediction_image)
          {
            #prediction_1.=TLC:::display.results(prediction,save_path=save_path,save_prediction=TRUE,display=FALSE,save_clips=FALSE)
            #prediction_1.$filename=basename(prediction[[1]])
          }
        }


      }else{
        warning(img1.)
        if(!supress_messages) message(paste("   Image corrupted:",filename,sep=""),"\r",appendLF=TRUE)
      }
    }else{
      warning(img1)
      if(!supress_messages) message(paste("   Image corrupted:",filename,sep=""),"\r",appendLF=TRUE)
    }

    if(!supress_messages)
    {
      message(paste("   Image processed:", Sys.time(),"   Tiles:", length(poly1),"/",filename,sep=""),"\r",appendLF=TRUE)
      flush.console()
    }
    t10=Sys.time()
    message(paste(round((f/length(files))*100,2),"%","    ",f, "   b:", t1,"   e:",difftime(t1, t10, units="secs"),"   ",filename,sep=""),"\r",appendLF=TRUE)
    flush.console()

  }
  if(TLC:::get_os()=="win") parallel::stopCluster(cl)
}
