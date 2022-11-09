function(input, output, session) {
############################################################### 
  re = observeEvent(input$ResetSetting, {
    labelInput<<-choose.dir(default = labelInput)
    nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-substr(labelInput,0, nchar(labelInput)-nchaBName)
    listOPP<<-list.files(pthOPP)
    NanmlsOnTiles<<-input$NanmlsOnTiles
    
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'ResetSetting',   label=paste0("INPUT:___", labelInput))
    pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
    if (file.exists(pth_log)==F) {log1=NULL } else {log1<<-read.csv(pth_log)}
    })
########################################################################################### 
 observeEvent(input$Up, {
    Species <<-input$Species
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
  source("Modules/ListTMPUpdate.r")
  
 })
######################################################################################
  observeEvent(input$SPTD, {
    BatchIntens<<-input$BatchIntens
    Split<<-input$Split	       
    Smooth<<-input$Smooth       
    batch_size<<-input$batch_size
    epochs<<-input$epochs
    Deformation<<-input$Deformation
        source("Modules/ListUniqueUpdate.r")
    
  })
  ############################################################################### 
  observeEvent(input$Start_Batch_process, {
   # NanmlsOnTiles<<-  input$NanmlsOnTiles
   # SpeciesManager<<-input$SpeciesManager
   # ProsessManager<<-input$ProcessManager
    listOPP1<<-input$OPPListMngr
    Species <<-input$Species
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
  	NanmlsOnTiles<<-input$NanmlsOnTiles
	Terrain<<-input$Terrain
	Count_type<<-input$type
	UseAllHauloutImages  <<- input$UseAllHauloutImages
	loopCheck<<-0
	HaulAsModel<<-input$HaulAsModel
	UseRookeryOnly<<-input$UseRookeryOnly
	SubFold<<-input$SubFold
	##########
   if (OPPListPred1=="All") {OPPListPred1=list.files(as.character(unique(listValue$pthOPP)))}
   ForLoop_OPPListPred1 <<- OPPListPred1
     source("Modules/ListTMPUpdate.r")
	for (d in 1:length(ForLoop_OPPListPred1)) {
	       dayInProgress <<-ForLoop_OPPListPred1[d]
	       labelInput <<-   paste0(unique(listValue$pthOPP),"\\",dayInProgress)
	##########
	       
	#check1= length(list.files(labelInput, pattern = ".files"))
	#check2=	"Save" %in% BatchProcessVector
		 
    if (SubFold==T)  {SubFolds<<-list.dirs(labelInput, full.names=T,recursive=F)} else { SubFolds<<-labelInput} 
#############  
		   
    for (k in 1:length(SubFolds)) {
	labelInput<<-SubFolds[k]
	
withProgress(message = paste0("Doing  ",labelInput), value = d , {
        pth_log<<-paste0(labelInput,"\\",substr(basename(labelInput),1,15), "-Log.csv")
	      if (file.exists(pth_log)==F) {log1=NULL } else {log1<<-read.csv(pth_log)}
         Haulout_polygon <<-paste0(labelInput,"\\Polygons\\Houlout\\Houlout.shp")	
         Rookery_polygon <<-paste0(labelInput,"\\Polygons\\Rookery\\Rookery.shp")	
 
   for (i in 1:length(BatchProcessVector)) {
      withProgress(message = paste0(BatchProcessVector[i]), value = i, { 
             action<<-paste0("Modules/", BatchProcessVector[i],".r")
             ValueAction <<-""
              startAction <<- data.frame(moment="Start", action=paste0(action), dtime=paste0(date() ),ValueAction=ValueAction)
              log1=rbind(log1,startAction)
              write.csv(log1,pth_log, row.names = F)
      Sys.sleep(1) 
      source(action)
	    Sys.sleep(1) 
     	print(paste0("Done  ",action,"   ",labelInput ))
           endAction <<-data.frame(moment="Finish", action=paste0(action), dtime=paste0(date() ),ValueAction=ValueAction)
           log1=rbind(log1,endAction)
           write.csv(log1,pth_log, row.names = F)               
   }) 
   }
   OPPListPred1<<-OPPListPred1[!(OPPListPred1 %in% dayInProgress)]
   message(paste0("Done  ",labelInput,"----",Sys.time() ),appendLF=TRUE)
   source("Modules/ListTMPUpdate.r")  
   })
   }
   }
   
   beep()
   })
########################################################################################  
  output$text <- renderDataTable(log1)
 ##############################################################################################################
  observeEvent(input$Get_data, { 
    SpeciesG <<-input$SpeciesG
    YearG <<-input$YearG
    SiteG <<-input$SiteG
  source("Modules/Get_data.r")
    print("Done")
    })
  ###################################################################################
  observeEvent(input$DeleteALL, { 
    SpeciesG <<-input$SpeciesG
    YearG <<-input$YearG
    SiteG <<-input$SiteG
  source("Modules/Delete.r")
    print("All data were deleted")
    })
  ################################################################################### 
  re4 = observeEvent(input$trainDir, {
    trainDir<<-choose.dir()	 
    updateActionButton (session,'trainDir',   label= paste0("trainDir:  ",trainDir))
	source("Modules/ListUniqueUpdate.r")
  })
  ########################################################################################
  re4 = observeEvent(input$Model_base, {
    Model_base<<-file.choose()
    updateActionButton (session,'Model_base',   label= paste0("MODEL BASE:  ",Model_base))
	source("Modules/ListUniqueUpdate.r")
  })
#########################################################################################################################
   re6 = observeEvent( input$Rookery_polygon, {
     Rookery_polygon<<-file.choose()
      updateActionButton (session,'Rookery_polygon',   label= paste0("Rookery_polygon:  ",Rookery_polygon))
    }) 
#############################################################################################################
   re7 = observeEvent( input$Haulout_polygon, {
     Haulout_polygon<<-file.choose()
     updateActionButton (session,'Haulout_polygon',   label= paste0("Haulout_polygon:  ",Haulout_polygon))
   }) 
 #######################################################################
  re8 = observeEvent( input$Image, {
    Image_dir_Sin<<-choose.dir()
   updateActionButton (session,'Image',   label= paste0("Image:                ",Image_dir_Sin))
  }) 
  ######################################################################
  re9 = observeEvent( input$Mask, {
    Mask_dir_Sin<<-choose.dir()
    updateActionButton (session,'Mask',   label= paste0("Mask:                ",Mask_dir_Sin))
  })  
  #########################################################################################
  re10 = observeEvent( input$Pth_img_error, {
    Pth_img_error<<-file.choose()
    updateActionButton (session,'Pth_img_error',   label= paste0("Pth_img_error:                ",Pth_img_error))
  })  
  #########################################################################################
  re11 = observeEvent( input$Exlude_polygon, {
    Exlude_polygon<<-file.choose()
    updateActionButton (session,'Exlude_polygon',   label= paste0("Exlude_polygon:  ",Exlude_polygon))
  }) 
  ##################################################################################
  re110 = observeEvent(input$AnmlsMearPth, {
    AnmlsMearPth<<-file.choose()
    updateActionButton (session,'AnmlsMearPth',   label= paste0("AnmlsMearPth:  ",AnmlsMearPth))
  }) 
  ##################################################################################
  re13 = observeEvent( input$Weight, {
    Weight<<-file.choose()
	 source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'Weight',   label= paste0("Weight:  ",Weight))
  }) 
  #####################################################################################
  re14 = observeEvent( input$ModelPoligon, {
    ModelPoligon<<-file.choose()
    updateActionButton (session,'ModelPoligon',   label= paste0("ModelPoligon:  ",ModelPoligon))
  }) 
  #####################################################################################
  re15 = observeEvent( input$ObserverPoint, {
    ObserverPoint<<-file.choose()
    updateActionButton (session,'ObserverPoint',   label= paste0("ObserverPoint:  ",ObserverPoint))
  }) 
  #####################################################################################
  re16 = observeEvent( input$PredictPoint, {
    PredictPoint<<-file.choose()
    updateActionButton (session,'PredictPoint',   label= paste0("PredictPoint:  ",PredictPoint))
  }) 
  #####################################################################################
  re17 = observeEvent( input$NFS_Pup_weight_pth, {
    NFS_Pup_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Pup_weight_pth',   label= paste0("NFS_Pup_weight_pth:  ",NFS_Pup_weight_pth))
  }) 
  ######################################################################
  re18 = observeEvent(input$SSL_Adult_weight_pth, {
    SSL_Adult_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SSL_Adult_weight_pth',   label= paste0("SSL_Adult_weight_pth:  ",SSL_Adult_weight_pth))
  })  
#########################################################################  
  re19 = observeEvent( input$SSL_Pup_weight_pth, {
    SSL_Pup_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SSL_Pup_weight_pth',   label= paste0("SSL_Pup_weight_pth:  ",SSL_Pup_weight_pth))
  }) 
 ###################################################################### 
  re20 = observeEvent( input$NFS_Adult_weight_pth, {
    NFS_Adult_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Adult_weight_pth',   label= paste0("NFS_Adult_weight_pth:  ",NFS_Adult_weight_pth))
  }) 
  ###################################################################### 
  re24 = observeEvent( input$WLRS_Sand_weight_pth, {
    WLRS_Sand_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'WLRS_Sand_weight_pth',   label= paste0("WLRS_Sand_weight_pth:  ",WLRS_Sand_weight_pth))
  }) 
  ###################################################################### 
    re24_1 = observeEvent( input$WLRS_Rocky_weight_pth, {
    WLRS_Rocky_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'WLRS_Rocky_weight_pth',   label= paste0("WLRS_Rocky_weight_pth:  ",WLRS_Rocky_weight_pth))
  }) 
  ###################################################################### 
  re21 = observeEvent( input$SQLite_path, {
    SQLite_path<<-file.choose()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SQLite_path',   label= paste0("SQLite_path:  ",SQLite_path))
  }) 
  #################################################################
  re22 = observeEvent( input$KK_Effort, {
    KK_Effort<<-file.choose()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'KK_Effort',   label= paste0("KK_Effort:  ",KK_Effort))
  }) 
  #########################################################################################
    re23 = observeEvent( input$SSL_Age_pth, {
    SSL_Age_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SSL_Age_pth',   label= paste0("SSL_Age_pth:  ",SSL_Age_pth))
  }) 
  #######################################################################################  
     re24 = observeEvent( input$DirModelsCheck, {
    DirModelsCheck<<-choose.dir()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'DirModelsCheck',   label= paste0("DirModelsCheck:  ",DirModelsCheck))
  }) 
  #######################################################################################  
       re25 = observeEvent( input$System_data, {
    System_data<<-choose.dir()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'System_data',   label= paste0("System_data:  ",System_data))
  }) 
  ####################################################################################### 
       re26 = observeEvent( input$LRGH_MSRMNTS, {
    LRGH_MSRMNTS<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'LRGH_MSRMNTS',   label= paste0("LRGH_MSRMNTS:  ",LRGH_MSRMNTS))
  }) 
#################################
    re27 = observeEvent( input$LRG_pth, {
    LRG_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'LRG_pth',   label= paste0("LRG_pth:  ",LRG_pth))
  }) 
#################################
re28 = observeEvent(input$DarkTheme, {
DarkTheme<<-input$DarkTheme
    source("Modules/ListUniqueUpdate.r")
  }) 

#########################
re28 = observeEvent(input$type, {
type<<-input$type
    source("Modules/ListUniqueUpdate.r")
  }) 

#########################



  observeEvent(input$Unet_train, {
    Split1<<-input$Split
    BatchIntens1<<-input$BatchIntens
    Smooth1<<-input$Smooth
    batch_size1<<-input$batch_size
    epochs1<<-input$epochs
    dfrmn1<<-input$Deformation
	TypeTrain<<-input$TypeTrain
    
    withProgress(message = 'Artificial intelligent prepare for the Word destruct..', value = 0, {
      
   source("Modules/UNET_Train_256.r")
    incProgress(1/1, detail = paste("Train", 1))
    Sys.sleep(0.1)  
    })
  })
  ###############################
  observeEvent(input$site, {
  site <<- input$site
   source("Modules/ListUniqueUpdate.r")
  })
################################################################################################
  observeEvent(input$Unet_train_512, {
    withProgress(message = 'Artificial intelligent prepare for the Word destruct..', value = 0, {
      NewTrain<<-input$NewTrain
      source("Modules/UNET_Train_512.r")
      incProgress(1/1, detail = paste("Train", 1))
      Sys.sleep(0.1)  
    })
  })
############################################################################################  
  }
  
