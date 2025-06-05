function(input, output, session) {
############################################################### 
  re = observeEvent(input$ResetSetting, {
   # nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-choose.dir(default = pthOPP)
    listOPP<<-list.files(pthOPP,recursive=T, pattern=".psx", full.names=T)
  
    source("Modules/ListUniqueUpdate.r")
	NanmlsOnTiles<<-input$NanmlsOnTiles
    updateActionButton (session,'ResetSetting',   label=paste0("INPUT:___", pthOPP))
	message("The list of OPP has been updated", appendLF=TRUE)
	
   # pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
   # if (file.exists(pth_log)==F) {log1=NULL } else {log1<<-read.csv(pth_log)}
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
	Count_type<<-input$Count_type
	UseAllHauloutImages  <<- input$UseAllHauloutImages
	loopCheck<<-0
	HaulAsModel<<-input$HaulAsModel
	UseRookeryOnly<<-input$UseRookeryOnly
	#SubFold<<-input$SubFold
	UseEffort<<-input$UseEffort
	########################################################################
	 rep2 <<- NULL
   if (OPPListPred1=="All") {OPPListPred1 <<- listOPP}
   source("Modules/ListTMPUpdate.r")
   # pth <<- paste0(pthOPP,"\\log.csv")
  #  lg=data.frame(OPPListPred1=OPPListPred1,Process="Start",dtime=paste0(date())); write.csv(lg,pth,row.names = F)   
#############  	   
	while (OPPListPred1 != "") {
       k <<- length(OPPListPred1)
	   psx <<- OPPListPred1[1]
	   
	   labelInput <<- gsub(basename(psx),"", psx)
	   #  lg = read.csv(pth)
        withProgress(message = paste0("Doing  ",labelInput), value = k , {    # for all opp
   for (g in 1:length(BatchProcessVector)) {
               withProgress(message = paste0(BatchProcessVector[g]), value = g, {     # 
               action <<- paste0("Modules/", BatchProcessVector[g],".r")
			   source(action)       
      Sys.sleep(1) 
     	     print(paste0("Done  ",action,"   ",labelInput ))
			 
              # strt <<- data.frame(OPPListPred1=psx, Process=BatchProcessVector[g], dtime=paste0(date()))
              # lg = rbind(lg,strt)
             # write.csv(lg,pth,row.names = F)                
   }) 
   }
   OPPListPred1<<-OPPListPred1[!(OPPListPred1 %in% psx)]
   message(paste0("Done  ",labelInput,"----",Sys.time() ),appendLF=TRUE)
   source("Modules/ListTMPUpdate.r")  
   })
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
  re2 = observeEvent(input$trainDir, {
    trainDir<<-choose.dir()	 
    updateActionButton (session,'trainDir',   label= paste0("trainDir:  ",trainDir))
	source("Modules/ListUniqueUpdate.r")
  })
  ########################################################################################
  re3 = observeEvent(input$Model_base, {
    Model_base<<-file.choose()
    updateActionButton (session,'Model_base',   label= paste0("MODEL BASE:  ",Model_base))
	source("Modules/ListUniqueUpdate.r")
  })
#########################################################################################################################
   re4 = observeEvent( input$Rookery_polygon, {
     Rookery_polygon<<-file.choose()
      updateActionButton (session,'Rookery_polygon',   label= paste0("Rookery_polygon:  ",Rookery_polygon))
    }) 
#############################################################################################################
   re5 = observeEvent( input$Haulout_polygon, {
     Haulout_polygon<<-file.choose()
     updateActionButton (session,'Haulout_polygon',   label= paste0("Haulout_polygon:  ",Haulout_polygon))
   }) 
 #######################################################################
  re6 = observeEvent( input$Image, {
    Image_dir_Sin<<-choose.dir()
   updateActionButton (session,'Image',   label= paste0("Image:                ",Image_dir_Sin))
  }) 
  ######################################################################
  re7 = observeEvent( input$Mask, {
    Mask_dir_Sin<<-choose.dir()
    updateActionButton (session,'Mask',   label= paste0("Mask:                ",Mask_dir_Sin))
  })  
  #########################################################################################
  re8 = observeEvent( input$Pth_img_error, {
    Pth_img_error<<-file.choose()
    updateActionButton (session,'Pth_img_error',   label= paste0("Pth_img_error:                ",Pth_img_error))
  })  
  #########################################################################################
  re9 = observeEvent( input$Exlude_polygon, {
    Exlude_polygon<<-file.choose()
    updateActionButton (session,'Exlude_polygon',   label= paste0("Exlude_polygon:  ",Exlude_polygon))
  }) 
  ##################################################################################
  re10 = observeEvent(input$AnmlsMearPth, {
    AnmlsMearPth<<-file.choose()
    updateActionButton (session,'AnmlsMearPth',   label= paste0("AnmlsMearPth:  ",AnmlsMearPth))
  }) 
  ##################################################################################
  re11 = observeEvent( input$Weight, {
    Weight<<-file.choose()
	 source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'Weight',   label= paste0("Weight:  ",Weight))
  }) 
  #####################################################################################
  re12 = observeEvent( input$ModelPoligon, {
    ModelPoligon<<-file.choose()
    updateActionButton (session,'ModelPoligon',   label= paste0("ModelPoligon:  ",ModelPoligon))
  }) 
  #####################################################################################
  re13 = observeEvent( input$ObserverPoint, {
    ObserverPoint<<-file.choose()
    updateActionButton (session,'ObserverPoint',   label= paste0("ObserverPoint:  ",ObserverPoint))
  }) 
  #####################################################################################
  re14 = observeEvent( input$PredictPoint, {
    PredictPoint<<-file.choose()
    updateActionButton (session,'PredictPoint',   label= paste0("PredictPoint:  ",PredictPoint))
  }) 
  #####################################################################################
  re15 = observeEvent(input$NFS_Pup_weight_pth, {
    NFS_Pup_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Pup_weight_pth',   label= paste0("NFS_Pup_weight_pth:  ",NFS_Pup_weight_pth))
  }) 
  ######################################################################
  re16 = observeEvent(input$SSL_Adult_weight_pth, {
    SSL_Adult_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SSL_Adult_weight_pth',   label= paste0("SSL_Adult_weight_pth:  ",SSL_Adult_weight_pth))
  })  
#########################################################################  
  re17 = observeEvent( input$SSL_Pup_weight_pth, {
    SSL_Pup_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SSL_Pup_weight_pth',   label= paste0("SSL_Pup_weight_pth:  ",SSL_Pup_weight_pth))
  }) 
 ###################################################################### 
  re18 = observeEvent( input$NFS_Adult_weight_pth, {
    NFS_Adult_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Adult_weight_pth',   label= paste0("NFS_Adult_weight_pth:  ",NFS_Adult_weight_pth))
  }) 
  ###################################################################### 
  re19 = observeEvent( input$WLRS_Sand_weight_pth, {
    WLRS_Sand_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'WLRS_Sand_weight_pth',   label= paste0("WLRS_Sand_weight_pth:  ",WLRS_Sand_weight_pth))
  }) 
  ###################################################################### 
    re20 = observeEvent( input$WLRS_Rocky_weight_pth, {
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
re29 = observeEvent(input$type, {
type<<-input$type
    source("Modules/ListUniqueUpdate.r")
  }) 
#########################
 re30 = observeEvent( input$NFS_AN_TF_UNET, {
    NFS_AN_TF_UNET<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_AN_TF_UNET',   label= paste0("NFS_AN_TF_UNET:  ",NFS_AN_TF_UNET))
  }) 
  ######################################################################
 re30 = observeEvent( input$NFS_Pup_weight_pth512, {
    NFS_Pup_weight_pth512<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Pup_weight_pth512',   label= paste0("NFS_Pup_weight_pth512:  ",NFS_Pup_weight_pth512))
  }) 
  ######################################################################

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
  
