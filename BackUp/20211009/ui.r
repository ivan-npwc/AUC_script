options = list(display.mode='showcase')
navbarPage(actionLink('ResetSetting',   label=paste0("INPUT:___", labelInput), style = "font-size:12px;"),
			 
  id="nav", 
 ############################################################################################          
           tabPanel("CIC",
                     fluidPage (
                       
                    #   column(width = 4,actionLink('Rookery_polygon',   label= paste0("Rookery_polygon:  ", Rookery_polygon), style = "font-size:12px;")),
                    #   column(width = 4,actionLink('Haulout_polygon',   label= paste0("Haulout_polygon:  ", Haulout_polygon), style = "font-size:12px;")),
                    #   column(width = 4,actionLink('Exlude_polygon',   label= paste0("Exlude_polygon:  ", Exlude_polygon), style = "font-size:12px;")),
                    #   column(width = 10,actionLink('AnmlsMearPth',   label= paste0("AnmlsMearPth:  ", AnmlsMearPth), style = "font-size:12px;")),
                 
                 fluidRow(column(width = 11, selectInput('Bath_Process', 'Bath Process', width="1000px",multiple = T,
                                                        c("00_DataStructure"="DataStructure",
														  "01_Count"="Count",
														 "01_ SSL_Pup_Predict"="SSL_Pup_Predict",
														  "02_CreateModelPolygons"="CreateModelPolygons",
														  "03_Sort_Image_error" = "Sort_Image_error",
														  "04_MaskAutoCreator" = "MaskAutoCreator",
														  "--------------------------------------------"="B",
														  "01_Unzip"="Unzip",
                                                          "02_KMLprepare"="KMLprepare",
                                                          "03_Image_prepare"="Image_prepare",
                                                          "04_Unet"="Unet",
                                                          "05_Blob_analisis"="BlobAnalys",
                                                          "06_Geo_ref"="Geo_ref",
                                                          "07_KML"="KML",
                                                          "08_Error_calculate" = "Error_calculate",
                                                        #  "--------------------------------------------"="B",
                                                          "09_SSL_Age_PREPROCESING"= "Age_PREPROCESING",
                                                          "10_SSL_Age_PREDICT"="Age_PREDICT",
                                                        # "11_SSL_Age_POSTPROCESING"="Age_POSTPROCESING",
                                                         # "--------------------------------------------"="B1",
														#  "12_prediction_evaluation"="prediction_evaluation",
														#  "--------------------------------------------"="B2",
														#  "01_Points_table_for_mask" = "Points_table_for_mask",
														#  "02_Animals_Count_On_Image"="Animals_Count_On_Image",
														#  "03_Points create"="Points create",
														 #  "04_Img_Copy_for_msk"="Img_Copy_for_msk",
														  
														  "--------------------------------------------"="B3",
														 #   "01_NormalizePxSiz" = "NormalizePxSiz",
														    
														    "01_Get_Agisoft_Shape"="GetAgisoftShape",
														    "Save"="Save",
															"02_ModelToWeight"="ModelToWeight",
														    "AGE_TrainPrepare"="AGE_TrainPrepare",
													 "-----WLRS----------------------------"="B4",
													 "01_Points_table_for_mask"="Points_table_for_mask",
													 "02_Animals_Count_On_Image"="Animals_Count_On_Image",
													 "03_Points_create"="Points_create",
													 "04_Img_Copy_for_msk"="Img_Copy_for_msk",
													 "05_ImgFltrMsk"="ImgFltrMsk",
													 "01_ModelsCheck"="ModelsCheck"
															
                                                          ),
														                             selected=listTMP$BatchProcessVector
                                                        )),
                          column(width = 6, selectInput('OPPListPred', 'OPP List', width="1000px",multiple = T,
                                                        c("All",listOPP),
							                            selected=listTMP$OPPListPred1))																								
														), 
														############
			fluidRow(column(3, conditionalPanel("input.Bath_Process=='Save'",
                                     selectInput("type", "type", c("auto_count_full","manual_count_full","manual_count_model"), multiple=F)
                                                                 ))),
														#############
			fluidRow(column(width = 4, selectInput('site', 'site', width="1000px",multiple = F,
                                                                                           c("138","30","19","20"),
							                                                              selected=site))),
                 
			     fluidRow(column(width = 11,selectInput('Species', 'Species', width="200px",multiple = F,
                                                         c("NFS_Adult"="NFSAdult",
                                                           "SSL_Adult"="SSLAdult",
                                                           "NFS_Pup"="NFSPup",
                                                           "SSL_Pup"="SSLPup",
														   "WLRS"="WLRS"),
													          selected=listTMP$Species
                 )),
				 ##################
				 column(3, conditionalPanel("input.Species=='WLRS'",
                               selectInput("Terrain", "Terrain", c("Sand","Rocky"), multiple=F)
                               ))
							   ),
				 ##################
				 
				 
				 
				 ########
				 
				 
				 fluidRow(column(width = 4, actionButton('Start_Batch_process', 'Start', width="200px")),
				          column(width = 4, actionButton('Up', 'Up', width="200px"))
                           ),
                
                )),
#######################################################################################################
tabPanel("Setting",
         
		 
			 
		hr(), 
         fluidRow(column(width = 4, actionLink('System_data',label= paste0("System_data:  ", System_data), style = "font-size:12px;"))),
        hr(),
            fluidRow(column(width = 12,actionLink('NFS_Adult_weight_pth',label= paste0("NFS_Adult_weight_pth:  ", NFS_Adult_weight_pth), style = "font-size:12px;"))),
         hr(),
            fluidRow(column(width = 12,actionLink('NFS_Pup_weight_pth',label= paste0("NFS_Pup_weight_pth:  ", NFS_Pup_weight_pth), style = "font-size:12px;"))),
         hr(),
             fluidRow(column(width = 12,actionLink('SSL_Adult_weight_pth',label= paste0("SSL_Adult_weight_pth:  ", SSL_Adult_weight_pth), style = "font-size:12px;"))),
         hr(),
             fluidRow(column(width = 12,actionLink('SSL_Pup_weight_pth',label= paste0("SSL_Pup_weight_pth:  ", SSL_Pup_weight_pth), style = "font-size:12px;"))),
         hr(),
		     fluidRow(column(width = 12,actionLink('WLRS_Sand_weight_pth',label= paste0("WLRS_Sand_weight_pth:  ", WLRS_Sand_weight_pth), style = "font-size:12px;"))),
		hr(),
		     fluidRow(column(width = 12,actionLink('WLRS_Rocky_weight_pth',label= paste0("WLRS_Rocky_weight_pth:  ", WLRS_Rocky_weight_pth), style = "font-size:12px;"))),
		 hr(),
		     fluidRow(column(width = 12,actionLink('SSL_Age_pth',label= paste0("SSL_Age_pth:  ", SSL_Age_pth), style = "font-size:12px;"))),
         hr(),
             fluidRow(column(width = 12,actionLink('SQLite_path',label= paste0("SQLite_path:  ", SQLite_path), style = "font-size:12px;"))),
		 hr(),   
		    fluidRow(column(width = 12,actionLink('KK_Effort',label= paste0("KK_Effort:  ", KK_Effort), style = "font-size:12px;"))),
	   	 hr(),
		   fluidRow(column(width = 12,actionLink('DirModelsCheck',label= paste0("DirModelsCheck:  ", DirModelsCheck), style = "font-size:12px;"))),
		 hr(),
	  	   fluidRow(dataTableOutput("text")) 
),
#####################################################################################################
tabPanel("Train",
        
         actionLink('trainDir',   label= paste0("trainDir:  ", trainDir), style = "font-size:12px;"),
hr(),  
      selectInput("TypeTrain", "",
        c(TrainEmptyModel = "TrainEmptyModel", Retrain = "Retrain")
      ),
      conditionalPanel(
        condition = "input.TypeTrain == 'Retrain'",
		#actionLink('Model_base',   label= paste0("MODEL BASE:  ", Model_base), style = "font-size:12px;"),
		#hr(),
		actionLink('Weight',   label= paste0("Weight:  ", Weight), style = "font-size:12px;"),
		hr(),     
       ),

	  numericInput("BatchIntens", "Batch Intens",value=BatchIntens,min=1,max=10,step=0.1),
      hr(), 
      numericInput("batch_size", "batch_size",value=batch_size,min=1,max=400),	  
	  hr(),
      numericInput("epochs", "epochs",value=epochs,min=1,max=40),		
		  
   
		 
		 
         fluidRow(column(width = 4, actionButton('Unet_train', 'Unet_train_256',width="400px"))),
     #  fluidRow(column(width = 4, actionButton('Unet_train_512', 'Unet_train_512', width="400px"))),
       fluidRow(column(width = 4, actionButton('SPTD', 'Save train parametrs as defult', width="400px"))),
        
      
    
         
         
),
####################################################################################################                     
tabPanel("Export",
         fluidRow(  column(3, selectInput("SpeciesG", "SpeciesG", c("All"="", c("SSL","CU")), multiple=TRUE)),
				          	column(3,selectInput("YearG", "YearG", c("All"="", c("2016","2017","2018","2019","2020")), multiple=TRUE)),
					          column(3,selectInput("SiteG", "SiteG", c("All"="", listR_site), multiple=TRUE))),
         fluidRow(  column(3, actionButton('Get_data', 'Get_data', width="400px"))),
         fluidRow(column(3, actionButton('Delete', 'Delete', width="400px")))		 

         
),

####################################################################################################  


conditionalPanel("false", icon("crosshair"))
 ) 
