
library(tidyr)
library(dplyr)
library(writexl)
SummarizePreds=function (OPPfolder= pthOPP,
                         Species1=Species0,
                         savePth= paste0("Output/", Species0,"_.xlsx")) {    
  PredsResult=NULL	
  MyErrorResult=NULL	
  AlexErrorResult=NULL	
  
  listOPP=list.files(OPPfolder,full.names=T)
  for (i in 1:length(listOPP)) {					  
    labelinput1=	listOPP[i]					  
    
 #   if (Species1=="NFSAdult") {
      PredsPth=paste0(labelinput1,"\\Predict\\", Species1,"_",basename(labelinput1),".csv")
      MyErrorPTH= paste0(labelinput1,"\\Predict\\Check_difference", Species1, "_",basename(labelinput1),".csv")
      AlexErrorPTH=paste0(labelinput1,"\\report_",basename(labelinput1),".csv")
 #   }
    if(file.exists(PredsPth)) {Preds=read.csv(PredsPth)
    Preds1=Preds %>% group_by(age) %>%
      summarize(count=n()) 			 
    Preds1$date=basename(labelinput1)				 
    PredsResult=rbind(PredsResult,Preds1)
    
    if(file.exists(MyErrorPTH)) {MyError=read.csv(MyErrorPTH)
    MyError$date=basename(labelinput1)
    MyErrorResult=rbind(MyErrorResult,MyError)
    
    }
    if(file.exists(AlexErrorPTH)) {AlexError=read.csv(AlexErrorPTH)
    AlexErrorResult=rbind(AlexErrorResult,AlexError)
    
    
    }
    }else {print(paste0(labelinput1, "_No preds found"))}
  }
  PredsResult1<<-PredsResult %>%
    spread(age,count)	
  MyErrorResult<<-MyErrorResult ; if (is.null(MyErrorResult)==T) {MyErrorResult=data.frame(No_data="No_data")}
  AlexErrorResult<<-AlexErrorResult ; if (is.null(AlexErrorResult)==T) {AlexErrorResult=data.frame(No_data="No_data")}
  
  write_xlsx(x = list(PredsResult=PredsResult1, MyErrorResult=MyErrorResult, AlexErrorResult=AlexErrorResult), path = savePth, col_names = T)
  print("Done")
}
SummarizePreds()
