
trainLog=function(trainDir1=trainDir) {

  Species=basename(trainDir1)
  ImgsDir=paste0(trainDir1,"\\Image")
  #TrainIndex
  #BatchIntens
  #Split=T	
  #Smooth
  #batch_size
  #epochs
  LogPth= "System data/TrainLog.csv"	   
  Date=as.character(Sys.time())
  Imgs=list.files(ImgsDir)
  
  
  
  
  if (file.exists(LogPth)==F){
    TrainReport=data.frame(
	  start1=Sys.time(),
      Date=Date,
      Species=Species,
      NewImgs=Imgs)
  }
  
  
  if (file.exists(LogPth)==T){trainLog=read.csv(LogPth)}
  
  SortSpeciesTable=trainLog[trainLog$Species==Species,]
  NewImgsRep=Imgs[!(Imgs %in% SortSpeciesTable$NewImgs)]
  if (length(NewImgsRep) == 0) {NewImgsRep=0}
  
  TrainReport=data.frame(
    start1=Sys.time(),
    Date=Date,
    Species=Species,
    NewImgs=NewImgsRep) 
  
  TrainReportFin=rbind(trainLog,TrainReport)								 
  
  
  
  write.csv(TrainReport,LogPth,row.names=F)
  
  
  
}
