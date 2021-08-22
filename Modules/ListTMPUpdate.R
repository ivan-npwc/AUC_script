
if (length(OPPListPred1)==0) {OPPListPred1=""}

         lastBackUp=unique(listTMP$backUpDate)
        if (lastBackUp != format(Sys.Date(),"%Y%m%d")) {
  from= "Modules"
  from1=list.files(from,full.names=T)
  to=paste0("BackUp\\",format(Sys.Date(),"%Y%m%d"))
  dir.create(to)
  file.copy(from1,to)
}                                     


ListTMP <- list(
  Species=Species,
  BatchProcessVector = BatchProcessVector,
  OPPListPred1= OPPListPred1,
  backUpDate= format(Sys.Date() ,"%Y%m%d")
 # SpeciesManager=SpeciesManager,
 # listOPP1=listOPP1,
 # ProsessManager=ProsessManager  
                )
				
				
    saveRDS(ListTMP, "ListTMP")
    listTMP <<- readRDS("ListTMP")


            Species<<-unique(listTMP$Species)
            BatchProcessVector<<-unique(listTMP$BatchProcessVector)
            OPPListPred1<<-unique(listTMP$OPPListPred1)
		#	SpeciesManager<<-SpeciesManager
		#	listOPP1<<-listOPP1
		#	ProsessManager<<-ProsessManager
			