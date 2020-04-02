
run.process.replicate.batch <- function(log.file="Ap_log_TEMPLATE.dat", data.path="./") {


  if (file.exists(data.path)){
    path.png =paste(data.path,"/png/", sep="")
    path =paste(data.path,"/RData/", sep="")
    path.out = path
  } else {
    print("The data.path does not exits.")
    print("The data.path should contains the following subdirectorie:  ")
    print("./png/ and ./RData/")
    print("STOP processing")
    return(0)
  }

  # Lecture des informations dans un fichier texte
  if (!file.exists(log.file)) {
    print("The log.file does not exits.")
    print("STOP processing")
    return(0)
  }

  Ap.log = fread(file=log.file, colClasses = "character")
  names(Ap.log)<-str_to_upper(names(Ap.log))
  Ap.log$ID = as.factor(Ap.log$ID)
  Ap.log$PROCESS = as.numeric(Ap.log$PROCESS)
  Ap.log$AP.GOOD=  as.numeric(Ap.log$AP.GOOD)
  Ap.log$NAP.GOOD= as.numeric(Ap.log$NAP.GOOD)

  ix = which(Ap.log$PROCESS == 1)
  IDs = levels(droplevels(Ap.log$ID[ix]))
  nID = length(IDs)
  print(paste("Total number of IDs to process:", nID))

  for (i in 1:nID) {

    print(paste("ID",i," out of", nID))
    ix = which(Ap.log$ID == IDs[i])


    Replicates = Ap.log$REPL[ix]

    if (any(Ap.log$AP.GOOD[ix] == 1)) {
      Ap = process.replicate(path,path.png,  IDs[i], Ap.log$STATION[ix[1]],
                              "Ap", Replicates[Ap.log$AP.GOOD[ix] == 1],
                              Ap.log$DEPTH[ix[1]], Ap.log$DATE[ix[1]])


      save(Ap, file=paste(path.out,IDs[i],"_" , "Ap",".RData", sep=""))

    } else {
      print("No good Ap spectra!!! Check Log file")
    }

    if (any(Ap.log$NAP.GOOD[ix] == 1)) {


      Ap = process.replicate(path,path.png,  IDs[i], Ap.log$STATION[ix[1]],
                             "NAp", Replicates[Ap.log$NAP.GOOD[ix] == 1],
                             Ap.log$DEPTH[ix[1]], Ap.log$DATE[ix[1]])


      save(Ap, file=paste(path.out,IDs[i],"_" , "NAp",".RData", sep=""))
    } else {
      print("No good NAP spectra!!! Check Log file")
    }

  }

}


