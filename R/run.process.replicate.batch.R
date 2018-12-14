

######### Ligne à éditer pour le batch mode
#batch.mode = 1  # On doit mettre = 1 pour traiter les fichiers en batch
#log.file = "~/Copy/data/BoueesIML/2015/Log/Ap_log_IML4_2015.txt"  # Fichier TXT avec: ID  Repl  Station  Depth	Vol	Farea	blank.file	Ap.good	NAp.good
#path = "~/Copy/data/BoueesIML/2015/Ap/RData/"
#MISSION = "IML4"
#path.out = "~/Copy/data/BoueesIML/2015/Ap/RData/"
#path.png = "~/Copy/data/BoueesIML/2015/Ap/png/"


#log.file = "~/Copy/data/Riverscape2013/Log/Ap_log_Riverscape_2013.txt"  # Fichier TXT avec: ID  Repl  Station  Depth	Vol	Farea	blank.file	Ap.good	NAp.good
#path = "~/Copy/data/Riverscape2013/ApAnapAphy/RData_V3/"
#MISSION = "Riverscape"
#path.out = "~/Copy/data/Riverscape2013/ApAnapAphy/RData_V3/"
#path.png = "~/Copy/data/Riverscape2013/ApAnapAphy/png_V3/"

# IMPORTANT: Les colonnes Ap.good et NAp.good à la fin du fichier LOG servent à
# sélectionner les fichiers qu'on veut retenir pour la moyenne (0 indique que le spectre est éliminé)

########## Lignes à éditer pour le traitement d'un seul fichier (batch.mode = 0).
#ID = "IML4_20150603"
#Repl = c("01", "02") # êrmet de slectionner les spectres qu'on désire garder pour la moyenne
#SpecType = "NAp"
#Station = "IML4"
#Depth = 0



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

  Ap.log = read.table(file=log.file, header=T)
  ix = which(Ap.log$process == 1)
  Stations = levels(droplevels(Ap.log$ID[ix]))
  nID = length(Stations)
  for (i in 1:nID) {

    ix = which(Ap.log$ID == Stations[i])

    #if (MISSION == "IML4") Replicates = paste("0", Ap.log$Repl[ix], sep="")
    #if (MISSION == "Riverscape") Replicates = Ap.log$Repl[ix]
    Replicates = Ap.log$Repl[ix]

    if (any(Ap.log$Ap.good[ix] == 1)) {
      Ap = process.replicate(path,path.png,  Stations[i], Ap.log$Station[ix[1]],
                             "Ap", Replicates[Ap.log$Ap.good[ix] == 1],
                             Ap.log$Depth[ix[1]])

      save(Ap, file=paste(path.out,Stations[i],"_" , "Ap",".RData", sep=""))

    } else {
      print("No good Ap spectra!!! Check Log file")
    }

    if (any(Ap.log$NAp.good[ix] == 1)) {

      Ap = process.replicate(path,path.png,  Stations[i], Ap.log$Station[ix[1]],
                             "NAp", Replicates[Ap.log$NAp.good[ix] == 1],
                             Ap.log$Depth[ix[1]])

      save(Ap, file=paste(path.out,Stations[i],"_" , "NAp",".RData", sep=""))
    } else {
      print("No good NAP spectra!!! Check Log file")
    }

  }

}


