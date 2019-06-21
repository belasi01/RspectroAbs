#' Process Particulate absorption spectra in batch mode
#'
#' This high level function reads an ASCII file containing the
#' list of samples to process. It calls \code{\link{process.Ap}}.
#'
#' @param log.file is the name of the ASCII file containing the
#' list of samples to process (see details below).
#' @param data.path is the path where the csv folder is located (txt folder in the case of ULTRAPATH).
#' So the files must be stored in data.path/csv/.  Default is "./".
#'
#' @details The most important thing to do before runing this programm is to prepare
#' the log.file. This file contains 11 fields (TAB delimated) :
#'
#'   ID         Unique ID of the sample
#'
#'   Repl       A letter corresponding to the replicat (A,B,C,etc)
#'
#'   Station    Station name
#'
#'   Date       Date of sampling
#'
#'   Depth      Depth of the sample
#'
#'   Vol        Filtered volume in mL
#'
#'   Farea      clearance area of particles on filter in m^2
#'
#'   blank.file ID if the reference blank filter
#'
#'   Ap.good	   Boolean quality control indicator  (1=good ; 0 = not good)
#'
#'   NAp.good   Boolean quality control indicator (1=good ; 0 = not good)
#'
#'   process    Boolean (1=to be process ; 0 = to skip in the batch processing)
#'
#'   NAP.method String indicating the method is retained to derive phytoplankton
#'   absorption ("Measured", "Fitted", "BS90_1"", "BS90_2")


run.process.Ap.batch <- function(log.file="Ap_log_TEMPLATE.dat", data.path="./") {

  # Lecture des informations dans un fichier texte
  if (file.exists(data.path)){
#    path.csv =paste(data.path,"/csv/", sep="")
#    path.blank =paste(data.path,"/blank/", sep="")

    path.csv =  file.path(data.path,"csv")
    path.blank =file.path(data.path,"blank")

    if (file.exists(path.csv) & file.exists(path.blank)) {
      print("Data path exists")
    } else {
      print("One of data path is missing")
      print("Check the path:")
      print(path.csv)
      print(path.blank)
      print("STOP processing")
      return(0)
    }
  } else {
      print("The data.path does not exits.")
      print("Put the data in data.path/csv/ and blank in data.path/blank/ ")
      print("STOP processing")
      return(0)
  }

  # Check the output directories and Create them if they are not available.

  path.png = file.path(data.path,"png")
  path.out = file.path(data.path,"RData")


  if (!file.exists(path.png)) dir.create(path.png)
  if (!file.exists(path.out)) dir.create(path.out)

  if (!file.exists(log.file)) {
    default.log.file =  file.path( Sys.getenv("RspectroAbs_DATA_DIR"), "Ap_log_TEMPLATE.dat")

    file.copy(from = default.log.file, to = log.file)
    cat("EDIT file", log.file, "and CUSTOMIZE IT\n")
    return(0)
  }


  Ap.log = read.table(file=log.file, header=T, sep="\t")
  names(Ap.log)<-str_to_upper(names(Ap.log))

  nsample = length(Ap.log$ID)
  for (i in 1:nsample) {

    if (Ap.log$PROCESS[i] == 1 ) {

      basename =paste(Ap.log$ID[i],"_" , Ap.log$REPL[i] ,"_Ap", sep="")
      print(paste("Process: ", basename))

      # Lectures des fichiers
      filen=paste(path.csv,"/",basename,".Sample.Raw.csv", sep="")
      if (file.exists(filen)) {
        sample = read.LAMBDA850(filen)
      } else {
        print(paste("File ", filen, "does not exists"))
        print("STOP PROCESSING")
        return(0)
      }

      filen=paste(path.blank,"/",Ap.log$BLANK.FILE[i],"_Ap.Sample.Raw.csv", sep="")
      if (file.exists(filen)) {
        blank = read.LAMBDA850(filen)
      } else {
        print(paste("File ", filen, "does not exists"))
        print("STOP PROCESSING")
        return(0)
      }
      # Calcul de l'absorption
      Ap = process.Ap(sample, blank, Ap.log$FAREA[i], Ap.log$VOL[i],
                      Ap.log$ID[i], Ap.log$STATION[i], "Ap",
                      Ap.log$REPL[i], Ap.log$DEPTH[i], Ap.log$DATE[i])

       save(Ap, file=paste(path.out,"/",basename,".RData", sep=""))

      png(file=paste(path.png,"/",Ap.log$ID[i],"_" , Ap.log$REPL[i],".png", sep=""), width=8, height=8, units="in", res=400)
      par(mfrow=c(2,2))

      plot(Ap$Lambda, Ap$ODc+Ap$ODref, xlab="Wavelength", ylab="Optical Depth", ylim=c(min(Ap$ODref, na.rm=T), max(Ap$ODc, na.rm=T)), pch=19, main=basename)
      points(Ap$Lambda, Ap$ODref, col=2, pch=19)
      points(Ap$Lambda, Ap$ODc, col=3, pch=19)
      legend("topright", c("OD measured", "OD blank", "OD corrected"), col=c(1,2,3), pch=19)

      plot(Ap$Lambda, Ap$Ap_Stramski, xlab="Wavelength", ylab="Absorption (/m)", pch=19)

      #### Now process Anap.
      basename =paste(Ap.log$ID[i],"_" , Ap.log$REPL[i] ,"_NAp", sep="")
      print(paste("Process: ", basename))

      # Lectures des fichiers
      filen=paste(path.csv,"/",basename,".Sample.Raw.csv", sep="")
      if (file.exists(filen)) {
        sample = read.LAMBDA850(filen)
      } else {
        print(paste("File ", filen, "does not exists"))
        print("STOP PROCESSING")
        return(0)
      }

      filen=paste(path.blank,"/",Ap.log$BLANK.FILE[i],"_NAp.Sample.Raw.csv", sep="")
      if (file.exists(filen)) {
        blank = read.LAMBDA850(filen)
      } else {
        print(paste("File ", filen, "does not exists"))
        print("STOP PROCESSING")
        return(0)
      }

      # Calcul de l'absorption des  particules non-algales

      Ap = process.Ap(sample, blank, Ap.log$FAREA[i], Ap.log$VOL[i],
                      Ap.log$ID[i], Ap.log$STATION[i], "NAp",
                      Ap.log$REPL[i], Ap.log$DEPTH[i], Ap.log$DATE[i])

      # Sauvegarde des données sous format RData
       save(Ap, file=paste(path.out,"/",basename,".RData", sep=""))

      plot(Ap$Lambda, Ap$ODc+Ap$ODref, xlab="Wavelength", ylab="Optical Depth", ylim=c(min(Ap$ODref, na.rm=T), max(Ap$ODc, na.rm=T)), pch=19, main=basename)
      points(Ap$Lambda, Ap$ODref, col=2, pch=19)
      points(Ap$Lambda, Ap$ODc, col=3, pch=19)
      legend("topright", c("OD measured", "OD blank", "OD corrected"), col=c(1,2,3), pch=19)

      plot(Ap$Lambda, Ap$Ap_Stramski, xlab="Wavelength", ylab="Absorption (/m)", pch=19)
      dev.off()

    } else {
      print(paste("Skip file : ", Ap.log$ID[i]))
    }


  }
}
