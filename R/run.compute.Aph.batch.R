#' Compute phytoplankton absorption spectrum in batch mode
#'
#' This is the third step of the Ap/Aph processing. It calls the \code{\link{compute.Aph}}
#' for each samples found in the Log file provided as input.
#'
#' @param log.file is the name of the ASCII file containing the list of
#' samples to process.
#' @param data.path is the path where the csv folder is located.
#' So the files must be stored in data.path/csv/.  Default is "./".
#' @param PLOT is a logical parameter indicating whether the plot are generated
#' in the RStudio interface. Default is FALSE.
#'
#'@return It produces plots with the spectra returned by \code{\link{compute.Aph}}
#'  and save the data in a RData file.
#'
#' @seealso \code{\link{compute.Aph}}
#' @author Simon Belanger
#'@export
run.compute.Aph.batch <- function(log.file="Ap_log_TEMPLATE.dat", data.path="./", PLOT=FALSE) {


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
  ix = which(Ap.log$PROCESS == 1)
  IDs = levels(droplevels(Ap.log$ID[ix]))
  nID = length(IDs)

  for (i in 1:nID) {
    print(paste("Process: ", IDs[i]))

    if (file.exists(file=paste(path,IDs[i],"_NAp.RData", sep=""))){
      load(file=paste(path,IDs[i],"_NAp.RData", sep=""))
      NAp <- Ap # Renomme la liste
      load(file=paste(path,IDs[i],"_Ap.RData", sep=""))

      Aph.RG <- compute.Aph(Ap$Lambda, Ap$Ap.RG.mean, NAp$Ap.RG.mean)
      Aph.Stramski <- compute.Aph(Ap$Lambda, Ap$Ap.Stramski.mean, NAp$Ap.Stramski.mean)

      png(file=paste(path.png,IDs[i],".png", sep=""), width = 7, height = 5, units = "in", res=300)
      plot(Ap$Lambda, Ap$Ap.Stramski.mean, lwd=5, type="l",
           ylim=c(0,max(Ap$Ap.Stramski.mean, na.rm=T)), xlim=c(300,750),
           xlab="Wavelength", ylab="Absorption (/m)",
           main=IDs[i], cex.lab=1.2, cex.axis=1.5, cex.main=1.5)
      lines(Ap$Lambda, NAp$Ap.Stramski.mean, col=1, lty=2, lwd=3)
      lines(Ap$Lambda, Aph.Stramski$Aph, col=2, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.offset, col=2,  lty=2, lwd=3)
      lines(Ap$Lambda, Aph.Stramski$Aph.fitted, col=3, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.fitted, col=3, lty=2, lwd=3)
      lines(Ap$Lambda, Aph.Stramski$Aph.BS90, col=4, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.BS90, col=4, lty=2,lwd=3)
      lines(Ap$Lambda, Aph.Stramski$Aph.BS90.v2, col=5, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.BS90.v2, col=5, lty=2,lwd=3)
      legend("topright", c("Ap", "Anap", "Aph", "Anap.offset", "Aph.fitted", "Anap.fitted", "Aph.BS90", "Anap.BS90","Aph.BS90.v2", "Anap.BS90.v2"),
             col=c(1,1,2,2,3,3,4,4,5,5),lwd=c(5,3,5,3,5,3,5,3,5,4), lty=c(1,2,1,2,1,2,1,2,1,2))
      dev.off()

      if (PLOT) {
        plot(Ap$Lambda, Ap$Ap.Stramski.mean, lwd=5, type="l",
             xlab="Wavelength", ylab="Absorption (/m)",
             ylim=c(0,max(Ap$Ap.Stramski.mean, na.rm=T)), xlim=c(300,750),
             main=IDs[i], cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
        lines(Ap$Lambda, NAp$Ap.Stramski.mean, col=1, lty=2, lwd=3)
        lines(Ap$Lambda, Aph.Stramski$Aph, col=2, lwd=5)
        lines(Ap$Lambda, Aph.Stramski$Anap.offset, col=2,  lty=2, lwd=3)
        lines(Ap$Lambda, Aph.Stramski$Aph.fitted, col=3, lwd=5)
        lines(Ap$Lambda, Aph.Stramski$Anap.fitted, col=3, lty=2, lwd=3)
        lines(Ap$Lambda, Aph.Stramski$Aph.BS90.v2, col=4, lwd=5)
        lines(Ap$Lambda, Aph.Stramski$Anap.BS90.v2, col=4, lty=2,lwd=3)
        legend("topright", c("Ap", "Anap", "Aph", "Anap.offset", "Aph.fitted", "Anap.fitted", "Aph.BS90.v2", "Anap.BS90.v2"),
               col=c(1,1,2,2,3,3,4,4),lwd=c(5,3,5,3,5,3,5,3), lty=c(1,2,1,2,1,2,1,2))

      }

      A = list(Ap = Ap, Anap = NAp, Aph.RG = Aph.RG, Aph.Stramski=Aph.Stramski)
      save(A,file=paste(path.out,IDs[i],".RData", sep=""))

    } else {
      print("No NAP file for this station")
      load(file=paste(path,IDs[i],"_Ap.RData", sep="")) #### added June 1 2018
      Aph.RG <- compute.Aph(Ap$Lambda, Ap$Ap.RG.mean, NA)
      Aph.Stramski <- compute.Aph(Ap$Lambda, Ap$Ap.Stramski.mean, NA)


      png(file=paste(path.png,IDs[i],".png", sep=""),  width = 7, height = 5, units = "in", res=300)
      plot(Ap$Lambda, Ap$Ap.Stramski.mean, lwd=5, type="l",
           xlab="Wavelength", ylab="Absorption (/m)",
           main=IDs[i], cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
      lines(Ap$Lambda, Aph.Stramski$Aph.BS90, col=4, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.BS90, col=4, lty=2,lwd=3)
      lines(Ap$Lambda, Aph.Stramski$Aph.BS90.v2, col=5, lwd=5)
      lines(Ap$Lambda, Aph.Stramski$Anap.BS90.v2, col=5, lty=2,lwd=3)
      legend("topright", c("Ap", "Aph.BS90","Anap.BS90","Aph.BS90_v2","Anap.BS90_v2" ),
             col=c(1,4,4,5,5),lwd=c(5,5,5,5,5), lty=c(1,1,2,1,2))
      dev.off()

      if (PLOT) {
        plot(Ap$Lambda, Ap$Ap.Stramski.mean, lwd=5, type="l",
             xlab="Wavelength", ylab="Absorption (/m)",
             main=IDs[i], cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
        lines(Ap$Lambda, Aph.Stramski$Aph.BS90, col=4, lwd=5)
        lines(Ap$Lambda, Aph.Stramski$Anap.BS90, col=7, lwd=5)
        legend("topright", c("Ap", "Aph.BS90","Anap.BS90"),
               col=c(1,4,7),lwd=c(5,5,5), lty=c(1,1,2,1,2))
      }


      A = list(Ap = Ap, Anap = list(), Aph.RG = Aph.RG, Aph.Stramski=Aph.Stramski)
      save(A,file=paste(path.out,IDs[i],".RData", sep=""))

    }
  }
}
