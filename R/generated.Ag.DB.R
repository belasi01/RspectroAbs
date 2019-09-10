#' Assemble the CDOM absorption data into a single data base
#'
#' This function assembles the CDOM data in a single data base and
#' generate 1) one RData file, 2) two ASCII files and 3) one plot
#' presenting all CDOM spectra.
#'
#' @param log.file is the name of the ASCII file containing the
#' list of ID to process (see details below).
#' @param data.path is the path where the RData folder is located.
#' So the files must be stored in data.path/RData.  Default is "./".
#' @param MISSION is a character string that will be used to name
#' the output files. The default is "YYY".
#' (i.e. MISSION.Ag.RData; MISSION.Ag.dat;
#' MISSION.Fitted.params.dat; MISSION.Ag.png)
#'
#' @return the function returns a list containing the following fields:
#'   waves, ID, Station, Depth,Ag.raw, Ag.offset,S275_295,
#'   S350_400, S350_500, Sr, K, a440.
#'
#'   This list (Ag.DB) is save in MISSION.Ag.RData.
#'   The Ag.offset spectra are saved in ASCII format in MISSION.Ag.dat,
#'   while the fitting parameters are saved in MISSION.Fitted.params.dat.
#'   Finally, a PNG file showing all CDOM spectra ise created.
#'
#' @seealso \code{\link{process.Ag}}, \code{\link{run.process.Ag.batch}}
#'
#' @author Simon Bélanger
#'
#' @export
#'
generate.Ag.DB <- function(log.file="Ag_log_TEMPLATE.dat",
                           data.path="./",
                           MISSION="YYY") {

  # Lecture des informations dans un fichier texte
  #path = paste(data.path, "/RData/", sep="")
  if (file.exists(data.path)){
    #path =paste(data.path,"/RData", sep="")
    path = file.path(data.path, "RData")

    if (file.exists(path)) {
      print("Data path exists")
    } else {
      print("The data path does not exists!")
      print("Check the path:")
      print(path)
      print("STOP processing")
      return(0)
    }
  } else {
    print("The data.path does not exits.")
    print("Put the data in data.path/RData/")
    print("STOP processing")
    return(0)
  }

  if (!file.exists(log.file)) {
    print("The log.file does not exits.")
    print("STOP processing")
    return(0)
  }


  Ag.log = read.table(file=log.file, header=T,  sep="\t")
<<<<<<< HEAD
  ix = which(Ag.log$Ag.good == 1)
  ID = Ag.log$ID[ix]
  nID = length(ID)

  print(paste("Number of ID is", nID))

=======
  names(Ag.log)<-str_to_upper(names(Ag.log))

  ix = which(Ag.log$AG.GOOD == 1)
  ID = Ag.log$ID[ix]
  nID = length(ID)

  print(paste("Number of ID is", nID))

>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81
  load(paste(path,"/",ID[1],".RData", sep=""))
  #load(file.path(path, ID[1], ".RData"))
  waves = Ag$Lambda
  ix350 = which(waves == 350)
  Ag.raw   = matrix(NA, ncol = nID, nrow=length(waves))
  Ag.offset = matrix(NA, ncol = nID, nrow=length(waves))
  S275_295 = rep(NA, nID)
  S350_400 = rep(NA, nID)
  S350_500 = rep(NA, nID)
  Sr = rep(NA, nID)
  a440 = rep(NA, nID)
  K = rep(NA, nID)
  Station = rep(NA, nID)
  Depth = rep(NA, nID)
  Date = rep(NA, nID)

  for (i in 1:nID) {
    load(paste(path,"/", ID[i],".RData", sep=""))

    #load(file.path(path, ID[i], ".RData"))
    Ag.raw[,i] = Ag$Ag
    Ag.offset[,i] = Ag$Ag.offset
    S275_295[i] = Ag$S275_295
    S350_400[i] = Ag$S350_400
    S350_500[i] = Ag$S350_500
    Sr[i] = Ag$Sr
    a440[i] = Ag$a440
    K[i] = Ag$K
    Station[i] = as.character(Ag$Station)
    Depth[i] = Ag$Depth
    Date[i]  = Ag$Date
  }

  # Save output in RData format

  filen = paste(data.path, "/", MISSION,".Ag.RData", sep="")
  Ag.DB = list(waves=waves, ID=ID,
<<<<<<< HEAD
               Station = Station, Depth=Depth,
=======
               Station = Station, Depth=Depth, Date=Date,
>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81
               Ag.raw=Ag.raw, Ag.offset=Ag.offset,
               S275_295=S275_295,
               S350_400=S350_400,
               S350_500=S350_500,
               Sr = Sr,
               K = K,
               a440 = a440)
  save(Ag.DB, file=filen)

  # Save output in ASCII format

  Ag.df = as.data.frame(Ag.offset)
  names(Ag.df) <- ID
  Ag.df$waves = waves
  Ag.df <-rbind(Ag.df, c(Station,NA))
  Ag.df <-rbind(Ag.df,c(Date,NA))
  Ag.df <-rbind(Ag.df,c(Depth,NA))
  write.table(Ag.df, file=paste(data.path,"/",MISSION,".Ag.dat",sep=""), quote=F, row.names = F, sep=";")

<<<<<<< HEAD
  Fitted.df = data.frame(ID, Station, Depth, S275_295, S350_400, S350_500, Sr, K, a440)
=======
  Fitted.df = data.frame(ID, Station, Depth, Date, S275_295, S350_400, S350_500, Sr, K, a440)
>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81
  write.table(Fitted.df, file=paste(data.path,"/",MISSION,".Fitted.params.dat", sep=""), quote=F, row.names = F, sep=";")


  # plot all Ag

  png(paste(data.path,"/",MISSION,".Ag.png",sep=""), res=300, height = 6, width = 8, units = "in")
  plot(waves, Ag.offset[,1], xlim=c(300,700), ylim=c(0,max(Ag.offset[ix350,],na.rm=T)), type="l",
       ylab=expression(paste(a[g],(lambda),(m^-1))), xlab=expression(lambda), col=8,
       main=paste(MISSION, ": CDOM absorption"))
  for (i in 2:nID) lines(waves, Ag.offset[,i],col=8)
  mean.Ag = apply(Ag.offset, 1, mean, na.rm=T)
  lines(waves, mean.Ag, lwd=2)
  sd.Ag = apply(Ag.offset, 1, sd, na.rm=T)
  lines(waves, (mean.Ag-sd.Ag), lwd=2, lty=2)
  lines(waves, (mean.Ag+sd.Ag), lwd=2, lty=2)
  dev.off()

  return(Ag.DB)

}
