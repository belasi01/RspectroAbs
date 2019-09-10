#' Assemble the Ap absorption data into a single data base
#'
#' This function assembles the Ap data in a single data base and
#' generate 1) three RData file, 2) three ASCII files and 3) three plot
#' presenting all Ap, Anap and Aphy spectra.
#'
#' @param log.file is the name of the ASCII file containing the
#' list of ID to process (see details below).
#' @param data.path is the path where the RData folder is located.
#' So the files must be stored in data.path/RData.  Default is "./".
#' @param MISSION is a character string that will be used to name
#' the output files. The default is "YYY".
#' (i.e. MISSION.Ap.RData; MISSION.Ap.dat;
#' MISSION.Fitted.params.dat; MISSION.Ap.png)
#' @param Beta if the amplification factor your want to select. Default is Stramski.
#'
#' @return
#'
#' @seealso \code{\link{process.Ap}}, \code{\link{run.process.Ap.batch}}
#'
#' @author Simon Bélanger
#'
#' @export

generate.Ap.DB <- function(log.file="Ap_log_TEMPLATE.dat", data.path="./",
                           MISSION="YYY", Beta="Stramski") {

  # Lecture des informations dans un fichier texte
  #path = paste(data.path, "/RData/", sep="")
  if (file.exists(data.path)){

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
    print("Put the data in data.path/RData")
    print("STOP processing")
    return(0)
  }

  if (!file.exists(log.file)) {
    print("The log.file does not exits.")
    print("STOP processing")
    return(0)
  }


  Ap.log = read.table(file=log.file, header=T, sep="\t")
  names(Ap.log)<-str_to_upper(names(Ap.log))

  ix = which(Ap.log$PROCESS == 1)
  ID = levels(droplevels(Ap.log$ID[ix]))
  nID = length(ID)

  load(paste(path, "/", ID[1],".RData", sep=""))
  waves = A$Ap$Lambda
  Ap   = matrix(NA, ncol = nID, nrow=length(waves))
  Anap = Ap
  Aph  = Ap
  Snap = rep(NA, nID)
  Cnap = rep(NA, nID)
  Station = rep(NA, nID)
  Date = rep(NA, nID)
  Depth = rep(NA, nID)

  for (i in 1:nID) {
    load(paste(path, "/", ID[i],".RData", sep=""))

<<<<<<< HEAD
    ix.st = which(Ap.log$ID == ID[i])
    NAP.method = Ap.log$NAP.METHOD[ix.st[1]]
=======
    ix.ID = which(Ap.log$ID == ID[i])
    NAP.method = Ap.log$NAP.METHOD[ix.ID[1]]
    Station[i] = as.character(A$Ap$Station)
    Date[i]    = A$Ap$Date
    Depth[i]   = A$Ap$Depth
>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81

    if (Beta == "Stramski") {
      Ap[,i] = A$Ap$Ap.Stramski.mean

      if (NAP.method == "Measured") {
        Anap[,i] = A$Aph.Stramski$Anap.offset
        Aph[,i] = A$Aph.Stramski$Aph
        Snap[i] = A$Aph.Stramski$Snap.fitted
        Cnap[i] = A$Aph.Stramski$C.fitted
      }
      if (NAP.method == "Fitted") {
        Anap[,i] = A$Aph.Stramski$Anap.fitted
        Aph[,i] = A$Aph.Stramski$Aph.fitted
        Snap[i] = A$Aph.Stramski$Snap.fitted
        Cnap[i] = A$Aph.Stramski$C.fitted
      }
      if (NAP.method == "BS90_1") {
        Anap[,i] = A$Aph.Stramski$Anap.BS90
        Aph[,i] = A$Aph.Stramski$Aph.BS90
        Snap[i] = A$Aph.Stramski$Snap.BS90
        Cnap[i] = NA
      }
      if (NAP.method == "BS90_2") {
        Anap[,i] = A$Aph.Stramski$Anap.BS90.v2
        Aph[,i] = A$Aph.Stramski$Aph.BS90.v2
        Snap[i] = A$Aph.Stramski$Snap.BS90.v2
        Cnap[i] = NA
      }
    }

    if (Beta == "RG") {
      Ap[,i] = A$Ap$Ap.RG.mean
      if (NAP.method == "Measured") {
        Anap[,i] = A$Aph.RG$Anap.offset
        Aph[,i] = A$Aph.RG$Aph
        Snap[i] = A$Aph.RG$Snap.fitted
        Cnap[i] = A$Aph.RG$C.fitted
      }
      if (NAP.method == "Fitted") {
        Anap[,i] = A$Aph.RG$Anap.fitted
        Aph[,i] = A$Aph.RG$Aph.fitted
        Snap[i] = A$Aph.RG$Snap.fitted
        Cnap[i] = A$Aph.RG$C.fitted
      }
      if (NAP.method == "BS90_1") {
        Anap[,i] = A$Aph.RG$Anap.BS90
        Aph[,i] = A$Aph.RG$Aph.BS90
        Snap[i] = A$Aph.RG$Snap.BS90
        Cnap[i] = NA
      }
      if (NAP.method == "BS90_2") {
        Anap[,i] = A$Aph.RG$Anap.BS90.v2
        Aph[,i] = A$Aph.RG$Aph.BS90.v2
        Snap[i] = A$Aph.RG$Snap.BS90.v2
        Cnap[i] = NA
      }
    }

  }

  # Save output in RData format

  filen = paste(data.path,"/", MISSION,".Ap.", Beta, ".RData", sep="")
<<<<<<< HEAD
  Ap.DB = list(Ap =Ap, waves=waves, ID=ID)
  save(Ap.DB, file=filen)

  filen = paste(data.path,"/", MISSION,".Anap.", Beta, ".RData", sep="")
  Anap.DB = list(Anap =Anap, Snap=Snap, Cnap=Cnap, waves=waves, ID=ID)
  save(Anap.DB, file=filen)

  filen = paste(data.path,"/", MISSION,".Aph.", Beta, ".RData",sep="")
  Aph.DB = list(Aph =Aph, waves=waves, ID=ID)
=======
  Ap.DB = list(Ap =Ap, waves=waves, ID=ID, Station=Station, Date=Date, Depth=Depth)
  save(Ap.DB, file=filen)

  filen = paste(data.path,"/", MISSION,".Anap.", Beta, ".RData", sep="")
  Anap.DB = list(Anap =Anap, Snap=Snap, Cnap=Cnap, waves=waves, ID=ID, Station=Station, Date=Date, Depth=Depth)
  save(Anap.DB, file=filen)

  filen = paste(data.path,"/", MISSION,".Aph.", Beta, ".RData",sep="")
  Aph.DB = list(Aph =Aph, waves=waves, ID=ID, Station=Station, Date=Date, Depth=Depth)

>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81
  save(Aph.DB, file=filen)

  # Save output in ASCII format

  Ap.df = as.data.frame(Ap)
  names(Ap.df) <- ID
  Ap.df$waves = waves
  Ap.df <-rbind(Ap.df, c(Station,NA))
  Ap.df <-rbind(Ap.df,c(Date,NA))
  Ap.df <-rbind(Ap.df,c(Depth,NA))
  write.table(Ap.df, file=paste(data.path,"/", MISSION,".Ap.", Beta, ".dat",sep=""), quote=F, row.names = F, sep=";")


  Anap.df = as.data.frame(Anap)
  names(Anap.df) <- ID
  Anap.df$waves = waves
  Anap.df <-rbind(Anap.df, c(Station,NA))
  Anap.df <-rbind(Anap.df,c(Date,NA))
  Anap.df <-rbind(Anap.df,c(Depth,NA))
  write.table(Anap.df, file=paste(data.path,"/", MISSION,".Anap.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")

  Aph.df = as.data.frame(Aph)
  names(Aph.df) <- ID
  Aph.df$waves = waves
  Aph.df <-rbind(Aph.df, c(Station,NA))
  Aph.df <-rbind(Aph.df,c(Date,NA))
  Aph.df <-rbind(Aph.df,c(Depth,NA))
  write.table(Aph.df, file=paste(data.path,"/", MISSION,".Aph.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")

<<<<<<< HEAD
  S.df = data.frame(ID, Snap, Cnap)
=======
  S.df = data.frame(ID, Snap, Cnap, Station, Depth, Date)

>>>>>>> a130dd12f99890769ad97cbb9eed85456fb0bd81
  write.table(S.df, file=paste(data.path,"/", MISSION,".Snap.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")


  # plot all Ap, Anap and Aphy

  png(paste(data.path,"/", MISSION,".Ap.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
  plot(waves, Ap[,1], xlim=c(300,700), ylim=c(0,max(Ap,na.rm=T)), type="l",
       ylab=expression(paste(a[p],(lambda),(m^-1))), xlab=expression(lambda), col=8,
       main=paste(MISSION, ": Particulate absorption"))
  for (i in 2:nID) lines(waves, Ap[,i],col=8)
  mean.Ap = apply(Ap, 1, mean, na.rm=T)
  lines(waves, mean.Ap, lwd=2)
  sd.Ap = apply(Ap, 1, sd, na.rm=T)
  lines(waves, (mean.Ap-sd.Ap), lwd=2, lty=2)
  lines(waves, (mean.Ap+sd.Ap), lwd=2, lty=2)
  dev.off()

  png(paste(data.path,"/", MISSION,".Anap.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
  plot(waves, Anap[,1], xlim=c(300,700), ylim=c(0,max(Anap,na.rm=T)), type="l",
       ylab=expression(paste(a[nap],(lambda),(m^-1))), xlab=expression(lambda), col=8,
       main=paste(MISSION, ": Non-algal absorption"))
  for (i in 2:nID) lines(waves, Anap[,i],col=8)
  mean.Anap = apply(Anap, 1, mean, na.rm=T)
  lines(waves, mean.Anap, lwd=2)
  sd.Anap = apply(Anap, 1, sd, na.rm=T)
  lines(waves, (mean.Anap-sd.Anap), lwd=2, lty=2)
  lines(waves, (mean.Anap+sd.Anap), lwd=2, lty=2)
  dev.off()

  png(paste(data.path,"/", MISSION,".Aph.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
  plot(waves, Aph[,1], xlim=c(300,700), ylim=c(0,max(Aph,na.rm=T)), type="l",
       ylab=expression(paste(a[phi],(lambda),(m^-1))), xlab=expression(lambda), col=8,
       main=paste(MISSION, ": Phytoplankton absorption"))
  for (i in 2:nID) lines(waves, Aph[,i],col=8)
  mean.Aph = apply(Aph, 1, mean, na.rm=T)
  lines(waves, mean.Aph, lwd=2)
  sd.Aph = apply(Aph, 1, sd, na.rm=T)
  lines(waves, (mean.Aph-sd.Aph), lwd=2, lty=2)
  lines(waves, (mean.Aph+sd.Aph), lwd=2, lty=2)
  dev.off()



  ix443 = which(waves == 443)

  png(paste(data.path,MISSION,".AphNORM.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
  plot(waves, Aph[,1]/Aph[ix443,1], xlim=c(300,700), ylim=c(0,2), type="l",
       ylab=expression(paste(a[phi],(lambda),"/", a[phi],(443))), xlab=expression(lambda), col=8,
       main=paste(MISSION, ": Normalized phytoplankton absorption"))
  for (i in 2:length(ID)) lines(waves, Aph[,i]/Aph[ix443,i],col=8)
  lines(waves, mean.Aph/mean.Aph[ix443], lwd=2)
  dev.off()


}
