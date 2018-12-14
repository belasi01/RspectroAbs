generate.Ap.DB <- function(log.file="Ap_log_TEMPLATE.dat", data.path="./",
                           MISSION="YYY", Beta="Stramski") {

  # Lecture des informations dans un fichier texte
  #path = paste(data.path, "/RData/", sep="")
  if (file.exists(data.path)){
    path =paste(data.path,"/RData/", sep="")
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


  Ap.log = read.table(file=log.file, header=T)
  ix = which(Ap.log$process == 1)
  Samples = levels(droplevels(Ap.log$ID[ix]))
  nID = length(Samples)

  load(paste(path,Samples[1],".RData", sep=""))
  waves = A$Ap$Lambda
  Ap   = matrix(NA, ncol = nID, nrow=length(waves))
  Anap = Ap
  Aph  = Ap
  Snap = rep(NA, nID)
  Cnap = rep(NA, nID)
  for (i in 1:nID) {
    load(paste(path,Samples[i],".RData", sep=""))

    ix.st = which(Ap.log$ID == Samples[i])
    NAP.method = Ap.log$NAP.method[ix.st[1]]

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

  filen = paste(data.path,MISSION,".Ap.", Beta, ".RData", sep="")
  Ap.DB = list(Ap =Ap, waves=waves, Samples=Samples)
  save(Ap.DB, file=filen)

  filen = paste(data.path,MISSION,".Anap.", Beta, ".RData", sep="")
  Anap.DB = list(Anap =Anap, Snap=Snap, Cnap=Cnap, waves=waves, Samples=Samples)
  save(Anap.DB, file=filen)

  filen = paste(data.path,MISSION,".Aph.", Beta, ".RData",sep="")
  Aph.DB = list(Aph =Aph, waves=waves, Samples=Samples)
  save(Aph.DB, file=filen)

  # Save output in ASCII format

  Ap.df = as.data.frame(Ap)
  names(Ap.df) <- Samples
  Ap.df$waves = waves
  write.table(Ap.df, file=paste(data.path,MISSION,".Ap.", Beta, ".dat",sep=""), quote=F, row.names = F, sep=";")


  Anap.df = as.data.frame(Anap)
  names(Anap.df) <- Samples
  Anap.df$waves = waves
  write.table(Anap.df, file=paste(data.path,MISSION,".Anap.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")

  Aph.df = as.data.frame(Aph)
  names(Aph.df) <- Samples
  Aph.df$waves = waves
  write.table(Aph.df, file=paste(data.path,MISSION,".Aph.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")

  S.df = data.frame(Samples, Snap, Cnap)
  write.table(S.df, file=paste(data.path,MISSION,".Snap.", Beta, ".dat", sep=""), quote=F, row.names = F, sep=";")


  # plot all Ap, Anap and Aphy

  png(paste(data.path,MISSION,".Ap.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
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

  png(paste(data.path,MISSION,".Anap.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
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

  png(paste(data.path,MISSION,".Aph.", Beta, ".png",sep=""), res=300, height = 6, width = 8, units = "in")
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
  for (i in 2:length(Samples)) lines(waves, Aph[,i]/Aph[ix443,i],col=8)
  lines(waves, mean.Aph/mean.Aph[ix443], lwd=2)
  dev.off()


}
