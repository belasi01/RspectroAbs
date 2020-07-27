#' Compute the total absorption for COPS processing of LuZ profile
#'
#' Compute the total absorption for COPS instrument self-shadow
#' correction of LuZ. It is run only after a preliminary processing
#' of the spectrophotometric data using the \pkg{Rspectro} package.
#'
#' @param cops.path is the full path of the COPS data where the absorption.cops.dat file can be found
#' @param ag.RData is the full path and file name of the RData file contaning the CDOM absorption spectrum
#' @param ap.RData is the full path and file name of the RData file contaning the particle absorption spectrum
#'
#' @details
#' The program will open a RData file located in ../COPS/BIN folder to get
#' the wavelengths available on the COPS. It will edit the file
#' ../COPS/absorption.cops.dat and produce a PNG figure showing the spectral
#' absorption that will be used in the COPS processing for instrument self-shadow
#' correction.
#'
#' The pure water absorption is taken from a composite table
#' (Kou etal 1993, Pope and Fry 1997, etc).
#'
#' @return It returns a vector of total absorption.
#'
#' @author Simon Belanger
#' @export

compute.discrete.aTOT.for.COPS <- function (cops.path="./",
                                            ag.RData="ag.RData",
                                            ap.RData="ap.RData") {

  setwd(cops.path)

  abs.file="absorption.cops.dat"
  df = read.table(abs.file,sep=";")

  #Load wavelenghts from COPS
  cops.files = list.files("./BIN/", pattern = "*.RData")
  load(file=paste("./BIN/",cops.files[1], sep=""))
  lambda = cops$Ed0.waves

  #Load and get Ap at COPS wavelengths
  load(ap.RData)
  a.p.cops = spline(A$Ap$Lambda, A$Ap$Ap.Stramski.mean,  xout=lambda, method="natural")$y

  #Load and get Ag at COPS wavelengths
  load(ag.RData)
  a.g.cops = spline(Ag$Lambda, Ag$Ag.offset,  xout=lambda, method="natural")$y

  # get Pure water absorption at COPS wavelengths
  a.w.cops = Riops::spectral.aw(lambda)

  a.tot.cops = a.p.cops + a.g.cops + a.w.cops

# Plot and save results
png(filen = "absorption.cops.png", width = 5, height = 5, res=300, units = "in")
plot(lambda, a.tot.cops,
     xlab = "Wavelenght", ylab="Absorption", pch=19,
     ylim=c(0, max(a.tot.cops)))

lines(A$Ap$Lambda,A$Ap$Ap.Stramski.mean, col=2, lwd=3)
lines(300:800, Riops::spectral.aw(300:800), col=4, lwd=4)
lines(Ag$Lambda,Ag$Ag.offset, col=3, lwd=3)
dev.off()

# write results in file absorption.cops.dat
nfile = length(df$V1) - 1
a.mat = matrix(a.tot.cops, nrow=nfile, ncol=19, byrow=T)
df.a = rbind(lambda,a.mat)
df.final = as.data.frame(cbind(df$V1, as.data.frame(df.a)))

write.table(df.final, file=abs.file, quote=F, sep=";", col.names = F, row.names=F)

return(a.tot.cops)

}

