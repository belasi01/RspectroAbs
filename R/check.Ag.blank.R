#' Plot a blank spectrum for diagnostic
#'
#'   Check a blank spectrum made during CDOM measurement with Lambda-850 spectrophotometer for diagnostic
#' @inheritParams read.LAMBDA850
#' @param PNG  is a logical parameter indicating whether or not
#' the plots are save in a PNG file. Default is TRUE.
#'
#' @return It returns a data frame with 2 columns for wavelength (wl)
#'and Optical Depth (OD). It creates a PNG file with two plots helping
#'to check the quality of the baseline.
#'
#' @examples
#' localpath = getwd()
#' setwd(path.package("RspectroAbs"))
#' nano = check.Ag.blank("data/nano_2.Sample.Raw.csv1", PNG=FALSE)
#' setwd(localpath)
#' @author Simon Bélanger
#'
#'
#' @export

check.Ag.blank <- function(filen, PNG=TRUE){

  nano = read.LAMBDA850(filen)
  ix350 = which.min(abs(nano$wl - 350))
  ix700 = which.min(abs(nano$wl - 700))

  mean.noise = mean(abs(nano$OD[ix350:ix700]))

  par(mar=c(4,5,0,1))
  if (PNG) {
    png(file=paste(filen,".png", sep=""), width=6, height=6, units="in", res=400)

    par(mfrow=c(2,1))
    plot(nano$wl, nano$OD, ylab="Raw Absorbance or Optical Depth",
         xlab="Wavelength", type="l", lwd=2)
    lines(nano$wl, rep(0.0005, length(nano$wl)), col=2, lwd=2)
    lines(nano$wl, rep(-0.0005, length(nano$wl)), col=2, lwd=2)

    plot(nano$wl, nano$OD, ylab="Raw Absorbance or Optical Depth",
         xlab="Wavelength", type="l", lwd=2,
         ylim=c(-0.001, 0.001))
    lines(nano$wl, rep(0.0005, length(nano$wl)), col=2, lwd=2)
    lines(nano$wl, rep(-0.0005, length(nano$wl)), col=2, lwd=2)

    if (mean.noise < 0.0005) {
      text(450, 0.0007, paste("mean noise (350-700):", signif(mean.noise, 4)))
    } else {
      text(450, 0.0007, paste("mean noise (350-700):", signif(mean.noise, 4)), col="red")
      text(450, 0, "TOO HIGH!!!")
    }

    dev.off()

  } else {

    par(mfrow=c(2,1))
    plot(nano$wl, nano$OD, ylab="Raw Absorbance or Optical Depth",
         xlab="Wavelength", type="l", lwd=2)
    lines(nano$wl, rep(0.0005, length(nano$wl)), col=2, lwd=2)
    lines(nano$wl, rep(-0.0005, length(nano$wl)), col=2, lwd=2)

    plot(nano$wl, nano$OD, ylab="Raw Absorbance or Optical Depth",
         xlab="Wavelength", type="l", lwd=2,
         ylim=c(-0.001, 0.001))
    lines(nano$wl, rep(0.0005, length(nano$wl)), col=2, lwd=2)
    lines(nano$wl, rep(-0.0005, length(nano$wl)), col=2, lwd=2)

    if (mean.noise < 0.0005) {
      text(450, 0.0007, paste("mean noise (350-700):", signif(mean.noise, 4)))
    } else {
      text(450, 0.0007, paste("mean noise (350-700):", signif(mean.noise, 4)), col="red")
      text(450, 0, "TOO HIGH!!!", col=2)
    }
  }

  return(nano)
}
