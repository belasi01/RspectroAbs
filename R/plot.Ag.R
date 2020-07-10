#' Plot CDOM spectra
#'
#' Plot the CDOM spectra stored in an Ag list returned by
#' \code{\link{process.Ag}}.
#'
#' @param path.png is the path where the png file will be saved
#' @param Ag is a list return by \code{\link{process.Ag}}.
#' @param  PNG is a logical parameter indicating whether or not
#' the plots are save in a PNG file. Default is TRUE.
#'
#' @details Four plots are produced with
#' the same three spectra but with different z-axis and x-axis.
#' The three spectra are the raw (black), the corrected spectra
#' assuming NULL point at 690 nm (red) and the fitted spectra
#' on the 350 to 500 nm range (green).
#'
#' @seealso  \code{\link{process.Ag}}
#'
#' @examples
#' setwd(path.package("RspectroAbs"))
#' sample = read.LAMBDA850("data/433423.Sample.Raw.csv1")
#' Ag = process.Ag(sample, ID=433423,
#'                Station="L3_20.5", Depth=99,pathlength=0.1)
#' plot.Ag("./",Ag, PNG=FALSE)
#'
#' @author Simon Belanger
#' @export
#'
plot.Ag <-function(path.png, Ag, PNG=TRUE){

  ix350 = which(Ag$Lambda == 350)
  par(mfrow=c(2,2))
  par(mar=c(4,5,1,1))
  if (PNG) {
    png(file=paste(path.png,"/",Ag$ID,".png", sep=""),
        width=6, height=6, units="in", res=300)

    par(mfrow=c(2,2))
    par(mar=c(4,5,1,1))

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l", main=Ag$ID,lwd=3)
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")
    legend("topright", c("Ag", "Ag.offset", "Ag.fitted", "Ag.fitted.m"), col=c(1,2,3,4), lwd=c(3,3,3,3))

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l", log="y",
         lwd=3, ylim=c(0.001,max(Ag$Ag, na.rm=T)))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l",
         lwd=3, xlim=c(350,700), ylim=c(0,Ag$Ag[ix350]))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")

    if (Ag$red.offset > 0.011) {
      text(550, (Ag$Ag[ix350]*0.8), paste("Red Offset= ", signif(Ag$red.offset,3)), col=2)
      text(550, (Ag$Ag[ix350]*0.6), "WARNING: Too high!?", col=2)
    } else {
      text(550, (Ag$Ag[ix350]*0.8), paste("Red Offset= ", signif(Ag$red.offset,3)))
    }

    maxAg=Ag$Ag[ix350]
    if (maxAg < 0) maxAg=Ag$Ag.fitted[ix350]
    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)",
         type="l", log="y", lwd=3, xlim=c(350,700), ylim=c(0.004,maxAg))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")

    dev.off()
  } else {

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l", main=Ag$ID,lwd=3)
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")
    legend("topright", c("Ag", "Ag.offset", "Ag.fitted", "Ag.fitted.m"), col=c(1,2,3,4), lwd=c(3,3,3,3))

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l", log="y",
         lwd=3, ylim=c(0.001,max(Ag$Ag, na.rm=T)))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")

    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)", type="l",
         lwd=3, xlim=c(350,700), ylim=c(0,Ag$Ag[ix350]))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")
    if (Ag$red.offset > 0.011) {
      text(550, (Ag$Ag[ix350]*0.8), paste("Red Offset= ", signif(Ag$red.offset,3)), col=2)
      text(550, (Ag$Ag[ix350]*0.6), "WARNING: Too high!?", col=2)
    } else {
      text(550, (Ag$Ag[ix350]*0.8), paste("Red Offset= ", signif(Ag$red.offset,3)))
    }

    maxAg=Ag$Ag[ix350]
    if (maxAg < 0) maxAg=Ag$Ag.fitted[ix350]
    plot(Ag$Lambda, Ag$Ag, xlab="Wavelength", ylab="Absorption (/m)",
         type="l", log="y", lwd=3, xlim=c(350,700), ylim=c(0.004,maxAg))
    lines(Ag$Lambda, Ag$Ag.offset, lwd=3, col=2)
    lines(Ag$Lambda, Ag$Ag.fitted, lwd=3, col=3)
    lines(Ag$Lambda, Ag$Ag.fitted.m, lwd=3, col=4)
    lines(Ag$Lambda, rep(0.0115, length(Ag$Lambda)), lwd=2, col="grey")
  }
}


