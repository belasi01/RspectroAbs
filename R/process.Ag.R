#'  Process raw optical depth to CDOM spectra and analyse the spectral
#'  shape using \pkg{cdom} package functionalities.
#'
#'  Spectrophotometer measures opttical density (depth) (OD) or Absorbance (A),
#'  which are unit less. This program converts OD into absorption coefficent
#'  in per meter. Then a series of spectral analyses is performed.
#'
#'
#'@param sample is a data frame return by \code{\link{read.LAMBDA850}} or
#'\code{\link{read.ULTRAPATH}}. It contains two  2 columns for wavelength (wl)
#'and Optical Depth (OD).
#'@param ID is the sample ID.
#'@param Station is the station code (optional).
#'@param Depth is the depth of the sample in meters (optional).
#'@param pathlength is the pathlength of the cuvette in meters.
#'@param DilutionFactor Is a multiplication factor to adjust the final Ag value if dilution was
#' performed in the lab (default=1).
#'
#'@details  The processing steps includes:
#'
#'1) Ag = 2.303*OD/pathlenght.
#'
#'2) Apply a null correction assuming Ag(690+/-5) = 0.
#'   If a(500) is negative after the applicatioc of the NULL correction, then
#'   the minimum a values between 500 and 690 is assume for the null correction.
#'   So Ag.offset = Ag - NULL.POINT
#'
#'3) Fit an exponential model to Ag.offset for the 350 to 500 nm range:
#'   Ag.fitted = Ag(440) exp(-S*(440-lambda)) + K.
#'   This is done using \code{\link{cdom_fit_exponential}}.
#'
#'4) Compute spectral slope for the 275-295 and 350-400 ranges.
#'   Compute slope ratio using  \code{\link{cdom_slope_ratio}}.
#'
#'5) Compute spectral curvature using \code{\link{cdom_spectral_curve}}.
#'
#'@return A list with all the computed parameters decribed above is return.
#'
#'@seealso The \pkg{cdom} package for more details on the CDOM spectral analysis.

#'@author Simon Belanger
#'@export

process.Ag <- function (sample, ID, Station, Depth, pathlength, DilutionFactor=1) {

  ix690 = which(sample$wl == 690)
  ix500 = which(sample$wl == 500)

  Ag = 2.303 * sample$OD / pathlength * DilutionFactor

  # Apply null correction in the red
  red.offset = mean(Ag[(ix690-5):(ix690+5)])
  Ag.offset = Ag  - red.offset
  Ag.offset[Ag.offset<0]=0

  if (Ag.offset[ix500] == 0) {
    red.offset = min(Ag[ix500:ix690])
    Ag.offset = Ag  - red.offset
  }

  # Fit an exponential model at different wavelengths range
  df = as.data.frame(cbind(sample$wl, Ag.offset))
  names(df) <- c("Wavelength", "Ag")

  # 350 to 500 nm spectral range
  S350_500 = cdom_fit_exponential(sample$wl, Ag.offset,440, 350,500)$params[1,2]
  K = cdom_fit_exponential(sample$wl, Ag.offset,440, 350,500)$params[2,2]
  a440 =   cdom_fit_exponential(sample$wl, Ag.offset,440, 350,500)$params[3,2]
  Ag.fitted = a440 * exp(S350_500*(440-sample$wl)) + K

  # 350 to 400 spectral range
   S350_400 =cdom_fit_exponential(sample$wl, Ag.offset,400, 350,400)$params[1,2]


  # 275 to 295 spectral range
  S275_295 = cdom_fit_exponential(sample$wl, Ag.offset,300, 275,295)$params[1,2]

  Sr = cdom_slope_ratio(sample$wl, Ag.offset)

  Scurv = cdom_spectral_curve(sample$wl, Ag.offset)

  return(list(ID = ID,
              Station = Station,
              Depth = Depth,
              Lambda = sample$wl,
              OD = sample$OD,
              Ag = Ag,
              Ag.offset = Ag.offset,
              red.offset = red.offset,
              Ag.fitted=Ag.fitted,
              S275_295=S275_295,
              S350_400=S350_400,
              S350_500=S350_500,
              Sr = Sr,
              K = K,
              a440 = a440,
              Scurv = Scurv,
              DilutionFactor = DilutionFactor
              ))

}

