#' Read CSV file from lambda 850
#'
#'@description Read CSV file from a Perkin Elmer Lambda-850 model,
#'which is a dual-beam spectrophotometer.
#'
#'@param filen is the name of the CSV file.
#'
#'@return It returns a data frame with 2 columns for wavelength (wl)
#'and Optical Depth (OD).
#'
#'@details OD is unitless. It is equal to the log10(I0/I) where I0 is the
#'reference beam intensity and I the sample beam intensity.
#'
#' @author Simon Bélanger
#' @export

read.LAMBDA850 <- function (filen) {

  # Add a condition to detect the separator (i.e. "," or ";")
  #df = read.table(file=filen, sep=",", header=1)
  df = fread(file=filen)
  names(df) <- c("wl", "OD")
  return(df)
}
