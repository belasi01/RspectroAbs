#' Read CSV file from lambda 35
#'
#'@description Read CSV file from a Perkin Elmer Lambda-35 model,
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

read.LAMBDA35 <- function (filen) {

  ### check extension
  #if (str_detect(filen, ".csv")) df = read.table(file=filen, sep=",", skip=86)
  #if (str_detect(filen, ".asc")) df = read.table(file=filen, sep="\t", skip=86)
  df = fread(filen)

  names(df) <- c("wl", "OD")
  return(df)
}
