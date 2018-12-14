#' Read CSV file from Ultrapath spectrophotometer
#'
#'@description Read text file from the Ultrapath spectrophotometer.
#'
#'@param filen is the file name.
#'
#'@return It returns a data frame with 2 columns for wavelength (wl)
#'and Optical Depth (OD).
#'
#'@details The ultrapath data files have a header containing wavelength
#'specifications and than 16 optical depth spectra. The function extract the header information
#'and average the 16 spectra.
#'
#' @author Simon Bélanger
#' @export
#'
read.ULTRAPATH <- function (filen) {

  # extract header information
  #1[HEADER]
  #2Format	J&M ASCII Table
  #3Comment	200cm
  #4Date	02/10/2014 11:09:57 AM
  #5Order	Y (evenX)
  #6TechType	UV
  #7NumPoints	523
  #8FirstX	200.00
  #9LastX	722.00
  #10XType	NMetr
  #11YType	Absrb
  #12ZType	Secs
  #13NumSubFile	16
  #14Matrix	Z vs X
  #15[DATA]

  id = file(filen, "r")
  line = strsplit(readLines(con=id, n =1), "\t") # Reads the first header line
  nrec = 0
  line=c("Start","Reading")
  while (strsplit(unlist(line), " ")[[1]][1] != "[DATA]"){
    line = strsplit(readLines(con=id, n =1), "\t") # reads the time and depth for each records
    nrec <- nrec+1
   # print(line)

    if (line != "character(0)") {

      #  print(strsplit(unlist(line), " ")[[1]][1])
      if (strsplit(unlist(line), " ")[[1]][1] == "FirstX") StartWave = as.numeric(strsplit(unlist(line), " ")[[2]][1])
      if (strsplit(unlist(line), " ")[[1]][1] == "LastX") EndWave = as.numeric(strsplit(unlist(line), " ")[[2]][1])
    } else line=c("Skip", "line")
  }
  close(id)

  wl = StartWave:EndWave
  df = read.table(filen, skip=nrec+2)

  OD = apply(df, 1,mean)


  df <- data.frame(wl, OD)
  return(df)
}
