#' Process CDOM spectra in batch mode
#'
#' This high level function reads an ASCII file containing the
#' list of samples to process. It calls \code{\link{process.Ag}}.
#'
#' @param log.file is the name of the ASCII file containing the
#' list of samples to process (see details below).
#' @param suffix is an optional character string for file names that
#' have a suffix after the ID in the file name (e.g. "_01_CDOM").
#' Default is "" for no suffix. (see details below).
#' @param data.path is the path where the csv folder is located (txt folder in the case of ULTRAPATH).
#' So the files must be stored in data.path/csv/.  Default is "./".
#' @param instrument is the type of instrument, i.e. "LAMBDA850" or "ULTRAPATH" or "LAMBDA35".
#' When ULTRAPATH is set, two files are red for each sample. inclusing the reference sample,
#' which is an artificial sea water solution.
#' The default is LAMBDA850.
#'
#' @details The most important thing to do before runing this programm is to prepare
#' the log.file (TAB delimated).
#' This file contains 6 fields : ID  Station  Depth	pathlength Ag.good DilutionFactor
#'
#' ID is the sample ID. It is usually the base name of the CSV file.
#'    for example, 407839 ID will have the following file name: 407839.Sample.Raw.csv.
#'    where the ".Sample.Raw.csv" was automatically added by the Lambda850 software.
#'    NOTE: Sometime the analyst added a suffix between the ID and ".Sample.Raw.csv".
#'    For example, "_Ag" may have been added yielding a file name like 407839_Ag.Sample.Raw.csv.
#'    In these case you have to set parameter suffix="_Ag"
#'
#' Station is the Station name. For example: "IML4", "L3_18", etc.
#'
#' Depth is the depth of the sample in meters.
#'
#' Date is the date of sampling
#'
#' pathlength is the pathlength of the cuvette in meters (e.g. 0.1 when using 10-cm cuvette).
#'
#' Ag.good is a binary field where 1 will process the sample while 0 will skip the sample.
#'
#' DilutionFactor Is a  factor to adjust the final Ag value if dilution was
#' performed in the lab (default=1).
#'
#' The program will automatically create two new folders in the data.path to store the results.
#' For each process sample, a png and a RData file is produce and stored in
#' data.path/png and data.path/RData, respectively.
#'
#' @seealso \code{\link{process.Ag}}
#'
#' @author Simon Belanger
#'
#' @export


run.process.Ag.batch <- function(log.file="Ag_log_TEMPLATE.dat",
                                 suffix="", data.path="./",
                                 instrument="LAMBDA850") {

  if (file.exists(data.path)){

    if (instrument == "LAMBDA850" | instrument == "LAMBDA35"){
      path.csv =  file.path(data.path,"csv")   #paste(data.path,"/csv/", sep="")
      path.asc =  file.path(data.path,"asc")
      if (file.exists(path.csv) | file.exists(path.asc)) {
        print("Data path exists")
      } else {
        print("NO csv/ or asc/ subdirectory in data.path")
        print("Check the path:")
        print(path.csv)
        print("STOP processing")
        return(0)
      }
    }

    if (instrument == "ULTRAPATH"){
        path.txt = file.path(data.path,"txt") # paste(data.path,"/txt/", sep="")
        if (file.exists(path.txt)) {
          print("Data path exists")
        } else {
          print("NO txt/ subdirectory in data.path")
          print("Check the path:")
          print(path.txt)
          print("STOP processing")
          return(0)
        }
    }



  } else {
    print("The data.path does not exits.")
    print("Put the data in data.path/csv/ (LAMBDA850, LAMBDA35) or data.path/txt (ULTRAPATH)")
    print("STOP processing")
    return(0)
  }

  # Check the output directories and Create them if they are not available.
  path.png = file.path(data.path,"png") #paste(data.path,"/png/", sep="")
  path.out = file.path(data.path,"RData") #paste(data.path,"/RData/", sep="")
  if (!file.exists(path.png)) dir.create(path.png)
  if (!file.exists(path.out)) dir.create(path.out)

  if (!file.exists(log.file)) {
    default.log.file =  file.path(Sys.getenv("RspectroAbs_DATA_DIR"), "Ag_log_TEMPLATE.dat")

    file.copy(from = default.log.file, to = log.file)
    cat("EDIT file", log.file, "and CUSTOMIZE IT\n")
    return(0)
  }


  # Lecture des informations dans un fichier texte
  Ag.log = fread(file=log.file)
   #                   colClasses = c("character", "character", "numeric","numeric","numeric","numeric"))
  names(Ag.log)<-str_to_upper(names(Ag.log))

  # Add the DilutionFactor if it is not included in the log file.
  if (is.null(Ag.log$DILUTIONFACTOR)) {
    Ag.log$DILUTIONFACTOR=1
  }

  nsample = length(Ag.log$ID)
  for (i in 1:nsample) {

    if (Ag.log$AG.GOOD[i] == 1) {
      basename = paste(Ag.log$ID[i],suffix, sep="")
      print(paste("Process: ", basename))

      # Lectures des fichiers
      if (instrument == "LAMBDA850") sample = read.LAMBDA850(paste(path.csv,"/",basename,".Sample.Raw.csv", sep=""))

      if (instrument == "ULTRAPATH") {
          s = read.ULTRAPATH(paste(path.txt,basename,"_s.txt", sep=""))
          r = read.ULTRAPATH(paste(path.txt,basename,"_r.txt", sep=""))
          sample = data.frame(s$wl, (s$OD - r$OD))
          names(sample) <- c("wl","OD")
      }

      if (instrument == "LAMBDA35") {
        ### Check whether the extension is csv or asc
        filen <- list.files(path = path.csv, pattern = basename)
        sample = read.LAMBDA35(paste(path.csv,"/",filen,sep=""))

      }

      # Convert OD to absorption and compute various index.
      Ag = process.Ag(sample,
                      Ag.log$ID[i],
                      Ag.log$STATION[i],
                      Ag.log$DEPTH[i],
                      Ag.log$DATE[i],
                      Ag.log$PATHLENGTH[i],
                      Ag.log$DILUTIONFACTOR[i])

      save(Ag, file=paste(path.out,"/",Ag.log$ID[i],".RData", sep=""))

      plot.Ag(path.png, Ag)

    } else {
      print(paste("Skip file: ", Ag.log$ID[i]))
    }
  }
}
