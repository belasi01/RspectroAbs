#Fuction wich match all Cops Station with avalible cdom and particular measured absorption
#Dependancies: function compute.discrete.aTOT.for.COPS from Rspectro abs library

Batch.A.COPS <- function(copsL2= "", cdomdir= "", partdir= ""){

  copsL2 <- "/home/raphael/Data/Chone/Cops/L2/"
  cdomdir <- "/home/raphael/Data/Chone/aLab/CDOM/RData/"
  partdir <- "/home/raphael/Data/Chone/aLab/PARTICULATE/RData/"

  copsL2 <- paste(copsL2, list.files(copsL2, pattern = "\\d\\d\\d[a-z]?"), sep = "")
  cops <- paste(copsL2, "COPS", sep = "/")

  cdom <- list.files(cdomdir, pattern = "\\d\\d\\d[a-z]?")
  cdom <- (paste(cdomdir, cdom, sep = ""))

  part <- list.files(partdir, pattern = "\\d\\d\\d[a-z]?[.]")
  part <- paste(partdir, part, sep = "")

  Dcops <- data.frame(cops, row.names = substring(cops, regexpr("Station", cops, fixed = T )+ 7, regexpr("/COPS", cops, fixed = T ) -1))
  Dcdom <- data.frame(cdom, row.names = substring(cdom, regexpr("\\d\\d\\d[a-z]?", cdom), regexpr(".RData", cdom, fixed = T ) -1))
  Dpart <- data.frame(part, row.names = substring(part, regexpr("\\d\\d\\d[a-z]?", part), regexpr(".RData", part, fixed = T ) -1))

  #Create a data frame with unique matches of each station as row.names
  #when all inputs are avalible (if not, station is remove by merge "row.names")

  l <- list(Dcops, Dcdom, Dpart)
  dat <- l[[1]]
  for(i in 2:length(l)){
    dat <- merge(dat, l[[i]], by= "row.names", all.x= F, all.y= F)
    rownames(dat) <- dat$Row.names
    dat$Row.names <- NULL
  }

  for(i in 1:length(rownames(dat))){
    print(as.character(dat$cops[i]))
    print(as.character(dat$cdom[i]))
    print(as.character(dat$part[i]))

    compute.discrete.aTOT.for.COPS(cops.path = as.character(dat$cops[i]),
                                   ag.RData = as.character(dat$cdom[i]),
                                   ap.RData = as.character(dat$part[i]))

  }
}
