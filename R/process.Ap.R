#'  Process raw optical depth to Ap spectra using filter-pad technique
#'
#'  Spectrophotometer measures optical density (depth) (OD) or Absorbance (A),
#'  which are unit less. This program converts OD into absorption coefficent
#'  in per meter. Then a series of spectral analyses is performed.
#'
#'@param sample is a data frame return by \code{\link{read.LAMBDA850}} for the measured optical depth
#'of the filter containing the particles. It contains two  2 columns for wavelength (wl)
#'and Optical Depth (OD).
#'@param blank is a data frame return by \code{\link{read.LAMBDA850}} for the measured optical depth
#'of a blank filter. It contains two  2 columns for wavelength (wl)
#'and Optical Depth (OD).
#'@param Farea is the filter clearance area of particles in m^2. This field is mandatory.
#'@param Vol.ml is the volume filtered in ml. This field is mandatory.
#'@param SpecType is a string indicating either "Ap" for the total particle  or "Anap" for non-algal particles.
#'This field is mandatory.
#'@param ID is the sample ID.
#'@param Station is the station code.
#'@param Repl is a Capital Letter or number corresponding of the replicat ("A", "B",... or "1", etc)
#'@param Depth is the depth of the sample in meters.
#'
#'@return Returns a list containing metadata provided to the function as well as the vector of OD corrected for blank,
#'the OD of the blank filter and 3 estimation of particle absorption:
#'\itemize{
#'    \item{Ap_Stramski folowwing Stramski et al, App. Optics 2015}
#'    \item{Ap_RG following Rottgers and Genhke, App. Opt. (2012)}
#'    \item{Ap_4.5 using a constant beta factor of 4.5 (see Rottgers and Genhke, 2012)}
#'}
#
process.Ap <- function (sample, blank, Farea=NA, Vol.ml=NA,
                        SpecType=NA, ID, Station, Repl, Depth){
  Vol.m3 = Vol.ml/1e6
  ODc = sample$OD - blank$OD

  # Compute Ap using Stramski et al 2015
  ODf = 0.323 * (ODc) ^ (1.0867)
  Ap_Stramski = 2.303 * (ODf) * (Farea/Vol.m3)
  Ap_Stramski[is.na(Ap_Stramski)] = 0  # Replace NAs by 0

  # Compute Ap using Rottgers and Genhke, App. Opt. (2012)
  Beta = 6.475 * (ODc^2) -(6.474*ODc) + 4.765
  Ap_RG = 2.303 * (ODc) * (Farea/Vol.m3) * (1/Beta)

  Ap_4.5  = 2.303 * (ODc) * (Farea/Vol.m3) * (1/4.5)

  return(list(ID = ID,
              Station=Station,
              SpecType=SpecType,
              Repl = Repl,
              Depth = Depth,
              Vol = Vol.m3,
              Farea = Farea,
              Lambda = sample$wl,
              ODc = ODc,
              ODref =  blank$OD,
              Ap_Stramski=Ap_Stramski,
              Ap_RG=Ap_RG,
              Ap_4.5 = Ap_4.5))
}
