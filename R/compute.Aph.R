#' Compute phytoplankton absorption spectrum (Aph) from total particulate absorption (Ap)
#' and, if available, the non-algal particles absorption (Anap) spectra
#'
#'
#' @param waves is a vector of wavelengths
#' @param Ap is the total particulate absorption vector (mandatory)
#' @param Anap is the non-algal particles absorption vector. If Anap=NA
#' only the spectral deconvolution method of Bricaud and Stramski (1990) is applied.
#'
#' @return It returns a list with all the cumputed spectra of Aph and Anap
#' as well as the NAP spectral slopes for the different method. The program computes the Aph using three approaches:
#' \itemize{
#'    \item{1. The Anap is subtracted from Ap after adjusting the NIR of Anap
#' such as Ap(NIR)==Anap(NIR) to obtain Aph.offset;}
#'    \item{2. An exponential fit is applied to the Anap spectrum (Anap.fitted) avoiding
#' the blue and red portions of the spectrum where some remaining
#' pigments on the filter after methanol extraction may be present.}
#'    \item{3. A spectral deconvolution following the method of Bricaud and Stramski (1990)
#' is performed. Two equations are applied to Ap spectra (their eq. 5 and eq 6) yielding
#' Aph.BS90, Anap.BS90 and Aph.BS90.v2, Anap.BS90.v2.}
#' }

#' @author Simon Bélanger


compute.Aph <- function(waves, Ap, Anap){

  # Fit an exponential function to Anap to avoid remaining
  # pigments on the filter after methanol extraction

  # find index some lambda
  i310 = which(waves == 310)
  i380 = which(waves == 380)
  i400 = which(waves == 400)
  i443 = which(waves == 443)
  i500 = which(waves == 500)
  i505 = which(waves == 505)
  i580 = which(waves == 580)
  i620 = which(waves == 620)
  i693 = which(waves == 693)
  i710 = which(waves == 710)
  i750 = which(waves == 750)

  if (!is.na(Anap[1])) {

    # Apply off set to NAP in the NIR
    offset = mean(Anap[i750-5:i750+5]) - mean(Ap[i750-5:i750+5])
    Anap <- Anap - offset

    # Replace NA by 0
    ix = is.na(Anap)
    Anap[ix] = 0
    Anap400 = Anap[i400]
    Anap750 = Anap[i750]

    # Check for consistency in NAP spectra
    if (Anap[i380] > 1.1*Anap[i310]) {
      print("NAP spectrum not realistic")
      print("No fit for this spectra")
      print("Fitted spectum for NAP will be set to the measured spectrum")
      coef <- c(NA,NA,NA)
      Anapf <- Anap

    } else {

      # Subset the aNAP file to avoid residual pigments.
      df = as.data.frame(cbind(waves[c(i750:i710,i620:i500,i400:i310)],
                               Anap[c(i750:i710,i620:i500,i400:i310)]))
      names(df) <- c("waves", "Anap")

      model <- nls(Anap ~ A*exp(-B*(waves-400)) + C, data=df,
                   start=list(A=Anap400, B=0.009, C=Anap750))

      coef = coef(model)
      Anapf <- coef[1]*exp(-coef[2]*(waves-400)) + coef[3]

    }

    Aphf  = Ap - Anapf
    Aph = Ap - Anap

    ix = (Aph < 0)
    Aph[ix] = 0
    ix = (Aphf < 0)
    Aphf[ix] = 0

    # Spetral deconvolution of Bricaud and Stramski 1990
    # right hand side of eq 5c
    # 0.99*A*exp(-380*S) - A*exp(-505*S) = res1
    res1 = 0.99*Ap[i380] - Ap[i505]

    # right hand side of eq 5d
    # A*exp(-580*S) - 0.92*A*exp(-693*S) = res2
    res2 = Ap[i580] - 0.92*Ap[i693]

    # A * exp(-750*S) = ap(750)
    #  Otherwise using A*exp(-750*S) = ap750 for 5d
    res3 = Ap[i750]

    res = c(res1,res2,res3)
    par = c(5,0.01)

    m = nleqslv(par, find.nap, res=res)
    m2 = nleqslv(par, find.nap2, res=res)


    # Equation 6 de Bricaud
    Anap.BS90 = m$x[1]*exp(-m$x[2]*waves) + Ap[i750] - m$x[1]*exp(-m$x[2]*750)

    Aph.BS90  = Ap - Anap.BS90

    Aph.BS90[Aph.BS90 < 0] = 0

    # Equation 6 de Bricaud
    Anap.BS90.v2 = m2$x[1]*exp(-m2$x[2]*waves) + Ap[i750] - m2$x[1]*exp(-m2$x[2]*750)

    Aph.BS90.v2  = Ap - Anap.BS90.v2

    Aph.BS90.v2[Aph.BS90.v2 < 0] = 0

    return(list(Aph=Aph, Anap.offset=Anap,
                Aph.fitted = Aphf, Anap.fitted = Anapf , Snap.fitted = coef[2], C.fitted=coef[3],
                Aph.BS90=Aph.BS90, Anap.BS90=Anap.BS90, Snap.BS90 = m$x[2],
                Aph.BS90.v2=Aph.BS90.v2, Anap.BS90.v2=Anap.BS90.v2, Snap.BS90.v2 = m2$x[2]))


  } else {


    # Spetral deconvolution of Bricaud and Stramski 1990
    # right hand side of eq 5c
    # 0.99*A*exp(-380*S) - A*exp(-505*S) = res1
    res1 = 0.99*Ap[i380] - Ap[i505]

    # right hand side of eq 5d
    # A*exp(-580*S) - 0.92*A*exp(-693*S) = res2
     res2 = Ap[i580] - 0.92*Ap[i693]

    #  Otherwise using A*exp(-750*S) = ap750 for 5d
     res3 = Ap[i750]

    res = c(res1,res2,res3)
    par = c(5,0.01)

    m = nleqslv(par, find.nap, res=res)
    m2 = nleqslv(par, find.nap2, res=res)

    # Equation 6 de Bricaud
    Anap.BS90 = m$x[1]*exp(-m$x[2]*waves) + Ap[i750] - m$x[1]*exp(-m$x[2]*750)
    Aph.BS90  = Ap - Anap.BS90
    Aph.BS90[Aph.BS90 < 0] = 0


    # Equation 6 de Bricaud
    Anap.BS90.v2 = m2$x[1]*exp(-m2$x[2]*waves) + Ap[i750] - m2$x[1]*exp(-m2$x[2]*750)
    Aph.BS90.v2  = Ap - Anap.BS90.v2
    Aph.BS90.v2[Aph.BS90.v2 < 0] = 0

    return(list(Aph=Aph.BS90, Anap.offset=NA,
                Aph.fitted = NA, Anap.fitted = NA , Snap.fitted = NA, C.fitted= NA,
                Aph.BS90=Aph.BS90, Anap.BS90=Anap.BS90, Snap.BS90 = m$x[2],
                Aph.BS90.v2=Aph.BS90.v2, Anap.BS90.v2=Anap.BS90.v2, Snap.BS90.v2 = m2$x[2]))
  }

}

find.nap <- function(par, res) {
  fval  <- numeric(length(par))
  A <- par[1]
  S <- par[2]

  fval[1] <- 0.99*A*exp(-380*S) - A*exp(-505*S) - res[1]
  fval[2] <- A*exp(-580*S) - 0.92*A*exp(-693*S) - res[2]

  fval
}

find.nap2 <- function(par, res) {
  fval  <- numeric(length(par))
  A <- par[1]
  S <- par[2]

  fval[1] <- 0.99*A*exp(-380*S) - A*exp(-505*S) - res[1]
  fval[2] <- A*exp(-750*S)  - res[3]

  fval
}
