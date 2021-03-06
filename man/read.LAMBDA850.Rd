% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.LAMBDA850.R
\name{read.LAMBDA850}
\alias{read.LAMBDA850}
\title{Read CSV file from lambda 850}
\usage{
read.LAMBDA850(filen)
}
\arguments{
\item{filen}{is the name of the CSV file.}
}
\value{
It returns a data frame with 2 columns for wavelength (wl)
and Optical Depth (OD).
}
\description{
Read CSV file from a Perkin Elmer Lambda-850 model,
which is a dual-beam spectrophotometer.
}
\details{
OD is unitless. It is equal to the log10(I0/I) where I0 is the
reference beam intensity and I the sample beam intensity.
}
\author{
Simon Bélanger
}
