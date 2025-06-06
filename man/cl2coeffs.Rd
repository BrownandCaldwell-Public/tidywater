% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidywater-package.R
\docType{data}
\name{cl2coeffs}
\alias{cl2coeffs}
\title{Data frame of Cl2 decay coefficients}
\format{
A dataframe with 3 rows and 4 columns
\describe{
\item{treatment}{Specifies the treatment applied to the water}
\item{a}{Coefficient in chlorine decay model, associated with chlorine dose and time}
\item{b}{Coefficient in chlorine decay model, associated with chlorine dose & organics}
\item{c}{Exponent in chlorine decay model, associated with chlorine dose & organics}
}
}
\source{
U.S. EPA (2001)
}
\usage{
cl2coeffs
}
\description{
A dataset containing coefficients for calculating Cl2 decay
}
\keyword{datasets}
