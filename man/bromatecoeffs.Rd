% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidywater-package.R
\docType{data}
\name{bromatecoeffs}
\alias{bromatecoeffs}
\title{Data frame of bromate coefficients for predicting bromate formation during ozonation}
\format{
A dataframe with 30 rows and 10 columns
\describe{
\item{model}{First author of source model}
\item{ammonia}{Either T or F, depending on whether the model applies to waters with ammonia present.}
\item{A}{First coefficient in bromate model}
\item{a}{Exponent in bromate model, associated with Br-}
\item{b}{Exponent in bromate model, associated with DOC}
\item{c}{Exponent in bromate model, associated with UVA}
\item{d}{Exponent in bromate model, associated with pH}
\item{e}{Exponent in bromate model, associated with Alkalinity}
\item{f}{Exponent in bromate model, associated with ozone dose}
\item{g}{Exponent in bromate model, associated with reaction time}
\item{h}{Exponent in bromate model, associated with ammonia (NH4+)}
\item{i}{Exponent in bromate model, associated with temperature}
\item{I}{Coefficient in bromate model, associated with temperature in the exponent. Either i or I are used, not both.}
}
}
\source{
Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997), Siddiqui et al (1994)

See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
}
\usage{
bromatecoeffs
}
\description{
A dataset containing coefficients for calculating ozone formation
}
\keyword{datasets}
