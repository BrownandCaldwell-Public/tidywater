% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidywater-package.R
\docType{data}
\name{dbp_correction}
\alias{dbp_correction}
\title{Data frame of correction factors for estimating DBP formation as a function of location}
\format{
A dataframe with 17 rows and 4 columns
\describe{
\item{ID}{abbreviation of dbp species}
\item{alias}{full name of dbp species}
\item{plant}{specifies the correction factor for modelling DBP formation within a treatment plant}
\item{ds}{specifies the correction factor for modelling DBP formation within the distribution system}
}
}
\source{
U.S. EPA (2001), Table 5-7

See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
}
\usage{
dbp_correction
}
\description{
A dataset containing correction factors for calculating DBP formation
}
\keyword{datasets}
