% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_pluck_water.R
\name{convert_water}
\alias{convert_water}
\alias{convert_watermg}
\title{Convert \code{water} class object to a dataframe}
\usage{
convert_water(water)

convert_watermg(water)
}
\arguments{
\item{water}{A water class object}
}
\value{
A data frame containing columns for all non-NA water slots.

A data frame containing columns for all non-NA water slots with ions in mg/L.
}
\description{
This converts a \code{water} class to a dataframe with individual columns for each slot (water quality parameter) in the \code{water}.
This is useful for one-off checks and is applied in all \code{fn_once} tidywater functions. For typical applications,
there may be a \code{fn_once} tidywater function that provides a more efficient solution.

Use \link{convert_water} to keep all slots in the same units as the water.

Use \link{convert_watermg} to convert to more typical units. Converts the following slots from M to mg/L:
na, ca, mg, k, cl, so4, hco3, co3, h2po4, hpo4, po4, ocl, bro3, f, fe, al.
Converts these slots to ug/L: br, mn.  All other values remain unchanged.
}
\examples{

library(dplyr)
library(tidyr)

# Generates 1 row dataframe
example_df <- define_water(ph = 7, temp = 20, alk = 100) \%>\%
  convert_water()

example_df <- water_df \%>\%
  define_water_chain() \%>\%
  mutate(to_dataframe = map(defined_water, convert_water)) \%>\%
  unnest(to_dataframe) \%>\%
  select(-defined_water)

water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) \%>\%
  convert_watermg()

}
