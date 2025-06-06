% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biofilter_toc.R
\name{biofilter_toc}
\alias{biofilter_toc}
\alias{biofilter_toc_chain}
\title{Determine TOC removal from biofiltration using Terry & Summers BDOC model}
\source{
Terry and Summers 2018
}
\usage{
biofilter_toc(water, ebct, ozonated = TRUE)

biofilter_toc_chain(
  df,
  input_water = "defined_water",
  output_water = "biofiltered_water",
  ebct = "use_col",
  ozonated = "use_col"
)
}
\arguments{
\item{water}{Source water object of class "water" created by \link{define_water}.}

\item{ebct}{The empty bed contact time (min) used for the biofilter.}

\item{ozonated}{Logical; TRUE if the water is ozonated (default), FALSE otherwise.}

\item{df}{a data frame containing a water class column, which has already been computed using
\link{define_water_chain}. The df may include a column indicating the EBCT or whether the water is ozonated.}

\item{input_water}{name of the column of Water class data to be used as the input for this function. Default is "defined_water".}

\item{output_water}{name of the output column storing updated parameters with the class, Water. Default is "biofiltered_water".}
}
\value{
\code{biofilter_toc} returns water class object with modeled DOC removal from biofiltration.

\code{biofilter_toc_chain} returns a data frame containing a water class column with updated DOC, TOC, and UV254 water slots.
}
\description{
This function applies the Terry model to a water created by \link{define_water} to determine biofiltered
DOC (mg/L).
For a single water use \code{biofilter_toc}; for a dataframe use \code{biofilter_toc_chain}.
Use \link{pluck_water} to get values from the output water as new dataframe columns.
For most arguments in the \verb{_chain} helper
"use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
function instead or an unquoted column name can be provided.
}
\details{
For large datasets, using \code{fn_once} or \code{fn_chain} may take many minutes to run. These types of functions use the furrr package
for the option to use parallel processing and speed things up. To initialize parallel processing, use
\code{plan(multisession)} or \code{plan(multicore)} (depending on your operating system) prior to your piped code with the
\code{fn_once} or \code{fn_chain} functions. Note, parallel processing is best used when your code block takes more than a minute to run,
shorter run times will not benefit from parallel processing.
}
\examples{
library(tidywater)
water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) \%>\%
  biofilter_toc(ebct = 10, ozonated = FALSE)


library(purrr)
library(tidyr)
library(dplyr)

example_df <- water_df \%>\%
  define_water_chain() \%>\%
  biofilter_toc_chain(input_water = "defined_water", ebct = 10, ozonated = FALSE)

example_df <- water_df \%>\%
  define_water_chain() \%>\%
  mutate(
    BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
    ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
  ) \%>\%
  biofilter_toc_chain(input_water = "defined_water", ebct = BiofEBCT)

\donttest{
# Initialize parallel processing
library(furrr)
# plan(multisession)
example_df <- water_df \%>\%
  define_water_chain() \%>\%
  biofilter_toc_chain(input_water = "defined_water", ebct = c(10, 20))

# Optional: explicitly close multisession processing
# plan(sequential)
}

}
