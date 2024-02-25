#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
## usethis namespace: end
NULL

#' Molar weights of relevant compounds
#'
#' A dataset containing the molar weights of several compounds in g/mol
#'
#' @docType data
#' @keywords datasets
#' @name mweights
#' @format A dataframe with one row and one column per compound
"mweights"


#' Dissociation constants for weak acids/bases
#'
#' A dataset containing the dissociation constants for carbonate, sulfate, phosphate, and hypochlorite
#'
#' @docType data
#' @keywords datasets
#' @name discons
#' @format A dataframe with one row and one column per constant
"discons"

#' Data frame of water quality parameters
#'
#' A dataset containing fabricated water quality to use as tidywater inputs.
#' Parameters are set to reasonable water quality ranges. Parameters are as follows:
#'
#' @docType data
#' @keywords datasets
#' @name water_df
#' @format A dataframe with 12 rows and 11 columns:
#' \describe{
#' \item{ph}{pH in standard units (SU)}
#' \item{temp}{Temperature in degree C}
#' \item{alk}{Alkalinity in mg/L as CaCO3}
#' \item{tot_hard}{Total hardness in mg/L as CaCO3}
#' \item{ca_hard}{Calcium hardness in mg/L as CaCO3}
#' \item{na}{Sodium in mg/L Na+}
#' \item{k}{Potassium in mg/L K+}
#' \item{cl}{Chloride in mg/L Cl-}
#' \item{so4}{Sulfate in mg/L SO42-}
#' \item{tot_ocl}{Total chlorine in mg/L as Cl2}
#' \item{po4}{Phosphate in mg/L as PO42-}
#' }
#' @source Fabricated for use in examples.
"water_df"
