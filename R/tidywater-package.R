#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
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

#' Data frame of Edwards model coefficients
#'
#' A dataset containing coefficients from the Edwards (1997) model for coagulation TOC removal.
#'
#' @docType data
#' @keywords datasets
#' @name edwardscoeff
#' @format A dataframe with 5 rows and 7 columns:
#' \describe{
#' \item{ID}{Coefficient type}
#' \item{x3}{x3 parameter}
#' \item{x2}{x2 parameter}
#' \item{x1}{x1 parameter}
#' \item{k1}{k1 parameter}
#' \item{k2}{k2 parameter}
#' \item{b}{b parameter}
#' }
#' @source Edwards (1997) Table 2.
"edwardscoeff"

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
#' \item{tot_po4}{Total phosphate in mg/L as PO42-}
#' }
#' @source Fabricated for use in examples.
"water_df"

#' Data frame of equilibrium constants for lead and copper solubility
#'
#' A dataset containing equilibrium constants for lead and copper solubility
#'
#' @docType data
#' @keywords datasets
#' @name leadsol_constants
#' @format A dataframe with 1 row and 38 columns
#' @format Solids:
#' \describe{
#' \item{K_solid_lead_hydroxide}{Solid lead hydroxide}
#' \item{K_solid_cerussite}{Solid cerussite}
#' \item{K_solid_hydrocerussite}{Solid hydrocerussite}
#' \item{K_solid_hydroxypyromorphite_schock}{Solid hydroxypyromorphite, from Schock et al. 1996}
#' \item{K_solid_hydroxypyromorphite_zhu}{Solid hydroxypyromorphite, from Zhu et al. 2015}
#' \item{K_solid_pyromorphite_xie}{Solid pyromorphite, from Xie & Giammar 2007}
#' \item{K_solid_pyromorphite_topolska}{Solid pyromorphite, from Topolska et al. 2016}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_secondary_lead_ortho}{Solid secondary lead orthophosphate}
#' \item{K_solid_tertiary_lead_ortho}{Solid tertiary lead orthophosphate}
#' \item{K_solid_anglesite}{Solid anglesite}
#' \item{K_solid_laurionite_nl}{Solid laurionite, from Nasanen & Lindell 1976}
#' \item{K_solid_laurionite_loth}{Solid laurionite, from Lothenbach et al. 1999}
#' }
#' @format Lead-Hydroxide Complexes:
#' \describe{
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' }
#' @format Lead-Chloride Complexes:
#' \describe{
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' }
#' @format Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes:
#' \describe{
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' }
#' @format Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes:
#' \describe{
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' }
#' @format Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes:
#' \describe{
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' \item{K_solid_primary_lead_ortho}{Solid primary lead orthophosphate}
#' }


#' @source Edwards (1997) Table 2.
"leadsol_constants"