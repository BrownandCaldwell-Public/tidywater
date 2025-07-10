#' @keywords internal
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(".")

## usethis namespace: start
#' @importFrom rlang :=
## usethis namespace: end
NULL

#' Molar weights of relevant compounds
#'
#' A dataset containing the molar weights of several compounds in g/mol. Column names are lowercase chemical formulas (with no charge),
#' with the exception of the following coagulants:
#' alum = Al2(SO4)3*14H2O,
#' ferricchloride = FeCl3,
#' ferricsulfate = Fe2(SO4)3*8.8H2O,
#' @docType data
#' @keywords datasets
#' @name mweights
#' @format A dataframe with one row and one column per compound
"mweights"


#' Dissociation constants and standard enthalpy for weak acids/bases
#'
#' Equilibrium constants (k) and corresponding standard enthalpy of reaction values (deltah) for significant acids in
#' water influencing pH at equilibrium. Includes carbonate, sulfate, phosphate, and hypochlorite.
#' Standard enthalpy of reaction is calculated by taking the sum of the enthalpy of formation of each individual component
#' minus the enthalpy of formation of the final product. e.g., the standard enthalpy of reaction for water can be
#' calculated as: deltah_h2o = deltah_f_oh + deltah_f_h - deltah_f_h2o = -230 + 0 - (-285.83) = 55.83 kJ/mol.
#' See MWH (2012) example 5-5 and Benjamin (2002) eq. 2.96.
#'
#' @docType data
#' @keywords datasets
#' @name discons
#' @format A dataframe with 8 rows and 3 columns
#' \describe{
#' \item{ID}{Coefficient type}
#' \item{k}{Equilibrium constant}
#' \item{deltah}{Standard enthalpy in J/mol}
#' }
#' @source Benjamin (2015) Appendix A.1 and A.2.
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
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
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
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
#' A dataset containing equilibrium constants for lead solubility
#'
#' @docType data
#' @keywords datasets
#' @name leadsol_constants
#' @format A dataframe with 38 rows and 3 columns
#' @format Solids:
#' \describe{
#' \item{species_name}{Name of lead solid or complex with possible _letter to cite different references}
#' \item{constant_name}{Reference ID for constants}
#' \item{log_value}{Equilibrium constant log value}
#' \item{source}{Source for equilibrium constant value}
#' }
#'
#' @source Benjamin (2010)
#' @source Lothenbach et al. (1999)
#' @source Nasanen & Lindell (1976)
#' @source Powell et al. (2009)
#' @source Powell et al. (2005)
#' @source Schock et al. (1996)
#' @source Topolska et al. (2016)
#' @source Xie & Giammar (2007)
#' @source Zhu et al. (2015)
#' @source Wahman et al. (2021)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
"leadsol_constants"

#' Data frame of DBP coefficients for predicting DBP formation
#'
#' A dataset containing coefficients for calculating DBP formation
#'
#' @docType data
#' @keywords datasets
#' @name dbpcoeffs
#' @format A dataframe with 30 rows and 10 columns
#' \describe{
#' \item{ID}{abbreviation of dbp species}
#' \item{alias}{full name of dbp species}
#' \item{water_type}{specifies which model the constants apply to, either treated or untreated water}
#' \item{A}{First coefficient in DBP model}
#' \item{a}{Second coefficient in DBP model, associated with TOC or DOC}
#' \item{b}{Third coefficient in DBP model, associated with Cl2 }
#' \item{c}{Fourth coefficient in DBP model, associated with Br-}
#' \item{d}{Fifth coefficient in DBP model, associated with temperature}
#' \item{e}{Sixth coefficient in DBP model, associated with pH}
#' \item{f}{Seventh coefficient in DBP model, associated with reaction time}
#' }
#'
#' @source U.S. EPA (2001)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
"dbpcoeffs"

#' Data frame of conversion factors for estimating DBP formation from chloramines
#'
#' A dataset containing conversion factors for calculating DBP formation
#'
#' @docType data
#' @keywords datasets
#' @name chloramine_conv
#' @format A dataframe with 17 rows and 3 columns
#' \describe{
#' \item{ID}{abbreviation of dbp species}
#' \item{alias}{full name of dbp species}
#' \item{percent}{specifies the percent of DBP formation predicted from chloramines compared to chlorine, assuming the same chlorine dose applied}
#' }
#' @source U.S. EPA (2001), Table 5-10
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
"chloramine_conv"

#' Data frame of correction factors for estimating DBP formation as a function of location
#'
#' A dataset containing correction factors for calculating DBP formation
#'
#' @docType data
#' @keywords datasets
#' @name dbp_correction
#' @format A dataframe with 17 rows and 4 columns
#' \describe{
#' \item{ID}{abbreviation of dbp species}
#' \item{alias}{full name of dbp species}
#' \item{plant}{specifies the correction factor for modelling DBP formation within a treatment plant}
#' \item{ds}{specifies the correction factor for modelling DBP formation within the distribution system}
#' }
#' @source U.S. EPA (2001), Table 5-7
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
"dbp_correction"



#' Data frame of bromate coefficients for predicting bromate formation during ozonation
#'
#' A dataset containing coefficients for calculating ozone formation
#'
#' @docType data
#' @keywords datasets
#' @name bromatecoeffs
#' @format A dataframe with 30 rows and 10 columns
#' \describe{
#' \item{model}{First author of source model}
#' \item{ammonia}{Either T or F, depending on whether the model applies to waters with ammonia present.}
#' \item{A}{First coefficient in bromate model}
#' \item{a}{Exponent in bromate model, associated with Br-}
#' \item{b}{Exponent in bromate model, associated with DOC}
#' \item{c}{Exponent in bromate model, associated with UVA}
#' \item{d}{Exponent in bromate model, associated with pH}
#' \item{e}{Exponent in bromate model, associated with Alkalinity}
#' \item{f}{Exponent in bromate model, associated with ozone dose}
#' \item{g}{Exponent in bromate model, associated with reaction time}
#' \item{h}{Exponent in bromate model, associated with ammonia (NH4+)}
#' \item{i}{Exponent in bromate model, associated with temperature}
#' \item{I}{Coefficient in bromate model, associated with temperature in the exponent. Either i or I are used, not both.}
#' }
#'
#' @source Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997), Siddiqui et al (1994)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
"bromatecoeffs"

#' Data frame of Cl2 decay coefficients
#'
#' A dataset containing coefficients for calculating Cl2 decay
#'
#' @docType data
#' @keywords datasets
#' @name cl2coeffs
#' @format A dataframe with 3 rows and 4 columns
#' \describe{
#' \item{treatment}{Specifies the treatment applied to the water}
#' \item{a}{Coefficient in chlorine decay model, associated with chlorine dose and time}
#' \item{b}{Coefficient in chlorine decay model, associated with chlorine dose & organics}
#' \item{c}{Exponent in chlorine decay model, associated with chlorine dose & organics}
#' }
#'
#' @source U.S. EPA (2001)
"cl2coeffs"

#' Data frame of PAC TOC model coefficients
#'
#' A dataset containing coefficients for calculating PAC TOC removal
#'
#' @docType data
#' @keywords datasets
#' @name pactoccoeffs
#' @format A dataframe with 4 rows and 3 columns
#' \describe{
#' \item{pactype}{Specifies PAC type}
#' \item{A}{Constant in the PAC model}
#' \item{a}{Coefficient in PAC model, associated with DOC0}
#' \item{b}{Coefficient in PAC model, associated with dose}
#' \item{c}{Coefficient in PAC model, associated with time}
#' }
#'
#' @source Cho (2007)
"pactoccoeffs"
