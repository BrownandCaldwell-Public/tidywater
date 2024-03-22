#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %$%
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
#' @format A dataframe with 7 rows and 3 columns
#' \describe{
#' \item{ID}{Coefficient type}
#' \item{k}{Equilibrium constant}
#' \item{deltah}{Standard enthalpy in J/mol}
#' }
#' @source Benjamin (2015) Appendix A.1 and A.2.
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
#' @format Lead-Hydroxide Complexes from Table 4-14
#' \describe{
#' \item{B_1_OH{PbOH+}
#' \item{B_2_OH}{Pb(OH)2}
#' \item{B_3_OH}{Pb(OH)3-}
#' \item{B_4_OH}{Pb(OH)4-2}
#' \item{B_2_1_OH}{Pb2(OH)+3}
#' \item{B_3_4_OH}{Pb3(OH)4+2}
#' \item{B_4_4_OH}{Pb4(OH)4+4}
#' \item{B_6_8_OH}{Pb6(OH)8+4}
#' }
#' @format Lead-Chloride Complexes:
#' \describe{
#' \item{K_1_Cl}{PbCl+1}
#' \item{B_2_Cl}{PbCl2}
#' \item{B_3_Cl}{PbCl3-}
#' \item{B_4_Cl}{PbCl4-2}
#' }
#' @format Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes:
#' \describe{
#' \item{K_s}{SO4-2}
#' \item{K_1_SO4}{PbSO4}
#' \item{B_2_SO4}{Pb(SO4)2-2}
#' }
#' @format Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes:
#' \describe{
#' \item{K_c_1}{Bicarbonate}
#' \item{K_c_2}{Carbonate}
#' \item{K_1_CO3}{PbHCO3+}
#' \item{K_2_CO3}{PbCO3}
#' \item{K_3_CO3}{Pb(CO3)2-2}
#' }
#' @format Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes:
#' \describe{
#' \item{K_p_1}{H2PO4—}
#' \item{K_p_2}{HPO4—2}
#' \item{K_p_3}{PO4-3}
#' \item{K_1_PO4}{PbHPO4}
#' \item{K_2_PO4}{PbH2PO4+}
#' }
#'


#' @source Benjamin, M. M. (2002) Water Chemistry, 1st Edition, McGraw-Hill, New York, NY.
#' @source Lothenbach, B., Ochs, M., Wanner, H. & Yui, M. (1999) Thermodynamic Data for the Speciation and Solubility of Pd, Pb, Sn, Sb, Nb and Bi in Aqueous Solution. Japan Nuclear Cycle Development Institute, Ibaraki, Japan.
#' @source Nasanen, R. & Lindell, E. (1976) Studies on Lead(II) Hydroxide Salts. Part I. The Solubility Product of Pb(OH)Cl, Finnish Chemical Letters, 95.
#' @source Powell, K.J., Brown, P.L., Byrne, R.H., Gajda, T., Hefter, G., Leuz, A.K., Sjoberg, S. & Wanner, H. (2009) Chemical Speciation of Environmentally Significant Metals with Inorganic Ligands - Part 3: The Pb2+, OH—, Cl—, CO32—, SO42—, and PO43— Systems - (IUPAC Technical Report). Pure and Applied Chemistry, 81:12:2425.
#' @source Powell, K.J., Brown, P.L., Byrne, R.H., Gajda, T., Hefter, G., Sjoberg, S. & Wanner, H. (2005) Chemical Speciation of Environmentally Significant Heavy Metals with Inorganic Ligands - Part 1: The Hg2+, Cl—, OH—, CO32—, SO42—, and PO43— Aqueous Systems - (IUPAC Technical Report). Pure and Applied Chemistry, 77:4:739.
#' @source Schock, M.R., Wagner, I. & Oliphant, R.J. (1996) Chapter 4 - Corrosion and Solubility of Lead in Drinking Water. Internal Corrosion of Water Distribution Systems, 2nd Edition. American Water Works Association Research Foundation, Denver, CO.
#' @source Topolska, J., Manecki, M., Bajda, T., Borkiewicz, O. & Budzewski, P. (2016) Solubility of Pyromorphite Pb5(PO4)3Cl at 5-65 °C and Its Experimentally Determined Thermodynamic Parameters. The Journal of Chemical Thermodynamics, 98:282.
#' @source Xie, L. & Giammar, D.E. (2007) Equilibrium Solubility and Dissolution Rate of the Lead Phosphate Chloropyromorphite. Environmental Science & Technology, 41:23:8050.
#' @source Zhu, Y.N., Zhu, Z.Q., Zhao, X., Liang, Y.P. & Huang, Y.H. (2015) Characterization, Dissolution, and Solubility of Lead Hydroxypyromorphite [Pb5(PO4)3OH] at 25-45 °C. Journal of Chemistry, 2015:269387:1.
#' @source Wahman, D. G., Pinelli, M. D., Schock, M. R., & Lytle, D. A. (2021). Theoretical equilibrium lead(II) solubility revisited: Open source code and practical relationships. AWWA Water Science, e1250. https://doi.org/10.1002/aws2.1250

"leadsol_constants"