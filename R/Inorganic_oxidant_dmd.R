#'@title Inorganic Oxidant Demand
#'
#' @description Calculates Cl residual from inorganic scavengers: Mg, Fe, and Ammonia.
#'
#' @param cl_dose chlorine dose
#' @param mg concentration of Mg in M(??)
#' @param fe concentration of irron
#' @param nh3 concentration of ammonia in the water
#' @param water input water, if provided will override individal species input
#'
#' Look at MWH textbook maybe to understand how to implement these reactions, whicha are esentially breakpoint reactions.
#' I think its essentially a ratio from the gitHub comments
#' weight ratio = 1.5 mol/mol 71 g cl2/ 14 g N = 7.61 mg cl2/mgN. So I think that this is saying that for every mg of N 7.61 g of Cl2 will be consumed.
#' and for Fe, its .64 c cl2/g fe
#' and for permanganate its 1.29
#'
#' so basically, the function should look like multiplying concentrations of each oxidant by their number, adding those numbers, then subtracting them from the cl dose. Cool and easy
#' @examples
#' inorg_ox_dmd(cl_dose = 20, mg = 1, fe = 2, tot_nh3 = 1)
#'
#' water1<-define_water(ph = 7, alk = 70, mg = 1, fe = 2, nh3 = 1)
#' cl_residual <- inorg_ox_dmd(20, water = water1)
#'
#' @export
#'

inorg_ox_dmd <- function(cl_dose, mg, fe, nh3, water ) {
  # make a multplying funct because both the if and else will use same function, and I want to try a different approach! functional programing 
  cl_residual <- function(dose, mg, fe, nh3) {
    mg <- mg * 1.29
    fe <- fe * 0.64
    nh3 <- nh3 * 7.6
    residual <- dose - (mg + fe + nh3)
    if (residual < 0) {
      cli::cli_abort("There is too much oxidant in the water, all avalaible cl will be used")
    } else {
      return(residual)
    }
  }

  if (missing(water)) {
    return(cl_residual(dose = cl_dose, mg = mg, fe = fe, nh3 = nh3))
  } else {
    validate_water(water, c("mg", "fe", "tot_nh3"))

    # convert each species to mg/l
    mg  <- convert_units(water@mg, "mg", "M", "mg/L")
    fe  <- convert_units(water@fe, "fe", "M", "mg/L")
    # tot nh3 stored as N 
    nh3 <- convert_units(water@tot_nh3, "n", "M", "mg/L")

    return(cl_residual(dose = cl_dose, mg = mg, fe = fe, nh3 = nh3))
  }
}
