# Acid/Base Equilibrium Functions

#### Function to calculate the pH from a given water quality vector using R implementation.

solve_ph_r <- function(water, so4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0, mno4_dose = 0, no3_dose = 0) {
  # Correct eq constants
  ks <- correct_k(water)
  gamma1 <- calculate_activity(1, water@is, water@temp)


  #### SOLVE FOR pH
  solve_h <- function(h, kw, so4_dose, tot_po4, h2po4_i, hpo4_i, po4_i, tot_co3, tot_ocl, tot_nh3, ocl_i, nh4_i, 
    tot_bo3, bo3_i, tot_sio4, h2sio4_i, h3sio4_i, tot_ch3coo, ch3coo_i,
                      carbonate_alk_eq, oh_i, h_i, na_dose, ca_dose, mg_dose, cl_dose, mno4_dose, no3_dose) {
    kw / (h * gamma1^2) +
      2 * so4_dose +
      tot_po4 * (calculate_alpha1_phosphate(h, ks) +
        2 * calculate_alpha2_phosphate(h, ks) +
        3 * calculate_alpha3_phosphate(h, ks)) +
      tot_co3 * (calculate_alpha1_carbonate(h, ks) +
        2 * calculate_alpha2_carbonate(h, ks)) +
      tot_ocl * calculate_alpha1_hypochlorite(h, ks) +
      tot_bo3 * calculate_alpha1_borate(h, ks) +
      tot_sio4 * (calculate_alpha1_silicate(h, ks) +
        2 * calculate_alpha2_silicate(h, ks)) +
      tot_ch3coo * calculate_alpha1_acetate(h, ks) +
      cl_dose +
      mno4_dose +
      no3_dose -
      (h + na_dose + 2 * ca_dose + 2 * mg_dose +
        tot_nh3 * calculate_alpha1_ammonia(h, ks)) -
      (carbonate_alk_eq + oh_i) -
      3 * po4_i - 2 * hpo4_i - h2po4_i - ocl_i + nh4_i + h_i - bo3_i - 2 * h2sio4_i - h3sio4_i
  }

  root_h <- stats::uniroot(solve_h,
    interval = c(1e-14, 1),
    kw = water@kw,
    so4_dose = so4_dose,
    tot_po4 = water@tot_po4,
    po4_i = water@po4,
    hpo4_i = water@hpo4,
    h2po4_i = water@h2po4,
    tot_co3 = water@tot_co3,
    tot_ocl = water@free_chlorine,
    ocl_i = water@ocl,
    tot_nh3 = water@tot_nh3,
    nh4_i = water@nh4,
    tot_bo3 = water@tot_bo3,
    bo3_i = water@bo3,
    tot_sio4 = water@tot_sio4,
    h2sio4_i = water@h2sio4,
    h3sio4_i = water@h3sio4,
    carbonate_alk_eq = water@carbonate_alk_eq,
    oh_i = water@oh,
    h_i = water@h,
    tot_ch3coo = water@tot_ch3coo,
    ch3coo_i = water@ch3coo,
    na_dose = na_dose,
    ca_dose = ca_dose,
    mg_dose = mg_dose,
    cl_dose = cl_dose,
    mno4_dose = mno4_dose,
    no3_dose = no3_dose,
    tol = 1e-14
  )
  phfinal <- -log10(root_h$root * gamma1)
  return(round(phfinal, 2))
}

#### Rust implementation of solve_ph with same API
#' Solve pH for water chemistry equilibrium (Rust implementation)
#'
#' This function provides a faster Rust implementation of the pH solver
#' with the same API as the original solve_ph function.
#'
#' @param water Water object (S4 class) with water chemistry parameters
#' @param so4_dose Sulfate dose (default 0)
#' @param na_dose Sodium dose (default 0)
#' @param ca_dose Calcium dose (default 0)
#' @param mg_dose Magnesium dose (default 0)
#' @param cl_dose Chloride dose (default 0)
#' @param mno4_dose Permanganate dose (default 0)
#' @param no3_dose Nitrate dose (default 0)
#' @return pH value rounded to 2 decimal places
#' @export
solve_ph_rust <- function(water, so4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0, mno4_dose = 0, no3_dose = 0) {
  # Extract parameters from water object and call Rust function
  result <- solve_ph_rust_inner(
    temp = water@temp,
    ionic_strength = if (is.na(water@is)) NULL else water@is,
    kw = water@kw,
    tot_po4 = water@tot_po4,
    tot_co3 = water@tot_co3,
    tot_ocl = water@free_chlorine,
    tot_nh3 = water@tot_nh3,
    tot_ch3coo = water@tot_ch3coo,
    h2po4_i = water@h2po4,
    hpo4_i = water@hpo4,
    po4_i = water@po4,
    ocl_i = water@ocl,
    nh4_i = water@nh4,
    ch3coo_i = water@ch3coo,
    carbonate_alk_eq = water@carbonate_alk_eq,
    oh_i = water@oh,
    h_i = water@h,
    so4_dose = so4_dose,
    na_dose = na_dose,
    ca_dose = ca_dose,
    mg_dose = mg_dose,
    cl_dose = cl_dose,
    mno4_dose = mno4_dose,
    no3_dose = no3_dose
  )

  # Round to 2 decimal places like the original function
  return(round(result, 2))
}

#### Main solve_ph function with backend selection
#' Solve pH for water chemistry equilibrium
#'
#' This function solves for the pH of water given its chemical composition
#' and optional chemical doses. It can use either a fast Rust implementation
#' (default) or the original R implementation.
#'
#' @param water Water object (S4 class) with water chemistry parameters
#' @param so4_dose Sulfate dose (default 0)
#' @param na_dose Sodium dose (default 0)
#' @param ca_dose Calcium dose (default 0)
#' @param mg_dose Magnesium dose (default 0)
#' @param cl_dose Chloride dose (default 0)
#' @param mno4_dose Permanganate dose (default 0)
#' @param no3_dose Nitrate dose (default 0)
#' @param backend Character string specifying which backend to use:
#'   "rust" (default, faster) or "r" (original R implementation)
#' @return pH value rounded to 2 decimal places
#' @export
solve_ph <- function(water, so4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0, mno4_dose = 0, no3_dose = 0, backend = "rust") {
  # Validate backend parameter
  backend <- match.arg(backend, choices = c("rust", "r"))

  # Call the appropriate backend
  if (backend == "rust") {
    return(solve_ph_rust(water, so4_dose, na_dose, ca_dose, mg_dose, cl_dose, mno4_dose, no3_dose))
  } else {
    return(solve_ph_r(water, so4_dose, na_dose, ca_dose, mg_dose, cl_dose, mno4_dose, no3_dose))
  }
}
