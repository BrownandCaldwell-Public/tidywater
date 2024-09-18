# Acid/Base Equilibrium Functions
# These functions determine pH and alkalinity after blending and chemical addition.

#### Function to calculate the pH from a given water quality vector. Not exported in namespace.

solve_ph <- function(water, so4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0) {
  # Correct eq constants
  ks <- correct_k(water)

  #### SOLVE FOR pH
  solve_h <- function(h, kw, so4_dose, tot_po4, h2po4_i, hpo4_i, po4_i, tot_co3, tot_ocl, tot_nh3, ocl_i, nh4_i,
                      alk_eq, na_dose, ca_dose, mg_dose, cl_dose) {
    kw / h +
      2 * so4_dose +
      tot_po4 * (calculate_alpha1_phosphate(h, ks) +
        2 * calculate_alpha2_phosphate(h, ks) +
        3 * calculate_alpha3_phosphate(h, ks)) +
      tot_co3 * (calculate_alpha1_carbonate(h, ks) +
        2 * calculate_alpha2_carbonate(h, ks)) +
      tot_ocl * calculate_alpha1_hypochlorite(h, ks) +
      cl_dose -
      (h + na_dose + 2 * ca_dose + 2 * mg_dose +
        tot_nh3 * calculate_alpha1_ammonia(h, ks)) -
      alk_eq -
      3 * po4_i - 2 * hpo4_i - h2po4_i - ocl_i + nh4_i
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
    tot_ocl = water@tot_ocl,
    ocl_i = water@ocl,
    tot_nh3 = water@tot_nh3,
    nh4_i = water@nh4,
    alk_eq = water@alk_eq,
    na_dose = na_dose,
    ca_dose = ca_dose,
    mg_dose = mg_dose,
    cl_dose = cl_dose,
    tol = 1e-14
  )
  phfinal <- -log10(root_h$root)
  return(round(phfinal, 2))
}
