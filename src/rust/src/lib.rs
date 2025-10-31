use extendr_api::prelude::*;
use roots::{find_root_brent, SimpleConvergency};

// ============================================================================
// Alpha calculation functions for chemical equilibria
// ============================================================================

/// Calculate alpha0 for carbonate system
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1co3` - First dissociation constant for carbonic acid
/// * `k2co3` - Second dissociation constant for carbonic acid
///
/// # Returns
/// * Alpha0 value (fraction of H2CO3 species)
fn calculate_alpha0_carbonate(h: f64, k1co3: f64, k2co3: f64) -> f64 {
    1.0 / (1.0 + (k1co3 / h) + (k1co3 * k2co3 / (h * h)))
}

/// Calculate alpha1 for carbonate system
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1co3` - First dissociation constant for carbonic acid
/// * `k2co3` - Second dissociation constant for carbonic acid
///
/// # Returns
/// * Alpha1 value (fraction of HCO3- species)
fn calculate_alpha1_carbonate(h: f64, k1co3: f64, k2co3: f64) -> f64 {
    (k1co3 * h) / (h * h + k1co3 * h + k1co3 * k2co3)
}

/// Calculate alpha2 for carbonate system
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1co3` - First dissociation constant for carbonic acid
/// * `k2co3` - Second dissociation constant for carbonic acid
///
/// # Returns
/// * Alpha2 value (fraction of CO3^2- species)
fn calculate_alpha2_carbonate(h: f64, k1co3: f64, k2co3: f64) -> f64 {
    (k1co3 * k2co3) / (h * h + k1co3 * h + k1co3 * k2co3)
}

/// Calculate alpha0 for phosphate system
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1po4` - First dissociation constant for phosphoric acid
/// * `k2po4` - Second dissociation constant for phosphoric acid
/// * `k3po4` - Third dissociation constant for phosphoric acid
///
/// # Returns
/// * Alpha0 value (fraction of H3PO4 species)
fn calculate_alpha0_phosphate(h: f64, k1po4: f64, k2po4: f64, k3po4: f64) -> f64 {
    1.0 / (1.0 + (k1po4 / h) + (k1po4 * k2po4 / (h * h)) + (k1po4 * k2po4 * k3po4 / (h * h * h)))
}

/// Calculate alpha1 for phosphate system (H2PO4-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1po4` - First dissociation constant for phosphoric acid
/// * `k2po4` - Second dissociation constant for phosphoric acid
/// * `k3po4` - Third dissociation constant for phosphoric acid
///
/// # Returns
/// * Alpha1 value (fraction of H2PO4- species)
fn calculate_alpha1_phosphate(h: f64, k1po4: f64, k2po4: f64, k3po4: f64) -> f64 {
    calculate_alpha0_phosphate(h, k1po4, k2po4, k3po4) * k1po4 / h
}

/// Calculate alpha2 for phosphate system (HPO4^2-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1po4` - First dissociation constant for phosphoric acid
/// * `k2po4` - Second dissociation constant for phosphoric acid
/// * `k3po4` - Third dissociation constant for phosphoric acid
///
/// # Returns
/// * Alpha2 value (fraction of HPO4^2- species)
fn calculate_alpha2_phosphate(h: f64, k1po4: f64, k2po4: f64, k3po4: f64) -> f64 {
    calculate_alpha0_phosphate(h, k1po4, k2po4, k3po4) * (k1po4 * k2po4 / (h * h))
}

/// Calculate alpha3 for phosphate system (PO4^3-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1po4` - First dissociation constant for phosphoric acid
/// * `k2po4` - Second dissociation constant for phosphoric acid
/// * `k3po4` - Third dissociation constant for phosphoric acid
///
/// # Returns
/// * Alpha3 value (fraction of PO4^3- species)
fn calculate_alpha3_phosphate(h: f64, k1po4: f64, k2po4: f64, k3po4: f64) -> f64 {
    calculate_alpha0_phosphate(h, k1po4, k2po4, k3po4) * (k1po4 * k2po4 * k3po4 / (h * h * h))
}

/// Calculate alpha1 for hypochlorite system (OCl-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `kocl` - Dissociation constant for hypochlorous acid
///
/// # Returns
/// * Alpha1 value (fraction of OCl- species - deprotonated form with -1 charge)
fn calculate_alpha1_hypochlorite(h: f64, kocl: f64) -> f64 {
    1.0 / (1.0 + h / kocl)
}

/// Calculate alpha1 for ammonia system (NH4+)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `knh4` - Dissociation constant for ammonium
///
/// # Returns
/// * Alpha1 value (fraction of NH4+ species - protonated form with +1 charge)
fn calculate_alpha1_ammonia(h: f64, knh4: f64) -> f64 {
    1.0 / (1.0 + knh4 / h)
}

/// Calculate alpha1 for borate system (H4BO4-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `kbo3` - Dissociation constant for boric acid
///
/// # Returns
/// * Alpha1 value (fraction of H4BO4- species - deprotonated form with -1 charge)
fn calculate_alpha1_borate(h: f64, kbo3: f64) -> f64 {
    1.0 / (1.0 + h / kbo3)
}

/// Calculate alpha1 for silicate system (H3SiO4-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1sio4` - First dissociation constant for silicic acid
/// * `k2sio4` - Second dissociation constant for silicic acid
///
/// # Returns
/// * Alpha1 value (fraction of H3SiO4- species - deprotonated form with -1 charge)
fn calculate_alpha1_silicate(h: f64, k1sio4: f64, k2sio4: f64) -> f64 {
    1.0 / (1.0 + h / k1sio4 + k2sio4 / h)
}

/// Calculate alpha2 for silicate system (H2SiO4^2-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `k1sio4` - First dissociation constant for silicic acid
/// * `k2sio4` - Second dissociation constant for silicic acid
///
/// # Returns
/// * Alpha2 value (fraction of H2SiO4^2- species - deprotonated with -2 charge)
fn calculate_alpha2_silicate(h: f64, k1sio4: f64, k2sio4: f64) -> f64 {
    1.0 / (1.0 + h / k2sio4 + (h * h) / (k1sio4 * k2sio4))
}

/// Calculate alpha1 for acetate system (CH3COO-)
///
/// # Arguments
/// * `h` - Hydrogen ion concentration [H+]
/// * `kch3coo` - Dissociation constant for acetic acid
///
/// # Returns
/// * Alpha1 value (fraction of CH3COO- species - deprotonated form with -1 charge)
fn calculate_alpha1_acetate(h: f64, kch3coo: f64) -> f64 {
    1.0 / (1.0 + h / kch3coo)
}

// ============================================================================
// Activity coefficient and equilibrium constant functions
// ============================================================================

/// Structure to hold dissociation constant data
#[derive(Debug, Clone)]
struct DisconData {
    k: f64,      // Equilibrium constant
    deltah: f64, // Standard enthalpy in J/mol
}

/// Get dissociation constants data
/// Based on data from Benjamin (2015) Appendix A.1 and A.2
fn get_discons_data() -> std::collections::HashMap<&'static str, DisconData> {
    let mut discons = std::collections::HashMap::new();

    // Carbonic acid: H2CO3<-->HCO3- + H+; HCO3<-->CO32- + H+
    discons.insert("k1co3", DisconData { k: 10_f64.powf(-6.35), deltah: 7700.0 });
    discons.insert("k2co3", DisconData { k: 10_f64.powf(-10.33), deltah: 14900.0 });

    // Sulfate: HSO4- <--> H+ + SO42-
    discons.insert("kso4", DisconData { k: 10_f64.powf(-1.99), deltah: -21900.0 });

    // Phosphate: H3PO4<-->H+ + H2PO4-; H2PO4-<-->H+ + HPO42-; HPO42--<-->H+ + PO43-
    discons.insert("k1po4", DisconData { k: 10_f64.powf(-2.16), deltah: -8000.0 });
    discons.insert("k2po4", DisconData { k: 10_f64.powf(-7.20), deltah: 4200.0 });
    discons.insert("k3po4", DisconData { k: 10_f64.powf(-12.35), deltah: 14700.0 });

    // Hypochlorite: HOCl<-->H+ + OCl-
    discons.insert("kocl", DisconData { k: 10_f64.powf(-7.53), deltah: 13800.0 });

    // Ammonia: NH4+ <--> NH3 + H+
    discons.insert("knh4", DisconData { k: 10_f64.powf(-9.244), deltah: 52210.0 });

    // Borate: H4BO4- <--> H3BO3 + OH-
    discons.insert("kbo3", DisconData { k: 10_f64.powf(-9.24), deltah: -42000.0 });

    // Silicate: H3SiO4- <--> H2SiO42- + H+; H2SiO42- <--> HSiO43- + H+
    discons.insert("k1sio4", DisconData { k: 10_f64.powf(-9.84), deltah: 25600.0 });
    discons.insert("k2sio4", DisconData { k: 10_f64.powf(-13.2), deltah: 37000.0 });

    // Acetate: CH3COOH <--> H+ + CH3COO-
    discons.insert("kch3coo", DisconData { k: 10_f64.powf(-4.757), deltah: -200.0 });

    discons
}

/// Calculate activity coefficients using Davies equation
///
/// Based on equation 5-43 from Davies (1967), Crittenden et al. (2012)
///
/// # Arguments
/// * `z` - Charge of ions in the solution
/// * `ionic_strength` - Ionic strength of the solution (M)
/// * `temp` - Temperature of the solution in Celsius
///
/// # Returns
/// * Activity coefficient value
fn calculate_activity(z: f64, ionic_strength: Option<f64>, temp: f64) -> f64 {
    match ionic_strength {
        Some(is) if !is.is_nan() => {
            let temp_abs = temp + 273.15; // absolute temperature (K)

            // Dielectric constant (relative permittivity) based on temperature
            // from Harned and Owen (1958), Crittenden et al. (2012) equation 5-45
            let de = 78.54 * (1.0 - (0.004579 * (temp_abs - 298.0))
                + 11.9e-6 * (temp_abs - 298.0).powi(2)
                + 28e-9 * (temp_abs - 298.0).powi(3));

            // Constant for use in calculating activity coefficients
            // from Stumm and Morgan (1996), Trussell (1998), Crittenden et al. (2012) equation 5-44
            let a = 1.29e6 * (2_f64.sqrt() / (de * temp_abs).powf(1.5));

            // Davies equation, Davies (1967), Crittenden et al. (2012) equation 5-43
            let activity = 10_f64.powf(-a * z * z * ((is.sqrt() / (1.0 + is.sqrt())) - 0.3 * is));
            activity
        }
        _ => 1.0, // Return 1.0 if ionic strength is None or NaN
    }
}

/// Temperature correction for equilibrium constants using van't Hoff equation
///
/// From Crittenden et al. (2012) equation 5-68 and Benjamin (2010) equation 2-17
/// Assumes delta H for a reaction doesn't change with temperature, valid for ~0-30 deg C
///
/// # Arguments
/// * `deltah` - Standard enthalpy of reaction (J/mol)
/// * `ka` - Equilibrium constant at 25°C
/// * `temp` - Temperature in Celsius
///
/// # Returns
/// * Temperature-corrected equilibrium constant
fn k_temp_adjust(deltah: f64, ka: f64, temp: f64) -> f64 {
    let r = 8.314; // J/mol * K
    let temp_abs = temp + 273.15;
    let ln_k = ka.ln();
    ((deltah / r * (1.0 / 298.15 - 1.0 / temp_abs)) + ln_k).exp()
}

/// Structure to hold corrected equilibrium constants
#[derive(Debug, Clone)]
struct CorrectedConstants {
    k1co3: f64,
    k2co3: f64,
    k1po4: f64,
    k2po4: f64,
    k3po4: f64,
    kocl: f64,
    knh4: f64,
    kso4: f64,
    kbo3: f64,
    k1sio4: f64,
    k2sio4: f64,
    kch3coo: f64,
}

/// Correct dissociation constants for temperature and ionic strength
///
/// Dissociation constants corrected for non-ideal solutions following Benjamin (2010) example 3.14.
///
/// # Arguments
/// * `temp` - Temperature in Celsius
/// * `ionic_strength` - Ionic strength of the solution (M), None if not available
///
/// # Returns
/// * CorrectedConstants structure with temperature and activity-corrected equilibrium constants
fn correct_k(temp: f64, ionic_strength: Option<f64>) -> CorrectedConstants {
    // Determine activity coefficients
    let (activity_z1, activity_z2, activity_z3) = match ionic_strength {
        Some(is) if !is.is_nan() => (
            calculate_activity(1.0, Some(is), temp),
            calculate_activity(2.0, Some(is), temp),
            calculate_activity(3.0, Some(is), temp),
        ),
        _ => (1.0, 1.0, 1.0),
    };

    let discons = get_discons_data();

    // Calculate temperature and activity corrected equilibrium constants
    // k1co3 = {h+}{hco3-}/{h2co3}
    let k1co3 = k_temp_adjust(discons["k1co3"].deltah, discons["k1co3"].k, temp) / (activity_z1 * activity_z1);

    // k2co3 = {h+}{co32-}/{hco3-}
    let k2co3 = k_temp_adjust(discons["k2co3"].deltah, discons["k2co3"].k, temp) / activity_z2;

    // kso4 = {h+}{so42-}/{hso4-} Only one relevant dissociation for sulfuric acid in natural waters.
    let kso4 = k_temp_adjust(discons["kso4"].deltah, discons["kso4"].k, temp) / activity_z2;

    // k1po4 = {h+}{h2po4-}/{h3po4}
    let k1po4 = k_temp_adjust(discons["k1po4"].deltah, discons["k1po4"].k, temp) / (activity_z1 * activity_z1);

    // k2po4 = {h+}{hpo42-}/{h2po4-}
    let k2po4 = k_temp_adjust(discons["k2po4"].deltah, discons["k2po4"].k, temp) / activity_z2;

    // k3po4 = {h+}{po43-}/{hpo42-}
    let k3po4 = k_temp_adjust(discons["k3po4"].deltah, discons["k3po4"].k, temp) * activity_z2 / (activity_z1 * activity_z3);

    // kocl = {h+}{ocl-}/{hocl}
    let kocl = k_temp_adjust(discons["kocl"].deltah, discons["kocl"].k, temp) / (activity_z1 * activity_z1);

    // knh4 = {h+}{nh3}/{nh4+}
    let knh4 = k_temp_adjust(discons["knh4"].deltah, discons["knh4"].k, temp) / (activity_z1 * activity_z1);

    // kbo3 = {oh-}{h3bo3}/{h4bo4-}
    let kbo3 = k_temp_adjust(discons["kbo3"].deltah, discons["kbo3"].k, temp) / (activity_z1 * activity_z1);

    // k1sio4 = {h+}{h2sio42-}/{h3sio4-}
    let k1sio4 = k_temp_adjust(discons["k1sio4"].deltah, discons["k1sio4"].k, temp) / (activity_z1 * activity_z1);

    // k2sio4 = {h+}{hsio43-}/{h2sio42-}
    let k2sio4 = k_temp_adjust(discons["k2sio4"].deltah, discons["k2sio4"].k, temp) / activity_z2;

    // kch3coo = {h+}{ch3coo-}/{ch3cooh}
    let kch3coo = k_temp_adjust(discons["kch3coo"].deltah, discons["kch3coo"].k, temp) / (activity_z1 * activity_z1);

    CorrectedConstants {
        k1co3,
        k2co3,
        k1po4,
        k2po4,
        k3po4,
        kocl,
        knh4,
        kso4,
        kbo3,
        k1sio4,
        k2sio4,
        kch3coo,
    }
}

// ============================================================================
// pH solving functions
// ============================================================================

/// Structure to hold water parameters needed for pH solving
#[derive(Debug, Clone)]
struct WaterParams {
    // Basic parameters
    temp: f64,
    ionic_strength: Option<f64>,
    kw: f64,

    // Total concentrations
    tot_po4: f64,
    tot_co3: f64,
    tot_ocl: f64,  // free_chlorine
    tot_nh3: f64,
    tot_ch3coo: f64,

    // Initial ion concentrations
    h2po4_i: f64,
    hpo4_i: f64,
    po4_i: f64,
    ocl_i: f64,
    nh4_i: f64,
    ch3coo_i: f64,
    carbonate_alk_eq: f64,
    oh_i: f64,
    h_i: f64,
}

/// Structure to hold dose parameters
#[derive(Debug, Clone, Default)]
struct DoseParams {
    so4_dose: f64,
    na_dose: f64,
    ca_dose: f64,
    mg_dose: f64,
    cl_dose: f64,
    mno4_dose: f64,
    no3_dose: f64,
}

/// Charge balance equation for pH solving
///
/// This function implements the charge balance equation used in solve_ph
/// Returns the charge imbalance that should equal zero at equilibrium
fn charge_balance_equation(
    h: f64,
    water: &WaterParams,
    doses: &DoseParams,
    ks: &CorrectedConstants,
    gamma1: f64,
) -> f64 {
    // Calculate alpha values for each system
    let alpha1_po4 = calculate_alpha1_phosphate(h, ks.k1po4, ks.k2po4, ks.k3po4);
    let alpha2_po4 = calculate_alpha2_phosphate(h, ks.k1po4, ks.k2po4, ks.k3po4);
    let alpha3_po4 = calculate_alpha3_phosphate(h, ks.k1po4, ks.k2po4, ks.k3po4);

    let alpha1_co3 = calculate_alpha1_carbonate(h, ks.k1co3, ks.k2co3);
    let alpha2_co3 = calculate_alpha2_carbonate(h, ks.k1co3, ks.k2co3);

    let alpha1_ocl = calculate_alpha1_hypochlorite(h, ks.kocl);
    let alpha1_nh3 = calculate_alpha1_ammonia(h, ks.knh4);
    let alpha1_ch3coo = calculate_alpha1_acetate(h, ks.kch3coo);

    // Charge balance equation (same as R implementation)
    water.kw / (h * gamma1 * gamma1) +
        2.0 * doses.so4_dose +
        water.tot_po4 * (alpha1_po4 + 2.0 * alpha2_po4 + 3.0 * alpha3_po4) +
        water.tot_co3 * (alpha1_co3 + 2.0 * alpha2_co3) +
        water.tot_ocl * alpha1_ocl +
        water.tot_ch3coo * alpha1_ch3coo +
        doses.cl_dose +
        doses.mno4_dose +
        doses.no3_dose -
        (h + doses.na_dose + 2.0 * doses.ca_dose + 2.0 * doses.mg_dose +
         water.tot_nh3 * alpha1_nh3) -
        (water.carbonate_alk_eq + water.oh_i) -
        3.0 * water.po4_i - 2.0 * water.hpo4_i - water.h2po4_i - water.ocl_i +
        water.nh4_i + water.h_i
}

/// Solve for pH using root finding (internal implementation)
///
/// This function solves the charge balance equation to find the hydrogen ion concentration,
/// then converts it to pH
///
/// # Arguments
/// * `water` - Water parameters structure
/// * `doses` - Chemical dose parameters (defaults to 0 if not specified)
///
/// # Returns
/// * pH value rounded to 2 decimal places
fn solve_ph_internal(water: &WaterParams, doses: &DoseParams) -> std::result::Result<f64, String> {
    // Get corrected equilibrium constants
    let ks = correct_k(water.temp, water.ionic_strength);

    // Calculate activity coefficient for H+
    let gamma1 = calculate_activity(1.0, water.ionic_strength, water.temp);

    // Define the function to find the root of
    let f = |h: f64| -> f64 {
        charge_balance_equation(h, water, doses, &ks, gamma1)
    };

    // NOTE: This is likely not the ideal method
    // Other solvers were tried and found to be faster, and have better convergence.
    // However, this method was found to give extremely close results to the prior R implementation.
    // In testing across a random sampling of semi-realistic waters we found with this approach had
    // extremely good agreement with the R implementation:
    //  - 94% of results are within 0.001 pH units (essentially identical)
    //  - 100% of results are within 0.01 pH units (excellent agreement)


    // Use Brent's method to find the root (same as R's uniroot)
    // Set up convergency criteria similar to R's uniroot defaults
    let mut convergency = SimpleConvergency {
        eps: 1e-14,      // Similar to R's tol parameter
        max_iter: 1000   // Similar to R's maxiter parameter
    };
    let result = find_root_brent(1e-14, 1.0, f, &mut convergency);

    match result {
        Ok(h_root) => {
            // Convert H+ concentration to pH
            let ph_final = -(h_root * gamma1).log10();
            Ok((ph_final * 100.0).round() / 100.0) // Round to 2 decimal places
        }
        Err(_) => Err("Failed to converge to a solution".to_string()),
    }
}



/// Internal Rust function to solve pH for water chemistry equilibrium
///
/// This function takes individual parameters and performs the pH calculation.
/// It should be called from the R wrapper function.
///
/// @param temp Temperature in Celsius
/// @param ionic_strength Ionic strength in M (optional, use NULL if not available)
/// @param kw Water dissociation constant
/// @param tot_po4 Total phosphate concentration
/// @param tot_co3 Total carbonate concentration
/// @param tot_ocl Total hypochlorite concentration (free chlorine)
/// @param tot_nh3 Total ammonia concentration
/// @param tot_ch3coo Total acetate concentration
/// @param h2po4_i Initial H2PO4- concentration
/// @param hpo4_i Initial HPO4^2- concentration
/// @param po4_i Initial PO4^3- concentration
/// @param ocl_i Initial OCl- concentration
/// @param nh4_i Initial NH4+ concentration
/// @param ch3coo_i Initial CH3COO- concentration
/// @param carbonate_alk_eq Carbonate alkalinity in equivalents
/// @param oh_i Initial OH- concentration
/// @param h_i Initial H+ concentration
/// @param so4_dose Sulfate dose
/// @param na_dose Sodium dose
/// @param ca_dose Calcium dose
/// @param mg_dose Magnesium dose
/// @param cl_dose Chloride dose
/// @param mno4_dose Permanganate dose
/// @param no3_dose Nitrate dose
/// @export
#[extendr]
fn solve_ph_rust_inner(
    temp: f64,
    ionic_strength: Nullable<f64>,
    kw: f64,
    tot_po4: f64,
    tot_co3: f64,
    tot_ocl: f64,
    tot_nh3: f64,
    tot_ch3coo: f64,
    h2po4_i: f64,
    hpo4_i: f64,
    po4_i: f64,
    ocl_i: f64,
    nh4_i: f64,
    ch3coo_i: f64,
    carbonate_alk_eq: f64,
    oh_i: f64,
    h_i: f64,
    so4_dose: f64,
    na_dose: f64,
    ca_dose: f64,
    mg_dose: f64,
    cl_dose: f64,
    mno4_dose: f64,
    no3_dose: f64,
) -> f64 {
    let water = WaterParams {
        temp,
        ionic_strength: ionic_strength.into(),
        kw,
        tot_po4,
        tot_co3,
        tot_ocl,
        tot_nh3,
        tot_ch3coo,
        h2po4_i,
        hpo4_i,
        po4_i,
        ocl_i,
        nh4_i,
        ch3coo_i,
        carbonate_alk_eq,
        oh_i,
        h_i,
    };

    let doses = DoseParams {
        so4_dose,
        na_dose,
        ca_dose,
        mg_dose,
        cl_dose,
        mno4_dose,
        no3_dose,
    };

    match solve_ph_internal(&water, &doses) {
        Ok(ph) => ph,
        Err(msg) => {
            rprintln!("Error in solve_ph_rust_inner: {}", msg);
            f64::NAN
        }
    }
}

extendr_module! {
    mod tidywater;
    fn solve_ph_rust_inner;
}
