# Chlorine/Chloramine Breakpoint Curve Simulator Functions
# This function carries out the Chlorine Breakpoint calculation, predicting the residual chlorine and chloramine concentrations

#' @title Calculate chlorine and chloramine Concentrations with the breakpoint cblorination approach
#'
#' @description [chemdose_chloramine], adopted from the U.S. EPA's Chlorine Breakpoint Curve Simulator,
#' calculates chlorine and chloramine concentrations based on the two papers Jafvert & Valentine
#' (Environ. Sci. Technol., 1992, 26 (3), pp 577-586) and Vikesland et al. (Water Res., 2001, 35 (7), pp 1766-1776).
#' Required arguments include an object of class "water" created by [define_water], chlorine dose, and reaction time.
#' The function also requires additional water quality parameters defined in [define_water]
#' including temperature, pH, and alkalinity.
#' For a single water use `chemdose_chloramine`; for a dataframe use `chemdose_chloramine_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by [define_water]
#' @param time Reaction time (minutes). Time defined needs to be greater or equal to 1 minute.
#' @param cl2 Applied chlorine dose (mg/L as Cl2), defaults to 0.If not specified, use free_chlorine slot in water.
#' @param nh3 Applied ammonia dose (mg/L as N), defaults to 0. If not specified, use tot_nh3 slot in water.
#' @param use_free_cl_slot Defaults to FALSE. If TRUE, uses free_chlorine slot in water. If TRUE AND there is a cl2 input, both the free_chlorine water slot and chlorine dose will be used.
#' @param use_tot_nh3_slot Defaults to FALSE. If TRUE, uses tot_nh3 slot in water. If TRUE AND there is a nh3 input, both the tot_nh3 water slot and ammonia dose will be used.
#' 
#' @examples
#' breakpoint <- define_water(7.5, 20, 65, free_chlorine = 5, tot_nh3 = 1) %>%
#'   chemdose_chloramine(time = 40, cl2 = 2, nh3 = 1, use_free_cl_slot = TRUE)
#'
#' @importFrom deSolve ode
#' @importFrom utils tail
#' @export
#'
#' @returns `chemdose_chloramine` returns a water class object with predicted chlorine and chloramine concentrations.
#'
chemdose_chloramine <- function(
  water, 
  time, 
  cl2 = 0, 
  nh3 = 0, 
  use_free_cl_slot = FALSE, 
  use_tot_nh3_slot = FALSE) {
  
  validate_water(water, c("ph", "alk", "temp"))

  if (missing(time)) {
    cli::cli_abort("Missing value for {.arg time}. Please check the inputs required to run {.fn chemdose_chloramine}.")
  }

  if (time < 1) {
    cli::cli_abort("{.arg time} is less than 1 minute; please set {.arg time} >= 1.")
  }

  if (missing(cl2)) {
    validate_water(water, "free_chlorine")
    cl2 <- water@free_chlorine
    TOTCl_ini <- cl2
    if (!use_free_cl_slot) {
      tidywater_warn("Chlorine dose not specified; function used {.field free_chlorine} 
      slot ({water@free_chlorine} mol/L) in given water as the initial free chlorine.")
    }
  } else if (!use_free_cl_slot) {
    TOTCl_ini <- convert_units(cl2, "cl2")
    if (water@free_chlorine > 0) {
      tidywater_warn(c("Chlorine dose was used; {.field free_chlorine} slot 
      ({water@free_chlorine} mol/L) was ignored.", "i" = "To use ONLY the water slot, 
      set {.code use_free_cl_slot = TRUE} and remove {.arg cl2}.", "i" = "To use BOTH, 
      set {.code use_free_cl_slot = TRUE}."))
    }
  } else if (use_free_cl_slot) {
    validate_water(water, "free_chlorine")
    TOTCl_ini <- water@free_chlorine + convert_units(cl2, "cl2")

    if (cl2 > 0) {
      tidywater_warn(c("Both {.arg cl2} dose and {.field free_chlorine} slot 
      ({water@free_chlorine} mol/L) were used.", "i" = "To use ONLY the dose, 
      set {.code use_free_cl_slot = FALSE}.", "i" = "To use ONLY the water slot, 
      remove {.arg cl2}."))
    }
  }

  if (missing(nh3)) {
    validate_water(water, "tot_nh3")
    nh3 <- water@tot_nh3
    TOTNH_ini <- nh3
    if (!use_tot_nh3_slot) {
      tidywater_warn("Ammonia dose not specified; using {.field tot_nh3} slot 
      ({water@tot_nh3} mol/L) as the initial free ammonia.")
    }
  } else if (!use_tot_nh3_slot) {
    TOTNH_ini <- convert_units(nh3, "n")
    if (water@tot_nh3 > 0) {
      tidywater_warn(c("Ammonia dose was used; {.field tot_nh3} slot 
      ({water@tot_nh3} mol/L) was ignored.", "i" = "To use ONLY the water slot, 
      set {.code use_tot_nh3_slot = TRUE} and remove {.arg nh3}.", 
      "i" = "To use BOTH, set {.code use_tot_nh3_slot = TRUE}."))
    }
  } else if (use_tot_nh3_slot) {
    validate_water(water, "tot_nh3")
    TOTNH_ini <- water@tot_nh3 + convert_units(nh3, "n")
    if (nh3 > 0) {
      tidywater_warn(c("Both {.arg nh3} dose and {.field tot_nh3} slot 
      ({water@tot_nh3} mol/L) were used.", "i" = "To use ONLY the dose, 
      set {.code use_tot_nh3_slot = FALSE}.", "i" = "To use ONLY the water slot, 
      remove {.arg nh3}."))
    }
  }

  if (!is.na(water@nh2cl) | !is.na(water@nhcl2) | !is.na(water@ncl3)) {
    tidywater_warn("Chloramine species present in water class object. Check slots 
    {.field nh2cl}, {.field nhcl2}, {.field ncl3}. Present concentrations will be used 
    as initial values.")
  }

  if (water@combined_chlorine != 0) {
    tidywater_warn("Chloramine present in water as {.field combined_chloramine}; 
    this slot will be overridden.")
  }

  time <- time * 60
  ph <- water@ph
  alk <- water@alk
  temp <- water@temp
  T_K <- temp + 273.15

  # in mg/L
  # CltoN_Mass <- convert_units(TOTCl_ini,'cl2','M','mg/L')/convert_units(TOTNH_ini,'n','M','mg/L')

  discons <- tidywater::discons
  # Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5)) # 10^-7.6
  KW <-  K_temp_adjust(discons["water", ]$deltah, discons["water", ]$k, temp)
  H <- 10^-ph
  OH <- KW / H

  # Calculate alpha values
  alpha0TOTCl <- 1 / (1 + KHOCl / H)
  alpha1TOTCl <- 1 / (1 + H / KHOCl)
  
  alpha0TOTCO <- calculate_alpha0_carbonate(H, water@ks)
  alpha1TOTCO <- calculate_alpha1_carbonate(H, water@ks)
  alpha2TOTCO <- calculate_alpha2_carbonate(H, water@ks)

  calculate_alpha0_ammonia <- function(h, k) {
    # NH3
    k1 <- k$knh4
    calculate_alpha1_ammonia(h, k) * k1 / h
  }

  # Note alphas_ammonia from original script, flipped from our definition (alpha0 <--> alpha1)
  # alpha0TOTNH <- 1/(1 + water@ks$knh4/H)
  # alpha1TOTNH <- 1/(1 + H/ks$knh4)
  alpha0TOTNH <- calculate_alpha0_ammonia(H, water@ks)
  alpha1TOTNH <- calculate_alpha1_ammonia(H, water@ks)

  # Calculate total carbonate concentration (moles/L)
  TOTCO <- (alk / 50000 + H - OH) / (alpha1TOTCO + 2 * alpha2TOTCO)

  # Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO * TOTCO
  HCO3 <- alpha1TOTCO * TOTCO
  CO3 <- alpha2TOTCO * TOTCO

  # Calculated rate constants (moles/L and seconds) adjusted for temperature # chloramine rate constants (leave as is, or add in dataframe)
  k1 <- 6.6e8 * exp(-1510 / T_K) # 4.2e6
  k2 <- 1.38e8 * exp(-8800 / T_K) # 2.1e-5
  k3 <- 3.0e5 * exp(-2010 / T_K) # 2.8e2        % -2080
  k4 <- 6.5e-7
  k5H <- 1.05e7 * exp(-2169 / T_K) # 6.9e3        % off by a bit
  k5HCO3 <- 4.2e31 * exp(-22144 / T_K) # 2.2e-1       % off by a bit
  k5H2CO3 <- 8.19e6 * exp(-4026 / T_K) # 1.1e1
  k5 <- k5H * H + k5HCO3 * HCO3 + k5H2CO3 * H2CO3
  k6 <- 6.0e4
  k7 <- 1.1e2
  k8 <- 2.8e4
  k9 <- 8.3e3
  k10 <- 1.5e-2
  k11p <- 3.28e9 * OH + 6.0e6 * CO3 # double check this and below
  k11OCl <- 9e4
  k12 <- 5.56e10
  k13 <- 1.39e9
  k14 <- 2.31e2

  if (is.na(water@nh2cl)) {
    water@nh2cl <- 0
  }
  if (is.na(water@nhcl2)) {
    water@nhcl2 <- 0
  }
  if (is.na(water@ncl3)) {
    water@ncl3 <- 0
  }

  # Define function for chloramine system
  chloramine <- function(t, y, parms) {
    with(as.list(y), {
      dTOTNH <- (-k1 *
        alpha0TOTCl *
        TOTCl *
        alpha1TOTNH *
        TOTNH +
        k2 * NH2Cl +
        k5 * NH2Cl^2 -
        k6 * NHCl2 * alpha1TOTNH * TOTNH * H)
      dTOTCl <- (-k1 *
        alpha0TOTCl *
        TOTCl *
        alpha1TOTNH *
        TOTNH +
        k2 * NH2Cl -
        k3 * alpha0TOTCl * TOTCl * NH2Cl +
        k4 * NHCl2 +
        k8 * I * NHCl2 -
        (k11p + k11OCl * alpha1TOTCl * TOTCl) * alpha0TOTCl * TOTCl * NHCl2 +
        2 * k12 * NHCl2 * NCl3 * OH +
        k13 * NH2Cl * NCl3 * OH -
        2 * k14 * NHCl2 * alpha1TOTCl * TOTCl)
      dNH2Cl <- (k1 *
        alpha0TOTCl *
        TOTCl *
        alpha1TOTNH *
        TOTNH -
        k2 * NH2Cl -
        k3 * alpha0TOTCl * TOTCl * NH2Cl +
        k4 * NHCl2 -
        2 * k5 * NH2Cl^2 +
        2 * k6 * NHCl2 * alpha1TOTNH * TOTNH * H -
        k9 * I * NH2Cl -
        k10 * NH2Cl * NHCl2 -
        k13 * NH2Cl * NCl3 * OH)
      # add in nitrite-/bromide-induced dNH2Cl loss

      dNHCl2 <- (k3 *
        alpha0TOTCl *
        TOTCl *
        NH2Cl -
        k4 * NHCl2 +
        k5 * NH2Cl^2 -
        k6 * NHCl2 * alpha1TOTNH * TOTNH * H -
        k7 * NHCl2 * OH -
        k8 * I * NHCl2 -
        k10 * NH2Cl * NHCl2 -
        (k11p + k11OCl * alpha1TOTCl * TOTCl) * alpha0TOTCl * TOTCl * NHCl2 -
        k12 * NHCl2 * NCl3 * OH -
        k14 * NHCl2 * alpha1TOTCl * TOTCl)
      dNCl3 <- ((k11p + k11OCl * alpha1TOTCl * TOTCl) *
        alpha0TOTCl *
        TOTCl *
        NHCl2 -
        k12 * NHCl2 * NCl3 * OH -
        k13 * NH2Cl * NCl3 * OH)
      dI <- (k7 * NHCl2 * OH - k8 * I * NHCl2 - k9 * I * NH2Cl)
      list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI))
    })
  }

  I_ini <- 0

  yin <- c(
    TOTNH = TOTNH_ini,
    TOTCl = TOTCl_ini,
    # assume chloramines are in the form of mol cl2/L
    NH2Cl = water@nh2cl,
    NHCl2 = water@nhcl2,
    NCl3 = water@ncl3,
    I = I_ini
  )

  # Solver of ODE System
  deSolve::ode
  out <- as.data.frame(ode(
    func = chloramine, # revisit as.data.frame vs. data.frame
    parms = NULL,
    y = yin,
    times = seq(0, time, by = 60), # read ode function
    atol = 1e-12,
    rtol = 1e-12
  ))

  sim_data <- tail(out, n = 1)

  # Note that some values turn out to be less than 0 and just oscillate around 0 as the ode calculates, may be set to NA
  sim_data[sim_data < 0] <- 0

  # All concentrations start in moles/L Cl2 or N
  water@nh2cl <- sim_data$NH2Cl
  water@nhcl2 <- sim_data$NHCl2
  water@ncl3 <- sim_data$NCl3
  water@free_chlorine <- sim_data$TOTCl

  # Ammonia from model is free. Calculate total for water. (1 mol NH3 per mol each chloramine).
  water@tot_nh3 <- sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3
  water@combined_chlorine <- water@nh2cl + water@nhcl2 + water@ncl3

  return(water)
}


#' @rdname chemdose_chloramine
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df].
#' The df may include a column named for the applied chlorine dose (cl2_dose), and a column for time in hours.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "chloraminated".
#' @param pluck_cols Extract water slots modified by the function ("free_chlorine", "nh2cl", "nhcl2", "ncl3", "combined_chlorine", "tot_nh3") into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#' @param keep_dose_cols Option to keep the original chemical dose columns in the output data frame. Default is FALSE.
#'
#' @examples
#' \donttest{
#' breakpoint <- water_df %>%
#'   dplyr::mutate(free_chlorine = 5, tot_nh3 = 1) %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     time = 8,
#'     cl2dose = rep(c(2, 3, 4), 4)
#'   ) %>%
#'   chemdose_chloramine_df(
#'     output_water = "final",
#'     cl2 = cl2dose,
#'     use_free_cl_slot = TRUE,
#'     use_tot_nh3_slot = TRUE,
#'     pluck_cols = TRUE
#'   )
#' }
#'
#' @export
#'
#' @returns `chemdose_chloramine_df` returns a data frame containing water class column with updated chlorine/chloramine slots:
#' free_chlorine, nh2cl, nhcl2, ncl3, combined_chlorine, tot_nh3. Optionally, it also adds columns for each of those slots individually.
#'
chemdose_chloramine_df <- function(
  df,
  input_water = "defined",
  output_water = "chloraminated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  keep_dose_cols = FALSE,
  time = "use_col",
  cl2 = "use_col",
  nh3 = "use_col",
  use_free_cl_slot = "use_col",
  use_tot_nh3_slot = "use_col"
) {
  validate_water_helpers(df, input_water)

  time <- tryCatch(time, error = function(e) enquo(time))
  cl2 <- tryCatch(cl2, error = function(e) enquo(cl2))
  nh3 <- tryCatch(nh3, error = function(e) enquo(nh3))
  use_free_cl_slot <- tryCatch(use_free_cl_slot, error = function(e) enquo(use_free_cl_slot))
  use_tot_nh3_slot <- tryCatch(use_tot_nh3_slot, error = function(e) enquo(use_tot_nh3_slot))

  arguments <- construct_helper(
    df,
    list(
      "cl2" = cl2,
      "nh3" = nh3,
      "time" = time,
      "use_free_cl_slot" = use_free_cl_slot,
      "use_tot_nh3_slot" = use_tot_nh3_slot
    )
  )

  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df,
    final_names,
    list(cl2 = 0, nh3 = 0, use_free_cl_slot = FALSE, use_tot_nh3_slot = FALSE)
  )
  df <- defaults_added$data

warning_counts <-list()

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(
      chemdose_chloramine(
        water = df[[input_water]][[i]],
        cl2 = df[[final_names$cl2]][i],
        nh3 = df[[final_names$nh3]][i],
        time = df[[final_names$time]][i],
        use_free_cl_slot = df[[final_names$use_free_cl_slot]][i],
      use_tot_nh3_slot = df[[final_names$use_tot_nh3_slot]][i]
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) + 1L
      invokeRestart("muffleWarning")
    }
  )
  })

  for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("free_chlorine", "nh2cl", "nhcl2", "ncl3", "combined_chlorine", "tot_nh3"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }
 if (!keep_dose_cols){
    
    output<-output[,!colnames(output) %in% ( c("cl2", "nh3"))]

   }

  return(output)
}
