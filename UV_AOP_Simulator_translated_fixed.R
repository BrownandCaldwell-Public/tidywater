# Translation of UV_AOP_Simulator.m to R using deSolve::ode

# Parse MATLAB vector assignment: var = [ ... ];
parse_matlab_vector <- function(lines, var_name, occurrence = c("first", "last")) {
  occurrence <- match.arg(occurrence)

  start_idx_all <- grep(paste0("^\\s*", var_name, "\\s*=\\s*\\["), lines)
  if (!length(start_idx_all)) {
    stop(sprintf("Could not find '%s' in MATLAB file.", var_name), call. = FALSE)
  }
  start_idx <- if (occurrence == "last") tail(start_idx_all, 1) else start_idx_all[[1]]

  chunk <- lines[start_idx]
  if (!grepl("\\];", chunk)) {
    end_rel <- grep("\\];", lines[(start_idx + 1):length(lines)])
    if (!length(end_rel)) {
      stop(sprintf("Could not find closing ]; for '%s'.", var_name), call. = FALSE)
    }
    end_idx <- start_idx + end_rel[[1]]
    chunk <- paste(lines[start_idx:end_idx], collapse = " ")
  }

  chunk <- gsub("%.*$", "", chunk)
  chunk <- sub("^[^[]*\\[", "", chunk)
  chunk <- sub("\\];.*$", "", chunk)
  chunk <- gsub("[;,\t\r\n]", " ", chunk)
  chunk <- gsub("\\s+", " ", trimws(chunk))

  vals <- suppressWarnings(as.numeric(strsplit(chunk, " ", fixed = TRUE)[[1]]))
  vals[!is.na(vals)]
}

# Convert one MATLAB scalar expression to R syntax.
matlab_expr_to_r <- function(expr_txt) {
  out <- expr_txt
  out <- gsub("\\.\\*", "*", out)
  out <- gsub("\\./", "/", out)
  out <- gsub("\\.\\^", "^", out)
  out <- gsub("y\\(([0-9]+)\\)", "y[\\1]", out, perl = TRUE)
  out
}

# Parse the 36 MATLAB ODE expressions from the dydt = [ ... ]; block.
parse_matlab_dydt_expressions <- function(lines) {
  start_idx <- grep("^\\s*dydt\\s*=\\s*\\[", lines)
  if (!length(start_idx)) {
    stop("Could not find 'dydt=[' block in MATLAB file.", call. = FALSE)
  }
  start_idx <- start_idx[[1]]

  end_rel <- grep("^\\s*\\];", lines[(start_idx + 1):length(lines)])
  if (!length(end_rel)) {
    stop("Could not find closing '];' for dydt block.", call. = FALSE)
  }
  end_idx <- start_idx + end_rel[[1]]

  block <- lines[(start_idx + 1):(end_idx - 1)]
  block <- gsub("%.*$", "", block)
  block <- trimws(block)
  block <- block[nzchar(block)]
  block <- gsub(";\\s*$", "", block)

  if (length(block) != 36) {
    stop(sprintf("Expected 36 ODE expressions, parsed %d.", length(block)), call. = FALSE)
  }

  vapply(block, matlab_expr_to_r, character(1))
}

# Load MP spectra arrays and ODE expressions from MATLAB source.
load_uv_aop_matlab_data <- function(matlab_file) {
  if (is.null(matlab_file) || !file.exists(matlab_file)) {
    stop("MATLAB file not found.", call. = FALSE)
  }

  lines <- readLines(matlab_file, warn = FALSE)

  mp_spectra <- list(
    Ep0 = parse_matlab_vector(lines, "Ep0", occurrence = "last"),
    E2 = parse_matlab_vector(lines, "E2", occurrence = "last"),
    E3 = parse_matlab_vector(lines, "E3", occurrence = "last"),
    E4 = parse_matlab_vector(lines, "E4", occurrence = "last"),
    E5 = parse_matlab_vector(lines, "E5", occurrence = "last"),
    E6 = parse_matlab_vector(lines, "E6", occurrence = "last")
  )

  lens <- vapply(mp_spectra, length, integer(1))
  if (any(lens != 201)) {
    stop(sprintf("Expected MP spectra length 201, got %s", paste(names(lens), lens, sep = "=", collapse = ", ")), call. = FALSE)
  }

  list(
    mp_spectra = mp_spectra,
    dydt_exprs = parse_matlab_dydt_expressions(lines)
  )
}

UV_AOP <- function(t, y, parms) {
  phiHOCl_OH <- 0.46; phiOCl_OH <- 0.7
  phiH2O2_OH <- 1.01; phiHO2_OH <- 1.0

  phiHOCl_Cl <- phiHOCl_OH; phiOCl_Cl <- phiOCl_OH; phiCl2_Cl <- 1

  phiTarget <- 6.5e-2
  phiProbe1 <- 0; phiProbe2 <- 0; phiProbe3 <- 0; phiProbe4 <- 0
  phiCl2 <- 0.5; phiHOCl <- 1.0; phiOCl <- 0.9; phiH2O2 <- 1.0; phiHO2 <- 1.0

  UV <- y[35]
  if (UV < 0.5) {
    Ep0 <- c(2.94e-09, 2.22e-10)
    E1 <- c(0, 0); E2 <- c(0, 0); E3 <- c(58, 20); E4 <- c(60, 235)
    E5 <- c(19, 0); E6 <- c(269, 1)
    E7 <- c(0, 0); E8 <- c(0, 0); E9 <- c(0, 0); E10 <- c(0, 0)
    B <- c(0, 0)
  } else {
    mp <- parms$mp_spectra
    if (is.null(mp)) stop("MP spectra were not provided in parms$mp_spectra.", call. = FALSE)

    Ep0 <- mp$Ep0
    E1 <- rep(0, 201)
    E2 <- mp$E2; E3 <- mp$E3; E4 <- mp$E4; E5 <- mp$E5; E6 <- mp$E6
    E7 <- rep(0, 201); E8 <- rep(0, 201); E9 <- rep(0, 201); E10 <- rep(0, 201)
    B <- rep(0, 201)
  }

  A <- E1 * y[1] + E2 * y[2] + E3 * y[3] + E4 * y[4] + E5 * y[5] + E6 * y[6] +
    E7 * y[31] + E8 * y[32] + E9 * y[33] + E10 * y[34] + B

  l <- 1.1
  L <- 36.5
  AL <- A * l
  one_minus <- 1 - 10^(-AL)
  WF <- one_minus / (2.3026 * AL)
  WF[abs(AL) < 1e-30] <- 1

  DF <- L / (L + l)
  PF <- 0.9750
  RF <- 0.9749
  Ep <- Ep0 * WF

  spec_rate <- function(Ei) {
    out <- ((Ep / WF) * Ei * one_minus) / AL * 1000
    small <- abs(AL) < 1e-30
    out[small] <- (Ep0[small] * Ei[small] * log(10)) * 1000
    out
  }

  R1 <- spec_rate(E1); R2 <- spec_rate(E2); R3 <- spec_rate(E3); R4 <- spec_rate(E4); R5 <- spec_rate(E5)
  R6 <- spec_rate(E6); R7 <- spec_rate(E7); R8 <- spec_rate(E8); R9 <- spec_rate(E9); R10 <- spec_rate(E10)

  sumR1 <- sum(R1); sumR2 <- sum(R2); sumR3 <- sum(R3); sumR4 <- sum(R4); sumR5 <- sum(R5)
  sumR6 <- sum(R6); sumR7 <- sum(R7); sumR8 <- sum(R8); sumR9 <- sum(R9); sumR10 <- sum(R10)

  OHFormation <- phiHOCl_OH * sumR3 * y[3] + phiOCl_OH * sumR4 * y[4] + phiH2O2_OH * sumR5 * y[5] + phiHO2_OH * sumR6 * y[6]
  ClFormation <- phiCl2_Cl * sumR2 * y[2] + phiHOCl_Cl * sumR3 * y[3] + phiOCl_Cl * sumR4 * y[4]

  TargetPhotolysis <- phiTarget * sumR1 * y[1]
  Cl2Photolysis <- phiCl2 * sumR2 * y[2]
  HOClPhotolysis <- phiHOCl * sumR3 * y[3]
  OClPhotolysis <- phiOCl * sumR4 * y[4]
  H2O2Photolysis <- phiH2O2 * sumR5 * y[5]
  HO2Photolysis <- phiHO2 * sumR6 * y[6]

  Probe1Photolysis <- phiProbe1 * sumR7 * y[7]
  Probe2Photolysis <- phiProbe2 * sumR8 * y[8]
  Probe3Photolysis <- phiProbe3 * sumR9 * y[9]
  Probe4Photolysis <- phiProbe4 * sumR10 * y[10]

  kforward_Cl <- 11; kbackward_Cl <- 2.79e-4
  kforward_HOCl <- 1.8e3; kbackward_HOCl <- 3e9
  kforward_HP <- 0.126; kbackward_HP <- 5e10
  kw <- 1.9055e-14
  kforward_Carb <- 2.345; kbackward_Carb <- 5e10

  k4 <- 8.46e4; k5 <- 8.8e9; k6 <- 3.00e8; k7 <- 1.56e9; k9 <- 8.5e6; k10 <- 3.9e8
  k11 <- 2.1e10; k12 <- 2.5e5; k13 <- 8.5e9; k14 <- 2.2e8; k15 <- 5.0e8; k16 <- 1.0e4
  k17 <- 4.5e7; k18 <- 8.0e7; k19 <- 1.6e8; k20 <- 4.3e9; k21 <- 6.1e9; k22 <- 1.8e10
  k23 <- 8.8e7; k24 <- 2.1e9; k25 <- 6.0e4; k26 <- 9.0e8; k27 <- 1.0e9; k28 <- 3.0e9
  k29 <- 8.3e9; k30 <- 6.0e2; k31 <- 3.0e9; k32 <- 5.1e5; k33 <- 3.0e7; k34 <- 1.3e3
  k35 <- 1.0e8; k36 <- 1.0e2; k37 <- 5.0e9; k38 <- 0; k39 <- 1.56e8; k40 <- 0; k41 <- 1.56e8
  k42 <- 0; k43 <- 1.56e8; k44 <- 0; k45 <- 1.56e8; k46 <- 1e6; k47 <- 6e5; k48 <- 1.2e10
  k49 <- 1.8e6; k50 <- 0; k51 <- 0; k52 <- 0.13; k53 <- 4.0e8; k54 <- 5e10; k55 <- 3.2e7
  k56 <- 4.0e8; k57 <- 3.6e9; k58 <- 2.6e3; k59 <- 1e10; k60 <- 7.5e9; k61 <- 6.6e9; k62 <- 7e9
  k63 <- 8.3e5; k64 <- 3e0; k65 <- 9.7e7; k66 <- 1.6e5; k67 <- 1.1e4; k68 <- 7.5e6; k69 <- 7.5e6
  k70 <- 2.0e9; k71 <- 1.4e5; k72 <- 3.1e9; k73 <- 2.0e9; k74 <- 1.7e5; k75 <- 2.0e8; k76 <- 4.3e5
  k77 <- 3.0e7; k78 <- 3.0e7; k79 <- 6.0e8; k80 <- 1e9; k81 <- 4e9; k82 <- 1.3e8; k83 <- 9.4e8
  k84 <- 3.4e7; k85 <- 6.3e9; k86 <- 1.0e11; k87 <- 2.5e9; k88 <- 5.5e9; k89 <- 0.36
  k94 <- 0; k95 <- 1.56e8; k96 <- 0; k97 <- 0; k98 <- 0; k99 <- 0; k100 <- 0; k101 <- 0
  k102 <- 0; k103 <- 0; k104 <- 6.9e9; k105 <- 1.46e10; k106 <- 0; k107 <- 5.1e7; k108 <- 0
  k109 <- 0; k110 <- 0; k111 <- 0; k112 <- 0; k113 <- 0; k114 <- 1e4; k115 <- 0; k116 <- 0
  k117 <- 0; k118 <- 0; k119 <- 0; k120 <- 0; k121 <- 0; k122 <- 0; k123 <- 0; k124 <- 0
  k125 <- 0; k126 <- 0; k127 <- 0; k128 <- 0; k129 <- 0; k130 <- 0; k131 <- 0; k132 <- 0
  k133 <- 0; k134 <- 0; k135 <- 0; k136 <- 0; k137 <- 0; k138 <- 0; k139 <- 0; k140 <- 0
  k141 <- 0; k142 <- 0; k143 <- 0; k144 <- 0; k145 <- 0; k146 <- 0; k147 <- 0; k148 <- 0
  k149 <- 0; k150 <- 0; k151 <- 0; k152 <- 0; k153 <- 0; k154 <- 0; k155 <- 0; k156 <- 0
  k157 <- 0; k158 <- 0; k159 <- 0; k160 <- 0; k161 <- 0; k162 <- 0; k163 <- 1.5e5; k164 <- 2e4

  pH <- -log10(y[14])

  dydt <- vapply(parms$dydt_exprs, function(expr_txt) {
    eval(parse(text = expr_txt), envir = environment())
  }, numeric(1))

  list(dydt)
}

UV_AOP_Simulator_translated <- function(
  pH = 5,
  UV = 0,
  Cl2_mgL = 10,
  H2O2_mgL = 0,
  tspan = seq(0, 1480, by = 370),
  target_ugL = 100,
  MW_target = 397.64,
  probe1_molesL = 4.83e-7,
  probe2_molesL = 0,
  probe3_molesL = 0,
  probe4_molesL = 0,
  DOC_mgL = 3,
  TIC_molesL = 1.131e-5,
  Cl_mgL = 0,
  Phos_Buf_Cap = 0.005,
  matlab_file = NULL
) {
  if (is.null(matlab_file)) {
    matlab_candidates <- c(
      "C:/Users/AFretz/OneDrive - Brown and Caldwell/GRP_tidywater - UVAOP/Nathan Model Code/UV_AOP_Simulator.m",
      "C:/Users/AFretz/Downloads/UV_AOP_Simulator.m"
    )
    matlab_file <- matlab_candidates[file.exists(matlab_candidates)][1]
  }

  matlab_data <- load_uv_aop_matlab_data(matlab_file)

  K_H <- 10^-1.5
  P_CO2 <- 10^-3.5
  H <- 10^-pH
  kw <- 1.9055e-14
  OH <- kw / H

  Cl2_molesL <- Cl2_mgL / (71 * 1000)
  HOCl <- Cl2_molesL / (1 + 10^(pH - 7.52))
  OCl <- Cl2_molesL - Cl2_molesL / (1 + 10^(pH - 7.52))

  H2O2_molesL <- H2O2_mgL / (34 * 1000)
  H2O2 <- H2O2_molesL / (1 + 10^(pH - 11.6))
  HO2 <- H2O2_molesL - H2O2_molesL / (1 + 10^(pH - 11.6))

  DOC_molesL <- DOC_mgL / (1000 * 12)
  target_molesL <- target_ugL / (MW_target * 1e6)

  K1 <- 10^-6.35
  K2 <- 10^-10.33
  Den <- H * H + H * K1 + K1 * K2
  H2CO3 <- TIC_molesL * H * H / Den
  HCO3 <- TIC_molesL * H * K1 / Den
  CO3 <- TIC_molesL * K1 * K2 / Den

  Cl <- Cl_mgL * (1 / 1000) * (1 / 35.453)
  H2PO4 <- Phos_Buf_Cap / (1 + 10^(pH - 7.2))
  HPO4 <- Phos_Buf_Cap - H2PO4

  y0 <- c(
    target_molesL, 0, HOCl, OCl, H2O2, HO2, DOC_molesL, HCO3, CO3, Cl,
    H2PO4, HPO4, 0, H, OH, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    probe1_molesL, probe2_molesL, probe3_molesL, probe4_molesL, UV, 0
  )

  out <- deSolve::ode(
    y = y0,
    times = tspan,
    func = UV_AOP,
    parms = list(
      mp_spectra = matlab_data$mp_spectra,
      dydt_exprs = matlab_data$dydt_exprs
    ),
    method = "lsoda"
  )

  out_df <- as.data.frame(out)
  colnames(out_df) <- c(
    "time", "target", "Cl2", "HOCl", "OCl", "H2O2", "HO2", "DOC", "HCO3", "CO3",
    "Cl", "H2PO4", "HPO4", "O2", "H", "OH", "HClOH", "ClO2", "ClO2_minus", "ClO3_minus",
    "OH_rad", "Cl_rad", "Cl2_rad", "ClO_rad", "ClOH_rad", "ClO2_rad", "O_rad", "O2_rad",
    "O3_rad", "HO2_rad", "CO3_rad", "probe1", "probe2", "probe3", "probe4", "UV", "HPO4_rad"
  )

  list(
    inputs = list(
      pH = pH, UV = UV, Cl2_mgL = Cl2_mgL, H2O2_mgL = H2O2_mgL,
      target_ugL = target_ugL, MW_target = MW_target, DOC_mgL = DOC_mgL,
      TIC_molesL = TIC_molesL, Cl_mgL = Cl_mgL, Phos_Buf_Cap = Phos_Buf_Cap,
      K_H = K_H, P_CO2 = P_CO2, matlab_file = matlab_file
    ),
    y0 = y0,
    results = out_df
  )
}
