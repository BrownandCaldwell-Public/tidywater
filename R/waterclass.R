# Create water class
methods::setClass(
  "water",
  representation(
    # General parameters
    ph = "numeric",
    temp = "numeric",
    alk = "numeric",
    tds = "numeric",
    cond = "numeric",
    tot_hard = "numeric",
    kw = "numeric",
    alk_eq = "numeric",
    carbonate_alk_eq = "numeric",
    phosphate_alk_eq = "numeric",
    ammonium_alk_eq = "numeric",
    borate_alk_eq = "numeric",
    silicate_alk_eq = "numeric",
    hypochlorite_alk_eq = "numeric",

    # Carbon
    toc = "numeric",
    doc = "numeric",
    bdoc = "numeric",
    uv254 = "numeric",
    dic = "numeric",

    # Ions
    is = "numeric",
    na = "numeric",
    ca = "numeric",
    mg = "numeric",
    k = "numeric",
    cl = "numeric",
    so4 = "numeric",
    mno4 = "numeric",
    no3 = "numeric",
    # pH dependent ions
    h2co3 = "numeric",
    hco3 = "numeric",
    co3 = "numeric",
    h2po4 = "numeric",
    hpo4 = "numeric",
    po4 = "numeric",
    nh4 = "numeric",
    bo3 = "numeric",
    h3sio4 = "numeric",
    h2sio4 = "numeric",
    h = "numeric",
    oh = "numeric",
    # total of pH dependent ions
    tot_po4 = "numeric",
    tot_nh3 = "numeric",
    tot_co3 = "numeric",
    tot_bo3 = "numeric",
    tot_sio4 = "numeric",
    # Additional ions
    br = "numeric",
    bro3 = "numeric",
    f = "numeric",
    fe = "numeric",
    al = "numeric",
    mn = "numeric",

    # Chlorine
    free_chlorine = "numeric",
    ocl = "numeric", # Ion of free chlorine
    combined_chlorine = "numeric", # sum of chloramines
    nh2cl = "numeric", # monochloramine
    nhcl2 = "numeric", # dichloramine
    ncl3 = "numeric", # trichloramine

    # Miscellaneous
    estimated = "character",

    # DBPs
    chcl3 = "numeric", # chloroform
    chcl2br = "numeric", # bromodichloromethane
    chbr2cl = "numeric", # dibromochloromethane
    chbr3 = "numeric", # bromoform
    tthm = "numeric",
    mcaa = "numeric", # chloroacetic acid
    dcaa = "numeric", # dichloroacetic acid
    tcaa = "numeric", # trichloroeacetic acid
    mbaa = "numeric", # bromoacetic acid
    dbaa = "numeric", # dibromoacetic acid
    haa5 = "numeric",
    bcaa = "numeric", # bromochloroacetic acid

    cdbaa = "numeric", # chlorodibromoacetic acid
    dcbaa = "numeric", # dichlorobromoacetic acid
    tbaa = "numeric" # tribromoacetic acid
  ),
  prototype(
    # General parameters
    ph = NA_real_,
    temp = NA_real_,
    alk = NA_real_,
    tds = NA_real_,
    cond = NA_real_,
    tot_hard = NA_real_,
    kw = NA_real_,
    alk_eq = NA_real_,
    carbonate_alk_eq = NA_real_,
    phosphate_alk_eq = NA_real_,
    ammonium_alk_eq = NA_real_,
    borate_alk_eq = NA_real_,
    silicate_alk_eq = NA_real_,
    hypochlorite_alk_eq = NA_real_,

    # Carbon
    toc = NA_real_,
    doc = NA_real_,
    bdoc = NA_real_,
    dic = NA_real_,
    uv254 = NA_real_,

    # Ions
    is = NA_real_,
    na = NA_real_,
    ca = NA_real_,
    mg = NA_real_,
    k = NA_real_,
    cl = NA_real_,
    so4 = NA_real_,
    mno4 = NA_real_,
    no3 = NA_real_,
    # pH dependent ions
    h2co3 = NA_real_,
    hco3 = NA_real_,
    co3 = NA_real_,
    h2po4 = NA_real_,
    hpo4 = NA_real_,
    po4 = NA_real_,
    nh4 = NA_real_,
    bo3 = NA_real_,
    h3sio4 = NA_real_,
    h2sio4 = NA_real_,
    h = NA_real_,
    oh = NA_real_,
    # total of pH dependent ions
    tot_po4 = NA_real_,
    tot_nh3 = NA_real_,
    tot_co3 = NA_real_,
    tot_bo3 = NA_real_,
    tot_sio4 = NA_real_,

    # Additional ions
    br = NA_real_,
    bro3 = NA_real_,
    f = NA_real_,
    fe = NA_real_,
    al = NA_real_,
    mn = NA_real_,

    # Chlorine
    free_chlorine = NA_real_,
    ocl = NA_real_, # Ion of free chlorine
    combined_chlorine = NA_real_, # sum of chloramines
    nh2cl = NA_real_, # monochloramine
    nhcl2 = NA_real_, # dichloramine
    ncl3 = NA_real_, # trichloramine

    # Miscellaneous
    estimated = "",

    # DBPs
    chcl3 = NA_real_, # chloroform
    chcl2br = NA_real_, # bromodichloromethane
    chbr2cl = NA_real_, # dibromochloromethane
    chbr3 = NA_real_, # bromoform
    tthm = NA_real_,
    mcaa = NA_real_, # chloroacetic acid
    dcaa = NA_real_, # dichloroacetic acid
    tcaa = NA_real_, # trichloroeacetic acid
    mbaa = NA_real_, # bromoacetic acid
    dbaa = NA_real_, # dibromoacetic acid
    haa5 = NA_real_,
    bcaa = NA_real_, # bromochloroacetic acid

    cdbaa = NA_real_, # chlorodibromoacetic acid
    dcbaa = NA_real_, # dichlorobromoacetic acid
    tbaa = NA_real_ # tribromoacetic acid
  )
)

methods::setMethod(
  "show",
  "water",
  function(object) {
    # General parameters
    cat("pH (unitless): ", object@ph, "\n")
    cat("Temperature (deg C): ", object@temp, "\n")
    cat("Alkalinity (mg/L CaCO3): ", object@alk, "\nUse summary functions or slot names to view other parameters.\n")
  }
)
