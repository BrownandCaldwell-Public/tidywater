# Generate data frames of data used across tidywater functions

# mweights ----
# List of molecular weights for different chemical additions
mweights <- data.frame(
  na = 22.98977,
  k = 39.0983,
  cl = 35.453,
  so4 = 96.0626,
  caco3 = 100.0869,
  caso4 = 136.141,
  hcl = 36.46094,
  h2so4 = 98.079,
  h3po4 = 97.995181,
  naoh = 39.9971,
  na2co3 = 105.98844,
  nahco3 = 84.00661,
  caoh2 = 74.09268,
  mgoh2 = 58.31968,
  cacl2 = 110.98,
  cl2 = 70.906,
  co2 = 44.009,
  al = 26.981539,
  fe = 55.845,
  alum = 26.981539 * 2 + 96.0626 * 3 + 14 * 18.01528, # 14 H2O
  ferricchloride = 55.845 + 35.453 * 3,
  fecl3 = 55.845 + 35.453 * 3,
  ferricsulfate = 2 * 55.845 + 3 * 96.0626 + 8.8 * 18.01528, # 8.8 H2O
  ach = 26.981539 * 2 + 17.008 * 5 + 35.453 + 2 * 18.01528, # 2 H2O
  mg = 24.305,
  ca = 40.078,
  po4 = 94.97,
  na3po4 = 163.939,
  hpo4 = 95.98,
  h2po4 = 96.99,
  h2co3 = 62.024,
  hco3 = 61.0168,
  co3 = 60.0089,
  ocl = 16 + 35.453,
  pb = 207.2,
  br = 79.904,
  bro3 = 79.904 + 3 * 15.999,
  f = 18.9984,
  naf = 41.9882,
  mn = 54.938,
  nh4 = 18.04,
  nh3 = 17.031,
  no3 = 62.005,
  hno3 = 63.0128,
  nh2cl = 51.48,
  nhcl2 = 85.92,
  ncl3 = 120.365,
  n = 14.0067,
  nh4oh = 18.04 + 17.008,
  nh42so4 = 18.04 * 2 + 96.0626,
  oh = 17.008,
  kmno4 = 158.032,
  mno4 = 158.032 - 39.0983,
  dic = 12.011
)

usethis::use_data(mweights, overwrite = TRUE)

# discons ----
# Acid dissociation constants and corresponding enthalpy
# Carbonic acid
discons <- data.frame(
  ID = c("k1co3", "k2co3"), # H2CO3<-->HCO3- + H+; HCO3<-->CO32- + H+
  k = c(10^-6.35, 10^-10.33),
  deltah = c(7700, 14900) # J/mol
) %>%
  # Sulfate
  add_row(ID = "kso4", k = 10^-1.99, deltah = -21900) %>%
  # Phosphate
  # H3PO4<-->H+ + H2PO4-; H2PO4-<-->H+ + HPO42-; HPO42--<-->H+ + PO43-
  add_row(ID = c("k1po4", "k2po4", "k3po4"), k = c(10^-2.16, 10^-7.20, 10^-12.35), deltah = c(-8000, 4200, 14700)) %>%
  # Hypochlorite
  add_row(ID = "kocl", k = 10^-7.53, deltah = 13800) %>% # HOCl<-->H+ + OCl-
  # Ammonia
  add_row(ID = "knh4", k = 10^-9.244, deltah = 52210) # NH4+ <--> NH3 + H+
rownames(discons) <- discons$ID

usethis::use_data(discons, overwrite = TRUE)

# water_df ----
# Dummy data frame for function examples
water_df <- data.frame(
  ph = rep(c(7.9, 8.5, 8.1, 7.8), 3),
  temp = rep(c(20, 25, 19), 4),
  alk = rep(c(50, 80, 100, 200), 3),
  tot_hard = rep(c(50, 75, 100, 30, 400, 110), 2),
  ca = rep(c(13, 20, 26, 8, 104, 28), 2),
  mg = rep(c(4, 6, 8, 3, 34, 9), 2),
  na = rep(c(20, 90), 6),
  k = rep(c(20, 90), 6),
  cl = rep(c(30, 92), 6),
  so4 = rep(c(20, 40, 60, 80), 3),
  free_chlorine = rep(c(0, 1), 6),
  tot_po4 = rep(c(0, 0, 1), 4),
  tds = rep(c(200, 100, NA), 4),
  cond = rep(c(100, 150, NA), 4),
  toc = rep(c(2, 3, 4), 4),
  doc = rep(c(1.8, 2.8, 3.5), 4),
  uv254 = rep(c(.05, .08, .12), 4)
)


usethis::use_data(water_df, overwrite = TRUE)

# edwardscoeff ----
# Data frame of Edwards model coefficients
edwardscoeff <- data.frame(
  ID = "Alum",
  x3 = 4.91, x2 = -74.2, x1 = 284,
  k1 = -0.075, k2 = 0.56,
  b = 0.147
) %>%
  add_row(
    ID = "Ferric",
    x3 = 4.96, x2 = -73.9, x1 = 280,
    k1 = -0.028, k2 = 0.23,
    b = 0.068
  ) %>%
  add_row(
    ID = "Low DOC",
    x3 = 6.44, x2 = -99.2, x1 = 387,
    k1 = -0.053, k2 = 0.54,
    b = 0.107
  ) %>%
  add_row(
    ID = "General Alum",
    x3 = 6.42, x2 = -98.6, x1 = 383,
    k1 = -0.054, k2 = 0.54,
    b = 0.145
  ) %>%
  add_row(
    ID = "General Ferric",
    x3 = 6.42, x2 = -98.6, x1 = 383,
    k1 = -0.054, k2 = 0.54,
    b = 0.092
  )
rownames(edwardscoeff) <- edwardscoeff$ID

usethis::use_data(edwardscoeff, overwrite = TRUE)

# leadsol_constants ----
# Data frame of equilibrium constants for lead and copper solubility
leadsol_constants <- data.frame(
  species_name = c("Lead Hydroxide", "Cerussite", "Hydrocerussite"),
  constant_name = c("K_solid_lead_hydroxide", "K_solid_cerussite", "K_solid_hydrocerussite"),
  log_value = c(13.06, -13.11, -18),
  source = rep("Schock et al. (1996)", 3)
) %>%
  # Solids
  add_row(
    species_name = c("Hydroxypyromorphite", "Hydroxypyromorphite", "Pyromorphite", "Pyromorphite"),
    constant_name = c("K_solid_hydroxypyromorphite_s", "K_solid_hydroxypyromorphite_z", "K_solid_pyromorphite_x", "K_solid_pyromorphite_t"),
    log_value = c(-62.83, -66.77, -80.4, -79.6),
    source = c("Schock et al. (1996)", "Zhu et al. (2015)", "Xie & Giammar (2007)", "Topolska et al. (2016)")
  ) %>%
  add_row(
    species_name = c("Primary Lead Orthophosphate", "Secondary Lead Orthophosphate", "Tertiary Lead Orthophosphate"),
    constant_name = c("K_solid_primary_lead_ortho", "K_solid_secondary_lead_ortho", "K_solid_tertiary_lead_ortho"),
    log_value = c(-48.916, -23.81, -44.4),
    source = c("Powell et al. (2009)", "Schock et al. (1996)", "Powell et al. (2009)")
  ) %>%
  add_row(
    species_name = c("Anglesite", "Laurionite", "Laurionite"),
    constant_name = c("K_solid_anglesite", "K_solid_laurionite_nl", "K_solid_laurionite_l"),
    log_value = c(-7.79, 0.619, 0.29),
    source = c("Schock et al. (1996)", "Nasanen & Lindell (1976)", "Lothenbach et al. (1999)")
  ) %>%
  # Lead-Hydroxide Complexes
  add_row(
    species_name = c("PbOH+", "Pb(OH)2", "Pb(OH)3-", "Pb(OH)4-2"),
    constant_name = c("B_1_OH", "B_2_OH", "B_3_OH", "B_4_OH"),
    log_value = c(-7.22, -16.91, -28.08, -39.72),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  add_row(
    species_name = c("Pb2OH+3", "Pb3(OH)4+2", "Pb4(OH)4+4", "Pb6(OH)8+4"),
    constant_name = c("B_2_1_OH", "B_3_4_OH", "B_4_4_OH", "B_6_8_OH"),
    log_value = c(-6.36, -23.86, -20.88, -43.62),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  # Lead-Chloride Complexes
  add_row(
    species_name = c("PbCl+1", "PbCl2", "PbCl3-", "PbCl4-2"),
    constant_name = c("K_1_Cl", "B_2_Cl", "B_3_Cl", "B_4_Cl"),
    log_value = c(1.59, 1.8, 1.71, 1.43),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
  add_row(
    species_name = c("PbSO4", "Pb(SO4)2-2"),
    constant_name = c("K_1_SO4", "B_2_SO4"),
    log_value = c(2.73, 3.5),
    source = rep("Schock et al. (1996)", 2)
  ) %>%
  # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
  add_row(
    species_name = c("PbHCO3+", "PbCO3", "Pb(CO3)2-2"),
    constant_name = c("K_1_CO3", "K_2_CO3", "K_3_CO3"),
    log_value = c(12.59, 7.1, 10.33),
    source = rep("Schock et al. (1996)", 3)
  ) %>%
  # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
  add_row(
    species_name = c("PbHPO4", "PbH2PO4+"),
    constant_name = c("K_1_PO4", "K_2_PO4"),
    log_value = c(15.41, 21.05),
    source = rep("Schock et al. (1996)", 2)
  )
rownames(leadsol_constants) <- leadsol_constants$constant_name

usethis::use_data(leadsol_constants, overwrite = TRUE)

# leadplotcoeffs ----
# Data frame of lead solubility given pH and DIC values
leadplotcoeffs <- merge(
  data.frame(leadplotph = seq(7.0, 10.0, 0.2)),
  data.frame(leadplotdic = seq(2, 58, 2))
) %>%
  .[order(.$leadplotph), ] %>%
  { row.names(.) <- NULL; . }
leadplotcoeffs$leadplotsol <- c(
  -0.0574436, -0.2700997, -0.3727667, -0.4348943, -0.4769195, -0.5073618, 
  -0.5304750, -0.5486394, -0.5632981, -0.5753790, -0.5855076, -0.5941211, 
  -0.6015347, -0.6079816, -0.6136380, -0.6186395, -0.6230924, -0.6270812, 
  -0.6306737, -0.6339251, -0.6368809, -0.6395788, -0.6420502, -0.6443217,
  -0.6464160, -0.6483523, -0.6501472, -0.6518150, -0.6533683, -0.2074788,
  -0.3642544, -0.4508115, -0.5015106, -0.5349983, -0.5588148, -0.5766322,
  -0.5904637, -0.6015087, -0.6105279, -0.6180272, -0.6243567, -0.6297664, 
  -0.6344396, -0.6385140, -0.6420948, -0.6452639, -0.6480861, -0.6506131, 
  -0.6528871, -0.6549423, -0.6568071, -0.6585053, -0.6600568, -0.6614784, 
  -0.6627846, -0.6639875, -0.6650979, -0.6661251, -0.3547418, -0.4293961, 
  -0.5029529, -0.5449165, -0.5721183, -0.5911856, -0.6052812, -0.6161120, 
  -0.6246818, -0.6316204, -0.6373432, -0.6421354, -0.6461993, -0.6496827, 
  -0.6526956, -0.6553220, -0.6576270, -0.6596618, -0.6614675, -0.6630769, 
  -0.6645170, -0.6658102, -0.6669750, -0.6680268, -0.6689788, -0.6698421, 
  -0.6706264, -0.6713399, -0.6719897, -0.4781987, -0.5274759, -0.5367854, 
  -0.5724755, -0.5952649, -0.6110442, -0.6225832, -0.6313597, -0.6382351, 
  -0.6437461, -0.6482445, -0.6519707, -0.6550945, -0.6577393, -0.6599970, 
  -0.6619373, -0.6636142, -0.6650700, -0.6663387, -0.6674473, -0.6684182,
  -0.6692695, -0.6700165, -0.6706718, -0.6712463, -0.6717491, -0.6721879, 
  -0.6725696, -0.6728998, -0.5832349, -0.6198993, -0.6182140, -0.6079058, 
  -0.6091211, -0.6226252, -0.6323805, -0.6397063, -0.6453672, -0.6498373, 
  -0.6534262, -0.6563447, -0.6587414, -0.6607240, -0.6623725, -0.6637476, 
  -0.6648961, -0.6658548, -0.6666528, -0.6673139, -0.6678571, -0.6682982, 
  -0.6686503, -0.6689244, -0.6691298, -0.6692742, -0.6693645, -0.6694063, 
  -0.6694047, -0.6746304, -0.7025702, -0.6962082, -0.6829020, -0.6682021, 
  -0.6536683, -0.6397776, -0.6430848, -0.6477755, -0.6513845, -0.6541948, 
  -0.6563987, -0.6581314, -0.6594909, -0.6605498, -0.6613631, -0.6619733, 
  -0.6624137, -0.6627107, -0.6628856, -0.6629557, -0.6629353, -0.6628361,
  -0.6626682, -0.6624397, -0.6621578, -0.6618286, -0.6614572, -0.6610481, 
  -0.7560667, -0.7783442, -0.7688391, -0.7534075, -0.7370948, -0.7212434,
  -0.7062213, -0.6920912, -0.6788139, -0.6663190, -0.6545303, -0.6523102, 
  -0.6533123, -0.6539670, -0.6543427, -0.6544913, -0.6544525, -0.6542577, 
  -0.6539317, -0.6534944, -0.6529621, -0.6523482, -0.6516636, -0.6509177, 
  -0.6501183, -0.6492719, -0.6483842, -0.6474600, -0.6465036, -0.8299954,
  -0.8489450, -0.8373241, -0.8202628, -0.8025668, -0.7854760, -0.7693075, 
  -0.7540956, -0.7397840, -0.7262913, -0.7135343, -0.7014359, -0.6899273, 
  -0.6789485, -0.6684471, -0.6583777, -0.6487007, -0.6401017, -0.6389371, 
  -0.6376718, -0.6363215, -0.6348990, -0.6334150, -0.6318784, -0.6302966, 
  -0.6286760, -0.6270219, -0.6253390, -0.6236313, -0.8976806, -0.9151484,
  -0.9020794, -0.8835962, -0.8645016, -0.8460338, -0.8285090, -0.8119609, 
  -0.7963325, -0.7815420, -0.7675056, -0.7541457, -0.7413933, -0.7291879, 
  -0.7174767, -0.7062140, -0.6953599, -0.6848793, -0.6747417, -0.6649202, 
  -0.6553908, -0.6461325, -0.6371263, -0.6283554, -0.6198046, -0.6114602, 
  -0.6033099, -0.5953423, -0.5896322, -0.9592480, -0.9768862, -0.9628241, 
  -0.9429155, -0.9222039, -0.9020292, -0.8827575, -0.8644494, -0.8470637, 
  -0.8305280, -0.8147645, -0.7996993, -0.7852657, -0.7714047, -0.7580645, 
  -0.7451996, -0.7327702, -0.7207415, -0.7090825, -0.6977660, -0.6867678, 
  -0.6760663, -0.6656421, -0.6554779, -0.6455580, -0.6358681, -0.6263955, 
  -0.6171283, -0.6080558, -1.0136396, -1.0332487, -1.0186061, -0.9971543, 
  -0.9744670, -0.9521099, -0.9305599, -0.9099370, -0.8902346, -0.8714006, 
  -0.8533698, -0.8360759, -0.8194571, -0.8034569, -0.7880248, -0.7731162, 
  -0.7586909, -0.7447137, -0.7311530, -0.7179805, -0.7051710, -0.6927017, 
  -0.6805520, -0.6687035, -0.6571392, -0.6458437, -0.6348029, -0.6240039, 
  -0.6134350, -1.0584213, -1.0823763, -1.0677854, -1.0447378, -1.0197168, 
  -0.9946788, -0.9702924, -0.9467803, -0.9241933, -0.9025129, -0.8816934, 
  -0.8616801, -0.8424170, -0.8238508, -0.8059318, -0.7886147, -0.7718583, 
  -0.7556251, -0.7398813, -0.7245963, -0.7097422, -0.6952936, -0.6812274, 
  -0.6675226, -0.6541597, -0.6411210, -0.6283901, -0.6159519, -0.6037925, 
  -1.0894352, -1.1212294, -1.1079867, -1.0837101, -1.0562951, -1.0283170, 
  -1.0007493, -0.9739760, -0.9481372, -0.9232645, -0.8993404, -0.8763252, 
  -0.8541700, -0.8328239, -0.8122367, -0.7923603, -0.7731501, -0.7545646,
  -0.7365653, -0.7191171, -0.7021876, -0.6857469, -0.6697676, -0.6542245, 
  -0.6390944, -0.6243558, -0.6099887, -0.5959748, -0.5822969, -1.1003634, 
  -1.1451776, -1.1359331, -1.1118219, -1.0827675, -1.0522708, -1.0217697, 
  -0.9918958, -0.9629258, -0.9349680, -0.9080471, -0.8821459, -0.8572265,
  -0.8332413, -0.8101394, -0.7878696, -0.7663825, -0.7456310, -0.7255709, 
  -0.7061612, -0.6873636, -0.6691428, -0.6514660, -0.6343029, -0.6176256, 
  -0.6014082, -0.5856266, -0.5702586, -0.5552835, -1.0824681, -1.1472985, 
  -1.1468311, -1.1261210, -1.0977070, -1.0663806, -1.0342619, -1.0023602, 
  -0.9711687, -0.9409216, -0.9117181, -0.8835841, -0.8565060, -0.8304493, 
  -0.8053689, -0.7812150, -0.7579369, -0.7354844, -0.7138097, -0.6928673,
  -0.6726144, -0.6530111, -0.6340204, -0.6156077, -0.5977410, -0.5803906, 
  -0.5635289, -0.5471305, -0.5311714, -1.0251273, -1.1174916, -1.1328541, 
  -1.1212462, -1.0979653, -1.0693752, -1.0385181, -1.0069594, -0.9755355, 
  -0.9446971, -0.9146820, -0.8856065, -0.8575163, -0.8304157, -0.8042854, 
  -0.7790926, -0.7547973, -0.7313564, -0.7087260, -0.6868628, -0.6657251, 
  -0.6452729, -0.6254686, -0.6062766, -0.5876638, -0.5695992, -0.5520538,
  -0.5350006, -0.5184144)

usethis::use_data(leadplotcoeffs, overwrite = TRUE)

# Data frame of pH and DIC values at the lead transition line from Water Pro
leadplottransition <- data.frame(
  ph = c(7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8, 8.1, 8.2, 8.3, 8.4, 
         8.5, 8.55, 8.6, 8.63, 8.647, 8.649),
  dic = c(0.8, 1.5, 2.2, 3, 4.2, 5.2, 6.5, 8.2, 10.2, 12, 14.8, 17.5, 21.5,
          26, 31.5, 36, 40, 46, 52, 58)
)

usethis::use_data(leadplottransition, overwrite = TRUE)

# dbpcoeffs ----
# Data frame of THM and HAA coefficients
dbpcoeffs <- data.frame(
  # raw/untreated water
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes", group = "tthm",
  treatment = "raw",
  A = 4.121e-2, a = 1.098, b = 0.152, c = 0.068, d = 0.609, e = 1.601, f = 0.263, ph_const = NA
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "raw",
    A = 6.237e-2, a = 1.617, b = -0.094, c = -0.175, d = 0.607, e = 1.403, f = 0.306, ph_const = NA
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "raw",
    A = 1.445e-3, a = 0.901, b = 0.017, c = 0.733, d = 0.498, e = 1.511, f = 0.199, ph_const = NA
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "raw",
    A = 2.244e-6, a = -0.226, b = 0.108, c = 1.810, d = 0.512, e = 2.212, f = 0.146, ph_const = NA
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "raw",
    A = 1.49e-8, a = -0.983, b = 0.804, c = 1.765, d = 0.754, e = 2.139, f = 0.566, ph_const = NA
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "raw",
    A = 30, a = 0.997, b = 0.278, c = -0.138, d = 0.341, e = -0.799, f = 0.169, ph_const = NA
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "raw",
    A = 9.98, a = 0.935, b = 0.443, c = -0.031, d = 0.387, e = -0.655, f = 0.178, ph_const = NA
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 0.45, a = 0.173, b = 0.379, c = 0.029, d = 0.573, e = -0.279, f = 0.009, ph_const = NA
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 0.3, a = 1.396, b = 0.379, c = -0.149, d = 0.465, e = 0.200, f = 0.218, ph_const = NA
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 92.68, a = 1.152, b = 0.331, c = -0.2299, d = 0.299, e = -1.627, f = 0.180, ph_const = NA
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "raw",
    A = 6.21e-5, a = -0.584, b = 0.754, c = 1.10, d = 0.707, e = 0.604, f = 0.090, ph_const = NA
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "raw",
    A = 3.69e-5, a = -1.087, b = 0.673, c = 2.052, d = 0.380, e = -0.001, f = 0.095, ph_const = NA
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "raw",
    A = 5.51e-3, a = 0.463, b = 0.522, c = 0.667, d = 0.379, e = 0.581, f = 0.220, ph_const = NA
  ) %>%
  # coagulated/softened water
  # tthms
  add_row(
    ID = "tthm",
    alias = "total trihalomethanes", group = "tthm",
    treatment = "coag",
    A = 23.9, a = 0.403, b = 0.225, c = 0.141, d = 1.1560, e = 1.0263, f = 0.264, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "coag",
    A = 266, a = 0.403, b = 0.424, c = -0.679, d = 1.1322, e = 1.0179, f = 0.333, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "coag",
    A = 1.68, a = 0.260, b = 0.114, c = 0.462, d = 1.0260, e = 1.0977, f = 0.196, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "coag",
    A = 8.0e-3, a = -0.056, b = -0.157, c = 1.425, d = 1.0212, e = 1.1271, f = 0.148, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "coag",
    A = 4.4e-5, a = -0.300, b = -0.221, c = 2.134, d = 1.0374, e = 1.3907, f = 0.143, ph_const = 7.5
  ) %>%
  # haa5 & haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "coag",
    A = 30.7, a = 0.302, b = 0.541, c = -0.012, d = 0.932, e = 1.021, f = 0.161, ph_const = 7.5
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "coag",
    A = 41.6, a = 0.328, b = 0.585, c = -0.121, d = 1.022, e = 0.9216, f = 0.150, ph_const = 7.5
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 4.58, a = -0.090, b = 0.662, c = -0.224, d = 1.024, e = 1.042, f = 0.043, ph_const = 7.5
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 60.4, a = 0.397, b = 0.665, c = -0.558, d = 1.017, e = 1.034, f = 0.222, ph_const = 7.5
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 52.6, a = 0.403, b = 0.749, c = -0.416, d = 1.014, e = 0.8739, f = 0.163, ph_const = 7.5
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "coag",
    A = 2.06e-2, a = 0.358, b = -0.101, c = 0.812, d = 1.162, e = 0.6526, f = 0.043, ph_const = 7.5
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "coag",
    A = 9.42e-5, a = 0.0590, b = 0.182, c = 2.109, d = 1.007, e = 1.210, f = 0.070, ph_const = 7.5
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "coag",
    A = 3.23e-1, a = 0.153, b = 0.257, c = 0.586, d = 1.042, e = 1.181, f = 0.201, ph_const = 7.5
  ) %>%
  # haa9
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 3.70e-3, a = -0.0162, b = -0.170, c = 0.972, d = 1.054, e = 0.839, f = 0.685, ph_const = 8
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 5.89e-1, a = 0.230, b = 0.140, c = 0.301, d = 1.022, e = 0.700, f = 0.422, ph_const = 8
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 5.59e-6, a = 0.0657, b = -2.51, c = 2.32, d = 1.059, e = 0.555, f = 1.26, ph_const = 8
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids", group = "haa9",
    treatment = "coag",
    A = 10.78, a = 0.25, b = 0.5, c = 0.054, d = 1.015, e = 0.894, f = 0.348, ph_const = 8
  ) %>%
  # gac treated water
  # tthms
  add_row(
    ID = "tthm",
    alias = "total trihalomethanes", group = "tthm",
    treatment = "gac",
    A = 17.7, a = 0.475, b = 0.173, c = 0.246, d = 1.316, e = 1.036, f = 0.366, ph_const = 8
  ) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "gac",
    A = 101, a = 0.615, b = 0.699, c = -0.468, d = 1.099, e = 1.035, f = 0.336, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "gac",
    A = 7.57, a = 0.443, b = 0.563, c = 0.0739, d = 1.355, e = 1.03, f = 0.281, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "gac",
    A = 3.99, a = 0.535, b = 0.125, c = 0.365, d = 1.436, e = 1.037, f = 0.322, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "gac",
    A = 1.47e-1, a = 0.408, b = -0.115, c = 0.961, d = 1.438, e = 1.048, f = 0.324, ph_const = 7.5
  ) %>%
  # haa5 & haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "gac",
    A = 41.2, a = 0.498, b = 0.388, c = -0.156, d = 0.867, e = 1.021, f = 0.263, ph_const = 8
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "gac",
    A = 37.8, a = 0.511, b = 0.374, c = -0.079, d = 0.913, e = 1.022, f = 0.280, ph_const = 8
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 1.31e-1, a = 0.202, b = 0.275, c = -0.958, d = 0.124, e = 1.036, f = 0.923, ph_const = 8
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 38.4, a = 0.503, b = 0.421, c = -0.393, d = 0.867, e = 1.019, f = 0.293, ph_const = 8
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 47.8, a = 0.627, b = 0.729, c = -0.425, d = 0.602, e = 1.011, f = 0.174, ph_const = 8
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "gac",
    A = 3.0e-1, a = 0.093, b = 0.964, c = -0.408, d = 0.134, e = 1.054, f = 0.554, ph_const = 8
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "gac",
    A = 3.96e-1, a = 0.509, b = -0.251, c = 0.689, d = 1.302, e = 1.019, f = 0.310, ph_const = 8
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "gac",
    A = 3.89, a = 0.560, b = 0.260, c = 0.117, d = 1.077, e = 1.018, f = 0.334, ph_const = 8
  ) %>%
  # haa9
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 5.56e-2, a = 0.831, b = -0.296, c = 0.782, d = 0.477, e = 1.016, f = 0.886, ph_const = 8
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 2.19, a = 0.665, b = 0.270, c = 0.221, d = 0.587, e = 0.985, f = 0.379, ph_const = 8
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 1.65e-4, a = 1.59, b = -2.19, c = 2.06, d = 0.575, e = 0.983, f = 1.78, ph_const = 8
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids", group = "haa9",
    treatment = "gac",
    A = 20.6, a = 0.509, b = 0.253, c = 0.053, d = 0.823, e = 1.019, f = 0.425, ph_const = 8
  )
rownames(dbpcoeffs) <- dbpcoeffs$ID

usethis::use_data(dbpcoeffs, overwrite = TRUE)

# Data frame of DBP conversion factors for chloramines
chloramine_conv <- data.frame(
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes",
  percent = 0.20
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform",
    percent = 0.20
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids",
    percent = 0.20
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids",
    percent = 0.20
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid",
    percent = 0.50
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid",
    percent = 0.05
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid",
    percent = 0.30
  ) %>%
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids",
    percent = 0.20
  )
rownames(chloramine_conv) <- chloramine_conv$ID

usethis::use_data(chloramine_conv, overwrite = TRUE)


# Data frame of DBP correction factors based on location from testing with ICR data
# No correction factors developed for CDBAA, BDCAA, TBAA, and HAA9 since ICR plant data was used to develop these equations

dbp_correction <- data.frame(
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes",
  plant = 1,
  ds = 1
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform",
    plant = 1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane",
    plant = 0.92,
    ds = 1
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane",
    plant = 0.65,
    ds = 0.46
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform",
    plant = 1,
    ds = 1
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids",
    plant = 1.1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids",
    plant = 1.1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid",
    plant = 0.72,
    ds = 1.1
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid",
    plant = 1.3,
    ds = 1.3
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid",
    plant = 0.86,
    ds = 2
  ) %>%
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids",
    plant = 1,
    ds = 1
  )
rownames(dbp_correction) <- dbp_correction$ID

usethis::use_data(dbp_correction, overwrite = TRUE)

# bromatecoeffs ----
# Dataframe of bromate formation coefficients
bromatecoeffs <- data.frame(
  model = rep("Ozekin", 2),
  ammonia = c(F, T),
  A = c(1.55E-6, 1.63E-6),
  a = c(0.73, 0.73),
  b = c(-1.26, -1.3),
  c = c(0, 0), # No UV in this model
  d = c(5.82, 5.79),
  e = c(0, 0), # No alk in this model
  f = c(1.57, 1.59),
  g = c(0.28, 0.27),
  h = c(0, -0.033),
  i = c(0, 0), # no temp in this model
  I = c(1, 1) # no temp in this model
) %>%
  add_row(
    model = rep("Sohn", 2),
    ammonia = c(F, T),
    A = c(1.19E-7, 8.71E-8),
    a = c(0.96, 0.944),
    b = c(0, 0), # No DOC in this model
    c = c(-0.623, -0.593),
    d = c(5.68, 5.81),
    e = c(-0.201, -0.167),
    f = c(1.307, 1.279),
    g = c(0.336, 0.337),
    h = c(0, -0.051),
    i = c(0, 0), # temp in exponent
    I = c(1.035, 1.035)
  ) %>%
  add_row(
    model = "Song",
    ammonia = T, # Only applies when ammonia > .005
    A = 7.76E-7,
    a = 0.88,
    b = -1.88,
    c = 0, # No UV in this model
    d = 5.11,
    e = 0.18,
    f = 1.42,
    g = 0.27,
    h = -0.18,
    i = 0, # no temp in this model
    I = 1 # no temp in this model
  ) %>%
  add_row(
    model = "Galey",
    ammonia = F, # Only applies when ammonia = 0
    A = 5.41E-5,
    a = .04,
    b = -1.08,
    c = 0, # No UV in this model
    d = 4.7,
    e = 0, # No alk in this model
    f = 1.12,
    g = 0.304,
    h = 0,
    i = 0.58,
    I = 1 # temp not in exponent
  ) %>%
  add_row(
    model = "Siddiqui",
    ammonia = F, # Only applies when ammonia = 0
    A = 1.5E-3,
    a = 0.61,
    b = 0.61,
    c = 0, # No UV in this model
    d = 2.26,
    e = 0, # No alk in this model
    f = 0.64,
    g = 0, # No time in this model
    h = 0,
    i = 2.03,
    I = 1 # temp not in exponent
  )
usethis::use_data(bromatecoeffs, overwrite = TRUE)

# cl2coeffs -----
# Data frame of Cl2 decay coefficients
cl2coeffs <- tibble(
  treatment = c("chlorine_raw", "chlorine_coag", "chloramine"),
  a = c(-0.8147, -0.8404, -0.99),
  b = c(-2.2808, -0.404, -0.015),
  c = c(-1.2971, -0.9108, NA)
)

usethis::use_data(cl2coeffs, overwrite = TRUE)

# pactoccoeffs -----
# Data frame of PAC TOC removal coefficients
pactoccoeffs <- tibble(
  pactype = c("bituminous", "lignite", "wood"),
  A = c(.1561, .4078, .3653),
  a = c(.9114, .8516, .8692),
  b = c(.0263, .0225, .0151),
  c = c(.002, .002, .0025)
)

usethis::use_data(pactoccoeffs, overwrite = TRUE)

# toc_compliance_table -----
# Data frame of PAC TOC removal coefficients

toc_compliance_table <- data.frame(
  toc_min = c(2, 2, 2, 4, 4, 4, 8, 8, 8),
  toc_max = c(4, 4, 4, 8, 8, 8, Inf, Inf, Inf),
  alk_min = c(0, 60, 120, 0, 60, 120, 0, 60, 120),
  alk_max = c(60, 120, Inf, 60, 120, Inf, 60, 120, Inf),
  required_compliance = c(35, 25, 15, 45, 35, 25, 50, 40, 30)
)

usethis::use_data(toc_compliance_table, overwrite = TRUE)

# vlog_removalcts -----
vlog_removalcts <- data.frame(
  ph_range = "6-9",
  temp_value = 0.5,
  ct_range = "6-9",
  vlog_removal = 2.0
) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 0.5,
    ct_range = "9-12",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 0.5,
    ct_range = "12",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "45-66",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "66-90",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "90",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "4-6",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "6-8",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "8",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "30-44",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "44-60",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "60",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "3-4",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "4-6",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "6",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "22-33",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "33-45",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "45",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "2-3",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "3-4",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "4",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "15-22",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "22-30",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "30",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "1-2",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "2-3",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "3",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "11-16",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "16-22",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "22",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "1",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "1-2",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "2",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "7-11",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "11-15",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "15",
    vlog_removal = 4.0
  )

usethis::use_data(vlog_removalcts, overwrite = TRUE)
