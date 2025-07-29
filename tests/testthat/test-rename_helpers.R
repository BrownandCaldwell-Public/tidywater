


test_that("_chain works", {
  water0 <- water_df %>%
    define_water_chain()

  water1 <- balance_ions_chain(water0)

  water2 <- biofilter_toc_chain(water0, ebct = 10)

  water3 <- chemdose_chloramine_chain(water0, time = 10, cl2 = 3, nh3 = .5)
  
  water4 <- chemdose_chlordecay_chain(water0, cl2_dose = 1, time = 8)
  
  water5 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_dbp_chain(cl2 = 2, time = 8)
  
  water6 <- chemdose_ph_chain(water0, hcl = 1)
  
  water7 <- chemdose_toc_chain(water0, alum = 5)

  water8 <- decarbonate_ph_chain(water0, co2_removed = 0.5)
  
  water9 <- modify_water_chain(water0, slot = "ca", value = 20, units = "mg/L")
  
  water10 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    ozonate_bromate_chain(dose = 1.5, time = 5)
  
  water11 <- pac_toc_chain(water0, dose = 15, time = 50)
  
  water12 <- calculate_corrosion_once(water0)
  
  water13 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_dbp_once(cl2 = 2, time = 8)
  
  water14 <- chemdose_ph_once(water0, hcl = 1)
  
  water15 <- chemdose_toc_once(water0, alum = 5)
  
  water16 <- dissolve_cu_once(water0)
  
  water17 <- dissolve_pb_once(water0)
  
  water18 <- solvect_chlorine_once(water0, time = 30, residual = 1, baffle = 0.7)
  
  water19 <- solvect_o3_once(water0, time = 10, dose = 2, kd = -0.5, baffle = 0.9)
  
  water20 <- solvedose_alk_once(water0, target_alk = 150, chemical = "naoh")
  
  water21 <- solvedose_ph_once(water0, target_ph = 8.5, chemical = "caoh2")
  
  water22 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    solveresid_o3_once(dose = 2, time = 10)
  
  water23 <- blend_waters_chain(water6, waters = c("defined_water", "dosed_chem_water"), ratios = c(0.5, 0.5))
  
})
