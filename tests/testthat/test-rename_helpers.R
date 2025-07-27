


test_that("define_water_chain and balance_ions_chain works", {
  water0 <- water_df %>%
    define_water_chain()

  water1 <- balance_ions_chain(water0)

  water2 <- biofilter_toc_chain(water0, ebct = 10)

})
