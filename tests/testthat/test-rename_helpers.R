


test_that("_chain works", {
  water0 <- water_df %>%
    define_water_chain()

  water1 <- balance_ions_chain(water0)

  water2 <- biofilter_toc_chain(water0, ebct = 10)

  water3 <- chemdose_chloramine_chain(water0, time = 10, cl2 = 3, nh3 = .5)

})
