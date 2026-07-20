test_that("scavenger works with alk params", {
  # water with experimental params
})


test_that("scavenger has reasonable PD from real data", {
  # water with experimental params
  real_data <- data.frame(
    ph = c(5, 5, 7.5, 7.5, 10, 10, 5.6, 5.6, 5.6),
    toc = c(3, 3, 3, 3, 3, 3, 3, 0.3, 0.3),
    tic = c(0, 0, 1.68611e-5, 0.000021, 0.000328722222222222, 0.000371055555555556,0,0,0),
    no2 = c(0, 0, 0, 0, 0, 0, 0.007, 0.007, 0.007),
    reported_scavenging = c(6.43E+04, 5.48E+04, 5.98E+04, 6.00E+04, 1.50E+05, 1.98E+05, 2.16E+04, 1.82E+04, 2.02E+04)
  )

  waters<-real_data |> 
    select(-reported_scavenging) |> 
    define_water_df()


  
for (i in 1:19) {
  waters$defined[[i]]@dic <- waters$dic[[i]]
  waters$defined[[i]]@no2 <- waters$no2[[i]] / 14007
}



})


test_that("changing scavenger's rate constant for DOC works", {
  # water with experimental params
})
