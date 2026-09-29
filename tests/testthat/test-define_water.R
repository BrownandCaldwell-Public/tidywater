# Define water -----
library(dplyr)

test_that("Define water outputs water class.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(
      ph = 7,
      temp = 25,
      alk = 100,
      tot_hard = 0,
      ca = 0,
      mg = 0,
      na = 0,
      k = 0,
      cl = 0,
      so4 = 0,
      toc = 5,
      doc = 4.8,
      uv254 = .1
    )
    water2 <- define_water(temp = 25, tot_hard = 50)
    water3 <- define_water(ph = 7, temp = 25, alk = 100)
  })
  expect_s4_class(water1, "water")
  expect_s4_class(water2, "water")
  expect_s4_class(water3, "water")
})

test_that("Define water calculates correct carbonate balance.", {
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
  })

  expect_equal(water1@ph, 7)
  expect_equal(signif(water1@tot_co3, 2), 0.0024)
  expect_equal(signif(water1@hco3, 2), 0.002)
})

test_that("Define water calculates correct TDS/IS/cond.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, cond = 312))
  water3 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 100, cl = 100, so4 = 30))

  expect_true(grepl("cond", water1@estimated))
  expect_true(grepl("tds", water2@estimated))
  expect_true(grepl("cond", water3@estimated) & grepl("tds", water3@estimated))

  expect_equal(round(water1@cond), 312)
  expect_equal(round(water2@tds), 200)
  expect_equal(signif(water1@is, 2), .005)
  expect_equal(signif(water2@is, 2), .005)
  expect_equal(signif(water3@is, 2), .005)
  expect_equal(round(water3@cond), 315)
  expect_equal(round(water3@tds), 201)
})


test_that("Define water gives missing value warnings.", {
  expect_warning(
    define_water(
      alk = 100,
      temp = 20,
      tot_hard = 70,
      ca = 10,
      mg = 10,
      na = 10,
      k = 10,
      cl = 10,
      so4 = 10,
      tds = 100,
      doc = 5,
      toc = 5,
      uv254 = .1,
      br = 50
    ),
    "Missing.+ph.+"
  )
  expect_warning(
    define_water(
      ph = 7,
      temp = 20,
      tot_hard = 70,
      ca = 10,
      mg = 10,
      na = 10,
      k = 10,
      cl = 10,
      so4 = 10,
      tds = 100,
      doc = 5,
      toc = 5,
      uv254 = .1,
      br = 50
    ),
    "Missing.+alk.+"
  )
  expect_warning(
    define_water(
      ph = 7,
      alk = 100,
      temp = 20,
      tot_hard = 70,
      ca = 10,
      mg = 10,
      na = 10,
      k = 10,
      cl = 10,
      so4 = 10,
      toc = 5,
      uv254 = .1,
      br = 50
    ),
    "Missing.+doc.+"
  )
})

test_that("Define water doesn't output carbonate when pH or alk aren't provided.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25)
    water2 <- define_water(temp = 25, alk = 50)
  })

  expect_equal(water1@tot_co3, NA_real_)
  expect_equal(water2@tot_co3, NA_real_)
  expect_equal(water1@alk, NA_real_)
  expect_equal(water2@ph, NA_real_)
})

test_that("define_water handles organics inputs correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 7, doc = 3.5, toc = 3.4))

  expect_equal(water1@doc, 3.325)
  expect_equal(round(water2@toc, 3), 3.684)
  expect_equal(water3@uv254, NA_real_)
})

test_that("define_water correctly specifies when estimates are used.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 50, na = 100, cl = 100))
  water2 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(
    ph = 7,
    temp = 25,
    alk = 100,
    tot_hard = 50,
    na = 100,
    cl = 100,
    toc = 3.5,
    doc = 3.5,
    ca = 40,
    tds = 100
  ))

  expect_true(grepl("tds", water1@estimated))
  expect_true(grepl("cond", water1@estimated))
  expect_true(grepl("ca", water1@estimated))
  expect_true(grepl("doc", water2@estimated))

  expect_false(grepl("tds", water3@estimated))
  expect_false(grepl("ca", water3@estimated))
  expect_false(grepl("doc", water3@estimated))
  expect_true(grepl("cond", water3@estimated))
})

test_that("define_water correctly calculates dic", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100))

  dic_mg <- water1@tot_co3 * tidywater::mweights$dic * 1000
  expect_equal(dic_mg, water1@dic)
})

test_that("define_water converts nitrite from mg/L as N to M", {
  water0 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100))
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, no2 = 1))

  expect_true(is.na(water0@no2))
  # 1 mg/L as N / 14.0067 g/mol N / 1000 mg/g
  expect_equal(signif(water1@no2, 5), 7.1394e-5)
})

test_that("define_water calculates co2 in mg/L as CO2 from h2co3", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100))
  water2 <- suppressWarnings(define_water(ph = 6, temp = 15, alk = 50, tds = 200))

  expect_equal(water1@co2, water1@h2co3 * 44.009 * 1000)
  expect_equal(water2@co2, convert_units(water2@h2co3, "co2", "M", "mg/L"))
  expect_true(water2@co2 > 0)
})

test_that("define_water accounts for multiple types of alkalinity", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 50, na = 100, cl = 100))
  water2 <- suppressWarnings(define_water(
    ph = 7,
    temp = 25,
    alk = 100,
    tot_po4 = 5,
    tot_nh3 = 5,
    tot_bo4 = 5,
    tot_sio4 = 5
  ))

  expect_true(water1@alk_eq == water1@carbonate_alk_eq)
  expect_true(water1@phosphate_alk_eq == 0)
  expect_true(!is.na(water2@phosphate_alk_eq))
  expect_true(!is.na(water2@borate_alk_eq))
  expect_true(!is.na(water2@silicate_alk_eq))
  expect_false(identical(water2@alk_eq, water2@carbonate_alk_eq))

  # Check that specific alkalinity values are calculated correctly
  water3 <- suppressWarnings(define_water(ph = 8.9, temp = 20, alk = 190, tds = 480, tot_po4 = 0.18, tot_nh3 = 0.047, 
                                         tot_bo4 = 0.71, tot_sio4 = 69))
  
  expect_true(round(water3@borate_alk_eq * 1000, 3) == 0.021) # 0.021
  expect_true(round(water3@silicate_alk_eq * 1000, 3) == 0.112) # 0.110
  # commented numbers on the side are from Anthony's spreadsheet with similar starting wq. He said the outputs are close enough!

})

# define_water helpers ----
# Test that define_water_df outputs are the same as base function, define_water.

test_that("define_water_df output is the same as define_water", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9,
    temp = 20,
    alk = 50,
    tot_hard = 50,
    ca = 13,
    mg = 4,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(slice(water_df, 1), output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  # check pluck_cols
  water4 <- suppressWarnings(define_water_df(slice(water_df, 1), pluck_cols = TRUE))

  expect_equal(water1, water3)
  expect_true(ncol(water4) == 5) # defined, ph, and alk
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("define_water_df outputs a water class and the output water argument works", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9,
    temp = 20,
    alk = 50,
    tot_hard = 50,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(slice(water_df, 1), output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  expect_s4_class(water3, "water")
})

# Check that this function can be piped to the next one and can handle a different output_water arg

test_that("define_water_df can be piped", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9,
    temp = 20,
    alk = 50,
    tot_hard = 50,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(slice(water_df, 1), output_water = "new_name"))

  water3 <- water2 %>% balance_ions_df("new_name")

  expect_equal(names(water2[1]), "new_name")
  expect_equal(ncol(water3), 2)
})

test_that("define_water_df correctly calculates dic", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df() %>%
    pluck_water(parameter = "dic")
  water1 <- water1 %>%
    slice(1)

  water2 <- suppressWarnings(define_water(
    ph = 7.9,
    temp = 20,
    alk = 50,
    tot_hard = 50,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  ))
  dic_mol <- water2@tot_co3 * tidywater::mweights$dic * 1000

  expect_equal(dic_mol, water1$defined_dic)
})


test_that("define_water_df passes no2 through", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water_df(slice(water_df, 1:2), no2 = 0.5))
  water2 <- suppressWarnings(define_water_df(mutate(slice(water_df, 1:2), no2 = 0.5)))

  expect_equal(water1$defined[[1]]@no2, convert_units(0.5, "no2", "mg/L N", "M"))
  expect_equal(water2$defined[[2]]@no2, convert_units(0.5, "no2", "mg/L N", "M"))
})

# Testing that define_water_df has "use_col" the right way

test_that("define_water_df use_col reads values correctly from matching columns", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water_df(slice(water_df, 1)))
  expect_equal(water1$defined[[1]]@ph,  water_df$ph[1])
  expect_equal(water1$defined[[1]]@alk, water_df$alk[1])
})

test_that("define_water_df works when arguments are not existing data frame columns", {
  testthat::skip_on_cran()
  
  water1 <- suppressWarnings(data.frame(alk=160, ph=7, toc=4, temp=25) |> 
    define_water_df())
   
  expect_equal(water1$defined[[1]]@alk, 160)
  expect_equal(water1$defined[[1]]@ph, 7)
  expect_equal(water1$defined[[1]]@toc, 4)
  expect_equal(water1$defined[[1]]@temp, 25)



water2 <-suppressWarnings(water_df |> 
  select(-ph) |> 
  mutate(alkalinity = alk) |> 
  define_water_df(ph=7.9, alk = alkalinity)
)
  wtr2<-water_df |> 
    define_water_df()


  # because the first row of water2 has the samme ph as the first row of water_df then the first waters should be equal 
  # but because the second rows have different ph values, then the second waters should not be equal
  expect_equal(wtr2$defined[[1]], water2$defined[[1]])
  expect_error(expect_equal(wtr2$defined[[2]], water2$defined[[2]]))
  
wtrdf<-water_df |> 
     rename(PH=ph, ALK=alk, TEMP=temp, TOC=toc) 
  
water3<-define_water_df(wtrdf, ph=PH, alk=ALK, temp=TEMP, toc=TOC)

  
  water4<- water_df |> 
    define_water_df() 

  expect_equal(water3$defined[[1]],water4$defined[[1]])
  
}) 


