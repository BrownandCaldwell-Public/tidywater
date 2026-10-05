# Chemdose TOC ----
library(dplyr)

test_that("chemdose_toc returns the same water when coagulant dose is 0.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  toc_rem1 <- suppressWarnings(chemdose_toc(water1))

  water2 <- suppressWarnings(define_water(ph = 7, toc = 3.5, doc = 3.2, uv254 = 0.1))
  toc_rem2 <- suppressWarnings(chemdose_toc(water2))

  expect_equal(water1, toc_rem1)
  expect_equal(water2, toc_rem2)
})

test_that("chemdose_toc does not run when model_coeff isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))

  expect_error(chemdose_toc(water1, model_coeff = "k1"))
  expect_error(chemdose_toc(water1, model_coeff = c(1, 1, 1, 1, 1, 1)))
  expect_error(chemdose_toc(water1, model_coeff = chemdosetoccoeff[1]))
})

test_that("chemdose_toc handles inputs correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7, uv254 = 0.1))

  expect_warning(chemdose_toc(water1, alum = 20, ferricchloride = 20))
  expect_error(chemdose_toc(water2, alum = 15))
})

test_that("chemdose_toc works.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water2 <- chemdose_toc(water1, alum = 30)
  water3 <- chemdose_toc(water1, ferricchloride = 50, model_coeff = "Ferric")
  water4 <- chemdose_toc(
    water1,
    ferricchloride = 50,
    model_coeff = data.frame(x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068)
  )
  # ACH and PACl are the same as alum when mols of Al3+ are the same
  water7 <- chemdose_toc(water1, ach = 30 * mweights$ach / mweights$alum)
  water8 <- chemdose_toc(water1, pacl = 30 * mweights$al2o3 / mweights$alum)

  water1_with_alk <- suppressWarnings(define_water(ph = 7, alk = 10, doc = 3.5, uv254 = 0.1))
  water5 <- suppressWarnings(chemdose_toc(water1_with_alk, alum = 30, caoh2 = 2))
  water6 <- suppressWarnings(chemdose_toc(water1_with_alk, caoh2 = 2))

  # Used to generate expected outputs cross check with edwards97 package
  # data = data.frame(DOC = 3.5, dose = convert_units(50, "ferricchloride", endunit = "mM"), pH = 7, UV254 = .1)
  # coagulate(data, coefs = edwards_coefs("Fe"))

  expect_equal(round(water2@doc, 1), 2.8)
  expect_equal(round(water3@doc, 1), 2.2)
  expect_equal(round(water4@doc, 1), 2.2)
  expect_equal(round(water7@doc, 1), round(water2@doc, 1))
  expect_equal(round(water8@doc, 1), round(water2@doc, 1))

  expect_false(water2@doc == water5@doc)
  expect_equal(round(water5@doc, 1), 3)
  expect_equal(round(water5@uv254, 2), 0.04)
  expect_equal(water1@doc, water6@doc)
  expect_equal(water1@uv254, water6@uv254)
})

test_that("Edwards model flatlines the xterm for ph>=8",{
  testthat::skip_on_cran()

  water1<- suppressWarnings(define_water(ph = 6, uv254=0.01, toc = 4))
  water2<- suppressWarnings(define_water(ph = 8, uv254=0.01, toc = 4))
  water3<- suppressWarnings(define_water(ph = 10, uv254=0.01, toc = 4))
  
  dosed1<-suppressWarnings(chemdose_toc(water1,alum=40))
  dosed2<-suppressWarnings(chemdose_toc(water2,alum=40))  
  dosed3<-suppressWarnings(chemdose_toc(water3,alum=40))
  
  # resulting toc for 8 and 10 should be the same
  expect_equal(dosed2@doc,dosed3@doc)
 # resulting toc for below 8 and 8 should be diffrent 
  expect_false(dosed1@doc == dosed2@doc)

})

test_that("Edwards model does not go negative because of high SUVA",{

  water1<- suppressWarnings(define_water(ph = 7, uv254=0.01, doc = 2, tds=50))
  water2<- suppressWarnings(define_water(ph = 7, uv254=0.1, doc = 2, tds=50))
  water3<- suppressWarnings(define_water(ph = 7, uv254=01, doc = 2, tds=50))
  
  dosed1<-suppressWarnings(chemdose_toc(water1,alum=20))
  dosed2<-suppressWarnings(chemdose_toc(water2,alum=20))  
  dosed3<-suppressWarnings(chemdose_toc(water3,alum=20))

  # result from high SUVA should be same 
  expect_equal(dosed2@doc,dosed3@doc)
  # result from SUVAs (one at SUVA=5 and one at SUVA<5) should be differnet
  expect_false(dosed1@doc == dosed2@doc)

})

test_that("chemdose_toc works with reuse model", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 6, doc = 7.8, uv254 = 0.1))
  custom_coeff <- data.frame(
    fns = 0.6,
    z0 = 620,
    z1 = -160,
    z2 = 9.2
  )

  water1 <- chemdose_toc(water0, alum = 3)
  water2 <- chemdose_toc(water0, alum = 50, model_coeff = "Reuse General Langmuir")
  water3 <- chemdose_toc(water0, alum = 50, model_coeff = "Reuse General Linear")
  water4 <- chemdose_toc(water0, alum = 50, model_coeff = "Reuse Site Langmuir", fns = 0.4)
  water5 <- chemdose_toc(water0, alum = 80, model_coeff = "Reuse General Langmuir")
  water6 <- chemdose_toc(water0, alum = 50, model_coeff = custom_coeff)

  perc_rem2 <- (water0@doc - water2@doc) / water0@doc * 100
  perc_rem3 <- (water0@doc - water3@doc) / water0@doc * 100
  perc_rem4 <- (water0@doc - water4@doc) / water0@doc * 100
  perc_rem5 <- (water0@doc - water5@doc) / water0@doc * 100

  expect_false(water1@doc == water2@doc)
  expect_false(water2@doc == water3@doc)
  expect_false(water2@doc == water4@doc)
  expect_false(water6@doc == water2@doc)
  expect_equal(round(perc_rem2), 29) # for mgAl/mgC ratio of 0.5-1, expected removal is 2-35%
  expect_equal(round(perc_rem3), 29)
  expect_equal(round(perc_rem4), 48) # high because of lower fns
  expect_equal(round(perc_rem5), 31) # max % rem at alum = 80, doc = 7.8 was 43%
})

test_that("chemdose_toc reuse model works at high pH", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 9, doc = 3.5, uv254 = 0.1))

  water1 <- suppressWarnings(chemdose_toc(water0, alum = 3, coeff = "Reuse General Langmuir"))
  water2 <- suppressWarnings(chemdose_toc(water0, alum = 3, coeff = "Reuse General Linear"))
  expect_warning(chemdose_toc(water0, alum = 3, coeff = "Reuse General Langmuir"), "5.5")

  expect_true(water1@doc > 0)
  expect_true(water2@doc > 0)
  expect_true(round(water1@doc, 1) == 3.3) # make sure it's a reasonable result
})

test_that("chemdose_toc reuse UV254 follows Wilder & Korak (2025) regression", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 6, doc = 8, uv254 = 0.16))
  water1 <- chemdose_toc(water0, alum = 80, coeff = "Reuse General Langmuir")

  doc_rem <- (water0@doc - water1@doc) / water0@doc * 100
  uv_rem <- (water0@uv254 - water1@uv254) / water0@uv254 * 100

  expect_equal(uv_rem, 0.97 * doc_rem + 5)
  expect_equal(round(doc_rem), 31)
  expect_equal(round(uv_rem), 35)
  expect_true(uv_rem > doc_rem) # preferential UV254 removal
})

test_that("chemdose_toc reuse models handle fns, coagulant type, and validity ranges", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 6, doc = 8, uv254 = 0.16))
  alum <- chemdose_toc(water0, alum = 80, coeff = "Reuse General Langmuir")

  # fns
  expect_error(chemdose_toc(water0, alum = 80, coeff = "Reuse Site Linear"))
  expect_error(chemdose_toc(water0, alum = 80, coeff = "Reuse Site Linear", fns = 1.2))
  expect_warning(chemdose_toc(water0, alum = 80, coeff = "Reuse General Linear", fns = 0.3), "ignored")

  # ACH and PACl equal alum at the same Al3+; ferric errors
  ach <- chemdose_toc(water0, ach = 80 * mweights$ach / mweights$alum, coeff = "Reuse General Langmuir")
  pacl <- chemdose_toc(water0, pacl = 80 * mweights$al2o3 / mweights$alum, coeff = "Reuse General Langmuir")
  expect_equal(ach@doc, alum@doc)
  expect_equal(pacl@doc, alum@doc)
  expect_error(chemdose_toc(water0, ferricchloride = 80, coeff = "Reuse General Langmuir"), "aluminum")

  # validity ranges
  expect_warning(chemdose_toc(water0, alum = 10, coeff = "Reuse General Langmuir"), "200")
})

################################################################################*
################################################################################*
# chemdose_toc helpers ----

test_that("chemdose_toc_df is a data frame and has correct classes in output columns", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df() %>%
    chemdose_toc_df(
      alum = 20,
      output_water = "coag",
      pluck_cols = T,
      water_prefix = FALSE,
      keep_dose_cols = TRUE
    )

  expect_true(is.data.frame(water1))
  expect_equal(colnames(water1), c("defined", "alum", "coag", "toc", "doc", "uv254"))
  expect_s4_class(water1$coag[[1]], "water")
  expect_true(is.numeric(water1$toc[1]))
})


test_that("chemdose_toc_df outputs are the same as base function, chemdose_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(
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
  )

  water1 <- chemdose_toc(water0, ferricchloride = 40, model_coeff = "Ferric")

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(ferricchloride = 40, model_coeff = "Ferric", output_water = "coag") %>%
    pluck_water("coag", c("toc", "doc", "uv254"))

  coag_doses <- data.frame(ferricchloride = seq(10, 100, 10))
  water3 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      define_water_df("raw") %>%
      merge(coag_doses) %>%
      chemdose_toc_df("raw", "coag", model_coeff = "Ferric") %>%
      pluck_water(c("coag"), c("doc"))
  )

  water4 <- water_df %>%
    slice(1) %>%
    define_water_df("raw") %>%
    merge(coag_doses) %>%
    {
      names(.)[names(.) == "ferricchloride"] <- "Coagulant"
      .
    } %>%
    chemdose_toc_df("raw", "coag", model_coeff = "Ferric", ferricchloride = Coagulant) %>%
    pluck_water(c("coag"), c("doc"))

  expect_equal(water1@toc, water2$coag_toc)
  expect_equal(water1@doc, water2$coag_doc)
  expect_equal(water1@uv254, water2$coag_uv254)
  expect_equal(water1@doc, water3$coag_doc[4])
  expect_equal(water1@doc, water4$coag_doc[4])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_toc_df output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      define_water_df() %>%
      chemdose_toc_df(input_water = "defined", ferricsulfate = 30, model_coeff = "Ferric")
  )

  water2 <- purrr::pluck(water1, "coagulated", 1)

  water3 <- suppressWarnings(
    water_df %>%
      define_water_df() %>%
      mutate(alum = 10) %>%
      chemdose_toc_df(output_water = "diff_name")
  )

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check chemdose_toc_df can use a column or function argument for chemical dose

test_that("chemdose_toc_df can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(
      input_water = "defined",
      ferricchloride = 40,
      model_coeff = "Ferric",
      pluck_cols = TRUE
    )

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    mutate(
      ferricchloride = 40,
      model_coeff = "Ferric"
    ) %>%
    chemdose_toc_df(pluck_cols = T)

  # Test that pluck_cols does the same thing as pluck_water function
  water3 <- water_df %>%
    slice(1) %>%
    define_water_df("raw") %>%
    mutate(ferricchloride = 40) %>%
    chemdose_toc_df(input_water = "raw", output_water = "final", model_coeff = "Ferric") %>%
    pluck_water(input_waters = "final", c("toc", "doc", "uv254"))

  expect_equal(water1$coagulated_toc, water2$coagulated_toc) # test different ways to input chemical
  expect_equal(water1$coagulated_doc, water2$coagulated_doc)
  expect_equal(water1$coagulated_uv254, water2$coagulated_uv254)

  # Test that inputting chemical and coeff separately (in column and as an argument)  gives same results
  expect_equal(water1$coagulated_toc, water3$final_toc)
  expect_equal(water1$coagulated_doc, water3$final_doc)
  expect_equal(water1$coagulated_uv254, water3$final_uv254)
})

test_that("chemdose_toc_df works when water_prefix is false", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      define_water_df() %>%
      chemdose_toc_df(ferricsulfate = 23, pluck_cols = TRUE)
  )

  water2 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      define_water_df() %>%
      chemdose_toc_df(ferricsulfate = 23, water_prefix = FALSE)
  )

  expect_equal(water1$coagulated_water_toc, water2$toc)
  expect_equal(water1$coagulated_water_uv254, water2$uv254)
})

test_that("chemdose_toc_df works with custom coefficients", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(alum = 20, pluck_cols = TRUE)

  custom_coeff <- data.frame(x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068)
  water1 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(alum = 20, model_coeff = custom_coeff, pluck_cols = TRUE)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(
      alum = 20,
      model_coeff = data.frame(x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068),
      pluck_cols = TRUE
    )

  expect_false(water0$coagulated_doc == water1$coagulated_doc)
  expect_false(water0$coagulated_toc == water1$coagulated_toc)
  expect_equal(water1$coagulated_doc, water2$coagulated_doc)
  expect_equal(water1$coagulated_toc, water2$coagulated_toc)
})
