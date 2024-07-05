# Test convertWater converts a water class input to a dataframe
test_that("convert water creates a dataframe", {
  water1 <- define_water(ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 10, toc = 3.5, doc = 3.2, uv254 = 0.1)
  df_water <- convert_water(water1)
  expect_true(is.data.frame(df_water))
})

test_that("convert water works", {
  water1 <- define_water(ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 10, toc = 3.5, doc = 3.2, uv254 = 0.1)
  df_water <- convert_water(water1)
  expect_equal(water1@ph, df_water$ph)
  expect_equal(water1@tot_co3, df_water$tot_co3)
})

# define_water helpers ----

# Test that define_water_once outputs are the same as base function, define_water.

test_that("define_water_once output is the same as define_water", {

  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  water2 <- convert_water(water1)

  water3 <- suppressWarnings(define_water_once(slice(water_df, 1)))

  expect_equal(water2, water3)
})

# Test that define_water_once output is a dataframe

test_that("define_water_once outputs a data frame", {

  water3 <- suppressWarnings(define_water_once(slice(water_df, 1)))

  expect_true(is.data.frame(water3))
})


# Test that define_water_chain outputs are the same as base function, define_water.

test_that("define_water_chain output is the same as define_water", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_chain(slice(water_df, 1), output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  expect_equal(water1, water3)

})

# Test that output is a column of water class lists, and changing the output column name works

test_that("define_water_chain outputs a water class and the output water argument works", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_chain(slice(water_df, 1), output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  expect_s4_class(water3, "water")

})

# Check that this function can be piped to the next one and can handle a different output_water arg

test_that("define_water_chain can be piped", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_chain(slice(water_df, 1), output_water = "new_name"))

  water3 <- water2 %>% balance_ions_chain("new_name")

  expect_equal(names(water2[1]), "new_name")
  expect_equal(ncol(water3), 2)

})


# balance_ions helpers ----
# Check balance_ions_once outputs are the same as base function, balance_ions

test_that("balance_ions_once output is the same as balance_ions", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  water2 <- balance_ions(water1)

  water3 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_once() %>%
    select(-defined_water)

  expect_equal(water2@cl, water3$cl) # check against base
})

# Check that output is a data frame

test_that("balance_ions_once outputs a data frame", {

  water1 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_once() %>%
    select(-defined_water)

  expect_true(is.data.frame(water1))
})


# Test that balance_ions_chain outputs are the same as base function, balance_ions.

test_that("balance_ions_chain outputs are the same as base function, balance_ions", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05))
  water2 <- balance_ions(water1)

  water3 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain()

  water4 <- purrr::pluck(water3, 2, 1)

  expect_equal(water2, water4) # check against base
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("balance_ions_chain output is a column of water class lists", {

  water1 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain()
  water2 <- purrr::pluck(water1, 2, 1)

  expect_s4_class(water2, "water") # check class

})

# Check that this function can be piped to the next one
test_that("balance_ions_chain can be piped and handle an output_water argument", {
  water1 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain(output_water = "different_column") %>%
    chemdose_ph_chain(naoh = 20)

  expect_equal(names(water1[2]), "different_column") # check output_water arg
  expect_equal(ncol(water1), 4) # check if pipe worked

})


# chemdose_ph helpers ----
# Check chemdose_ph_once outputs are the same as base function, chemdose_ph
# Check that output is a data frame

test_that("chemdose_ph_once outputs are the same as base function, chemdose_ph", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 5)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5))

  expect_equal(water1@ph, water2$ph)
})

# Check that output is a data frame

test_that("chemdose_ph_once is a data frame", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5))


  expect_true(is.data.frame(water1))
})

# Check chemdose_ph_once can use a column or function argument for chemical dose

test_that("chemdose_ph_once can use a column and/or function argument for chemical dose", {

  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5))

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 5) %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water"))

  water3 <- water_df %>%
    define_water_chain() %>%
    mutate(naoh = seq(0, 11, 1)) %>%
    chemdose_ph_once(hcl = c(5, 8))

  water4 <- water3 %>%
    slice(11) # same starting wq as water 5

  water5 <- water1 %>%
    slice(6) # same starting wq as water 4

  expect_equal(water1$ph, water2$ph) # test different ways to input chemical
  expect_equal(ncol(water3), 34) # both naoh and hcl dosed
  expect_equal(nrow(water3), 24) # joined correctly
  expect_error(expect_equal(water4$ph, water5$ph)) # since HCl added to water3, pH should be different
})


# Test that chemdose_ph_chain outputs are the same as base function, chemdose_ph.
test_that("chemdose_ph_chain outputs the same as base, chemdose_ph", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 10)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10))

  water3 <- purrr::pluck(water2, 3, 1)

  expect_equal(water1, water3) # check against base
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_ph_chain output is list of water class objects, and can handle an ouput_water arg", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10))

  water2 <- purrr::pluck(water1, 3, 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[3]), "diff_name") # check if output_water arg works
})

# Check that this function can be piped to the next one
test_that("chemdose_ph_chain works", {

  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water"))

  expect_equal(ncol(water1), 4) # check if pipe worked
})

# Check that variety of ways to input chemicals work
test_that("chemdose_ph_chain can handle different ways to input chem doses", {


  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10, output_water = "dosed_chem"))

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = seq(0, 11, 1)) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(hcl = c(5, 8)))

  water4 <- water3 %>%
    slice(21) # same starting wq as water 5

  water5 <- water1 %>%
    slice(11) # same starting wq as water 4

  expect_equal(pluck_water(water1, "dosed_chem", "toc")$toc,
    pluck_water(water2, "dosed_chem_water", "toc")$toc) # test different ways to input chemical
  expect_equal(ncol(water3), 5) # both naoh and hcl dosed
  expect_equal(nrow(water3), 24) # joined correctly
  expect_error(expect_equal(pluck_water(water4, "dosed_chem", "toc")$toc,
    pluck_water(water5, "dosed_chem", "toc")$toc)) # since HCl added to water3, pH should be different

})


# solvedose_ph helper ----
# Check solvedose_ph_once outputs are the same as base function, solvedose_ph

test_that("solvedose_ph_once outputs are the same as base function, solvedose_ph", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    solvedose_ph(target_ph = 9.2, chemical = "naoh")

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  expect_equal(water1, water2$dose_required)
})

# Check that output is a data frame

test_that("solvedose_ph_once outputs data frame", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  expect_true(is.data.frame(water2))
})

# test different ways to input chemical
test_that("solvedose_ph_once can handle different input formats", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(target_ph = 9.2,
      chemical = "naoh") %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", output_column = "caustic_dose"))

  expect_equal(water2$dose_required, water3$caustic_dose)
})


# solvedose_alk helper ----
# Check solvedose_alk_once outputs are the same as base function, solvedose_alk

test_that("solvedose_alk_once outputs are the same as base function, solvedose_alk", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50)) %>%
    balance_ions() %>%
    solvedose_alk(target_alk = 100, chemical = "naoh")

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "naoh"))

  expect_equal(round(water1), round(water2$dose_required))
})

# Check that output is a data frame

test_that("solvedose_alk_once outputs data frame", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "naoh"))

  expect_true(is.data.frame(water2))
})

# test different ways to input chemical
test_that("solvedose_alk_once can handle different input formats", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "na2co3"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(target_alk = 100,
      chemical = "na2co3") %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", output_column = "soda_ash"))

  expect_equal(water2$dose_required, water3$soda_ash)
})


# blend_waters helpers ----
# Check blend_waters_once outputs are the same as base function, blend_waters

test_that("blend_waters_once outputs are the same as base function, blend_waters", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions()

  water2 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 20)

  blend1 <- blend_waters(waters = c(water1, water2), ratios = c(.4, .6))

  blend2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))

  expect_equal(blend1@ph, blend2$ph)
  expect_equal(blend1@temp, blend2$temp)
  expect_equal(blend1@alk, blend2$alk)
})

# Check that output is a data frame

test_that("blend_waters_once outputs a data frame", {

  blend2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))


  expect_true(is.data.frame(blend2))
})

# test different ways to input ratios

test_that("blend_waters_once can handle different ways to input ratios", {

  blend2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))


  blend3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    mutate(ratio1 = .4,
      ratio2 = .6) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c("ratio1", "ratio2")))


  expect_equal(blend2$ph, blend3$ph)
})

# Test that blend_waters_chain outputs are the same as base function, blend_waters
test_that("blend_waters_chain outputs are the same as base function, blend_waters", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions()

  water2 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 20)

  blend1 <- blend_waters(waters = c(water1, water2), ratios = c(.4, .6))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))

  blend2 <- purrr::pluck(water2, 5, 1)

  expect_equal(blend1, blend2)
})

# Test that output is a column of water class lists, and changing the output column name works
test_that("blend_waters_chain outputs a column of water class lists, and output_water arg works", {

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6), output_water = "testoutput"))

  blend2 <- purrr::pluck(water2, 5, 1)

  expect_s4_class(blend2, "water") # check class
  expect_equal(names(water2[5]), "testoutput") # check output_water arg
})


# Check that this function can handle different ways to input ratios
test_that("blend_waters_chain can handle different ways to input ratios", {

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))

  blend2 <- purrr::pluck(water2, 5, 1)

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    mutate(ratio1 = .4,
      ratio2 = .6) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c("ratio1", "ratio2")))

  blend3 <- purrr::pluck(water3, 7, 1)

  expect_equal(blend2, blend3) # test different ways to input ratios
})

# pluck_waters----
test_that("pluck_water works", {

  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    pluck_water(parameter = "tot_co3"))

  tot_co3_water <- purrr::pluck(water1, 1, 4)
  tot_co3_pluck <- water1 %>% slice(4)

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    pluck_water(input_water = c("defined_water", "balanced_water"), parameter = "na"))

  expect_equal(ncol(water1), 2)
  expect_equal(tot_co3_water@tot_co3, tot_co3_pluck$defined_water_tot_co3)
  expect_equal(ncol(water2), 4)
  expect_failure(expect_equal(water2$defined_water_na, water2$balanced_water_na)) # check that Na is being plucked from 2 different waters

})

test_that("pluck_water inputs must be waters and water slots", {
  water1 <- water_df %>%
    define_water_chain("raw") %>%
    mutate(ohno = "not a water")
  water2 <- water_df

  expect_error(water1 %>% pluck_water("raw", c("oops", "ca")))
  expect_error(water2 %>% pluck_water("na", c("na", "ca")))
  expect_error(water1 %>% pluck_water(c("raw", "ohno"), c("na", "ca")))

})

# dissolve_pb helper ----
# Check dissolve_pb_once outputs are the same as base function, dissolve_pb

test_that("dissolve_pb_once outputs are the same as base function, dissolve_pb", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50,
    ca = 13, mg = 4, na = 20, k = 20, cl = 30, so4 = 20, tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    balance_ions() %>%
    dissolve_pb()

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    dissolve_pb_once(input_water = "balanced_water"))

  expect_equal(water1$tot_dissolved_pb, water2$balanced_water_pb)
  expect_equal(water1$controlling_solid, water2$balanced_water_controlling_solid)
})

# Check that output column is numeric

test_that("dissolve_pb_once outputs data frame", {
  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    dissolve_pb_once(input_water = "balanced_water"))

  expect_true(is.numeric(water2$balanced_water_pb))
  expect_true(is.character(water2$balanced_water_controlling_solid))
})

# Check that outputs are different depending on selected source
test_that("dissolve_pb_once processes different input constants", {
  water2 <- suppressWarnings(water_df %>%
    slice(3) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    dissolve_pb_once(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    slice(3) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    dissolve_pb_once(input_water = "balanced_water", pyromorphite = "Xie"))

  expect_equal(water2$balanced_water_controlling_solid, water3$balanced_water_controlling_solid)
  expect_error(expect_equal(water2$balanced_water_pb, water3$balanced_water_pb))
})

# Check that the function stops due to errors in selected source
test_that("dissolve_pb_once errors work", {
  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain())

  expect_error(dissolve_pb_once(water1, input_water = "balanced_water", hydroxypyromorphite = "schock"))
  expect_error(dissolve_pb_once(water1, input_water = "balanced_water", pyromorphite = "Schock"))
  expect_error(dissolve_pb_once(water1, input_water = "balanced_water", laurionite = "Lothebach"))
})

# chemdose_toc helpers ----
test_that("chemdose_toc_once outputs are the same as base function, chemdose_toc", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50, tot_hard = 50, na = 20, k = 20, cl = 30,
    so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05)) %>%
    chemdose_toc(alum = 40)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_toc_once(alum = 40))

  expect_equal(water1@toc, water2$toc)
  expect_equal(water1@doc, water2$doc)
  expect_equal(water1@uv254, water2$uv254)
})

# Check that output is a data frame

test_that("chemdose_toc_once is a data frame", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", alum = 5))

  expect_true(is.data.frame(water1))
})

# Check chemdose_toc_once can use a column or function argument for chemical dose

test_that("chemdose_toc_once can use a column or function argument for chemical dose", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", fe2so43 = 40, coeff = "Ferric"))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(fe2so43 = 40,
      coeff = "Ferric") %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(fe2so43 = 40) %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", coeff = "Ferric"))

  expect_equal(water1$toc, water2$toc) # test different ways to input chemical
  expect_equal(water1$doc, water2$doc)
  expect_equal(water1$uv254, water2$uv254)

  # Test that inputting chemical and coeffs separately (in column and as an argument)  gives save results
  expect_equal(water1$toc, water3$toc)
  expect_equal(water2$doc, water3$doc)
  expect_equal(water2$uv254, water3$uv254)
})


test_that("chemdose_toc_chain outputs are the same as base function, chemdose_toc", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50, tot_hard = 50, na = 20, k = 20, cl = 30,
    so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05) %>%
    chemdose_toc(fecl3 = 40, coeff = "Ferric"))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_toc_chain(fecl3 = 40, coeff = "Ferric", output_water = "coag") %>%
    pluck_water("coag", c("toc", "doc", "uv254")))

  expect_equal(water1@toc, water2$coag_toc)
  expect_equal(water1@doc, water2$coag_doc)
  expect_equal(water1@uv254, water2$coag_uv254)
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_toc_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(input_water = "balanced_water", fe2so43 = 30, coeff = "Ferric"))

  water2 <- purrr::pluck(water1, 4, 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(alum = 10) %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[4]), "diff_name") # check if output_water arg works
})

# Check chemdose_toc_chain can use a column or function argument for chemical dose

test_that("chemdose_toc_chain can use a column or function argument for chemical dose", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", fecl3 = 40, coeff = "Ferric"))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(fecl3 = 40,
      coeff = "Ferric") %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(fecl3 = 40) %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", coeff = "Ferric"))

  expect_equal(water1$toc, water2$toc) # test different ways to input chemical
  expect_equal(water1$doc, water2$doc)
  expect_equal(water1$uv254, water2$uv254)

  # Test that inputting chemical and coeffs separately (in column and as an argument)  gives save results
  expect_equal(water1$toc, water3$toc)
  expect_equal(water2$doc, water3$doc)
  expect_equal(water2$uv254, water3$uv254)
})


# CORROSION ___-------
# calculate_corrosion helpers ----
# Check calculate_corrosion_once outputs are the same as base function, calculate_corrosion

test_that("calculate_corrosion_once outputs are the same as base function, calculate_corrosion", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05) %>%
    balance_ions() %>%
    calculate_corrosion())

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  expect_equal(water1@langelier, water2$langelier)
  expect_equal(water1@ryznar, water2$ryznar)
  expect_equal(water1@aggressive, water2$aggressive)
  expect_equal(water1@csmr, water2$csmr)
  expect_equal(water1@ccpp, water2$ccpp)
  expect_equal(water1@larsonskold, water2$larsonskold)
})

test_that("function catches index typos", {

  water <- suppressWarnings(water_df %>%
    define_water_chain())

  expect_error(calculate_corrosion_chain(water, index = "csr"))
  expect_error(calculate_corrosion_chain(water, index = c("aggressive", "ccccp")))
  expect_no_error(calculate_corrosion_chain(water, index = c("aggressive", "ccpp"))) # no error
  expect_error(calculate_corrosion_once(water, index = "langlier"))
  expect_error(calculate_corrosion_once(water, index = c("ai", "ccccp")))
  expect_no_error(calculate_corrosion_chain(water, index = c("ryznar", "csmr", "larsonskold"))) # no error
})

# Check that output is a data frame

test_that("calculate_corrosion_once is a data frame", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  expect_true(is.data.frame(water1))
})

# Check calculate_corrosion_once outputs an appropriate number of indices

test_that("calculate_corrosion_once outputs an appropriate number of indices", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water", index = c("aggressive", "csmr")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(naoh = 5) %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  water3 <- water1 %>%
    select_if(names(water1) %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))
  water4 <- water2 %>%
    select_if(names(water2) %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))

  expect_error(expect_equal(length(water1), length(water2))) # waters with different indices shouldn't be equal
  expect_equal(length(water3), 2) # indices selected in fn should match # of output index columns
  expect_equal(length(water4), 6)
})


# Test that calculate_corrosion_chain outputs are the same as base function, calculate_corrosion
test_that("calculate_corrosion_chain outputs the same as base, calculate_corrosion", {
  water1 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05) %>%
    balance_ions() %>%
    calculate_corrosion())

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water3 <- purrr::pluck(water2, 3, 1)

  expect_equal(water1, water3) # check against base

})

# Test that output is a column of water class lists, and changing the output column name works

test_that("calculate_corrosion_chain output is list of water class objects, and can handle an ouput_water arg", {

  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water2 <- purrr::pluck(water1, 3, 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[4]), "diff_name") # check if output_water arg works
})


# Check that variety of ways to input chemicals work
test_that("calculate_corrosion_chain can handle different forms of CaCO3", {

  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water", form = "aragonite"))

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water", form = "vaterite"))

  pluck1 <- purrr::pluck(water1, 3)
  pluck2 <- purrr::pluck(water2, 3)
  pluck3 <- purrr::pluck(water3, 3)

  expect_error(expect_equal(pluck1, pluck2))
  expect_error(expect_equal(pluck2, pluck3))
  expect_error(expect_equal(pluck1, pluck3))

})
