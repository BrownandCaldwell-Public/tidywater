#Test convertWater converts a water class input to a dataframe
test_that("convertWater creates a dataframe", {
  water1 <- define_water(6.7, 20, 20, 50, 40, 10, 10, 10, 10)
  df_water <- convert_water(water1)
  expect_true(is.data.frame(df_water))
})

test_that("convert water works", {
  water1 <- define_water(6.7, 20, 20, 50, 40, 10, 10, 10, 10)
  df_water <- convert_water(water1)
  expect_equal(water1@ph, df_water$ph)
  expect_equal(water1@tot_co3, df_water$tot_co3)
  })

# define_water helpers ----

# Test that define_water_once outputs are the same as base function, define_water.

test_that("define_water_once output is the same as define_water", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  water2 <- convert_water(water1)

  water3 <- define_water_once(slice(water_df, 1))

  expect_equal(water2, water3)
})

# Test that define_water_once output is a dataframe

test_that("define_water_once outputs a data frame", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  water2 <- convert_water(water1)

  water3 <- define_water_once(slice(water_df, 1))

  expect_true(is.data.frame(water3))
})


# Test that define_water_chain outputs are the same as base function, define_water.

test_that("define_water_chain output is the same as define_water", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  # water2 <- convert_Water(water1)

  water2 <- define_water_chain(slice(water_df, 1), output_water = "new_name")
  water3 <- purrr::pluck(water2, 1, 1)

  expect_equal(water1, water3)

})

# Test that output is a column of water class lists, and changing the output column name works

test_that("define_water_chain outputs a water class and the output water argument works", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  # water2 <- convert_Water(water1)

  water2 <- define_water_chain(slice(water_df, 1), output_water = "new_name")
  water3 <- purrr::pluck(water2, 1, 1)

  expect_s4_class(water3, "water")

})

# Check that this function can be piped to the next one and can handle a different output_water arg

test_that("define_water_chain can be piped", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  # water2 <- convert_Water(water1)

  water2 <- define_water_chain(slice(water_df, 1), output_water = "new_name")

  water3 <- water2 %>% balance_ions_chain("new_name")

  expect_equal(names(water2[1]), "new_name")
  expect_equal(ncol(water3), 2)

})


# balance_ions helpers ----
# Check balance_ions_once outputs are the same as base function, balance_ions

test_that("balance_ions_once output is the same as balance_ions", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  water2 <- balance_ions(water1)

  water3 <- define_water_chain(slice(water_df, 1)) %>%
    balance_ions_once() %>%
    select(-defined_water)

  expect_equal(water2@cl, water3$cl) #check against base
})

# Check that output is a data frame

test_that("balance_ions_once outputs a data frame", {

  water1 <- define_water_chain(slice(water_df, 1)) %>%
    balance_ions_once() %>%
    select(-defined_water)

  expect_true(is.data.frame(water1))
})


# Test that balance_ions_chain outputs are the same as base function, balance_ions.

test_that("balance_ions_chain outputs are the same as base function, balance_ions", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0)
  water2 <- balance_ions(water1)

  water3 <- define_water_chain(slice(water_df, 1)) %>%
    balance_ions_chain()
  water4 <- purrr::pluck(water3, 2,1)

  expect_equal(water2, water4) #check against base

})

# Test that output is a column of water class lists, and changing the output column name works

test_that("balance_ions_chain output is a column of water class lists", {

  water1<- define_water_chain(slice(water_df, 1)) %>%
    balance_ions_chain()
  water2 <- purrr::pluck(water1, 2, 1)

  expect_s4_class(water2, "water") # check class

})

# Check that this function can be piped to the next one
test_that("balance_ions_chain can be piped and handle an output_water argument", {
 water1 <- define_water_chain(slice(water_df, 1)) %>%
    balance_ions_chain(output_water = "different_column") %>%
    chemdose_ph_chain(naoh = 20)

  expect_equal(names(water1[2]), "different_column") #check output_water arg
  expect_equal(ncol(water1), 4) #check if pipe worked

})


# chemdose_ph helpers ----
# Check chemdose_ph_once outputs are the same as base function, chemdose_ph
# Check that output is a data frame

test_that("chemdose_ph_once outputs are the same as base function, chemdose_ph", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 5)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5)

  expect_equal(water1@ph, water2$ph)
})

# Check that output is a data frame

test_that("chemdose_ph_once is a data frame", {

  water1 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5)


  expect_true(is.data.frame(water1))
})

# Check chemdose_ph_once can use a column or function argument for chemical dose

test_that("chemdose_ph_once can use a column or function argument for chemical dose", {

  water1 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water", naoh = 5)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(naoh =5) %>%
    balance_ions_chain() %>%
    chemdose_ph_once(input_water = "balanced_water")

  expect_equal(water1$ph, water2$ph) # test different ways to input chemical
})


# Test that chemdose_ph_chain outputs are the same as base function, chemdose_ph.
test_that("chemdose_ph_chain outputs the same as base, chemdose_ph", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions() %>%
    chemdose_ph(naoh =10)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10)

  water3 <- purrr::pluck(water2, 3,1)

  expect_equal(water1, water3) #check against base
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_ph_chain output is list of water class objects, and can handle an ouput_water arg", {

  water1 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10)

  water2 <- purrr::pluck(water1, 3,1)

  water3 <- water_df %>%
    define_water_chain() %>%
    mutate(naoh =10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(output_water = "diff_name")

  expect_s4_class(water2, "water") #check class
  expect_equal(names(water3[3]), "diff_name") #check if output_water arg works
})

# Check that this function can be piped to the next one
test_that("chemdose_ph_chain works", {

  water1 <- water_df %>%
    define_water_chain() %>%
    mutate(naoh =10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water")

  expect_equal(ncol(water1), 4) #check if pipe worked
})

# Check that variety of ways to input chemicals work
test_that("chemdose_ph_chain can handle different ways to input chem doses", {


  water4 <- water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10, output_water = "dosed_chem") %>% # check out put water changes
    solvedose_ph_once(input_water = "dosed_chem", target_ph = 10.5, chemical = "naoh")

  water5 <- water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water")

  pluck4 <- purrr::pluck(water4, 3)
  pluck5 <- purrr::pluck(water5, 3)

  expect_equal(pluck4, pluck5) # test different ways to input chemical
})


# solvedose_ph helper ----
# Check solvedose_ph_once outputs are the same as base function, solvedose_ph
# Check that output is a data frame

test_that("solvedose_ph_once works", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions() %>%
    solvedose_ph(target_ph = 9.2,  chemical = "naoh")

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh")

  water3 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(target_ph = 9.2,
           chemical = "naoh") %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", output_column = "caustic_dose")

  expect_equal(water1, water2$dose_required)
  expect_true(is.data.frame(water2))
  expect_equal(water2$dose_required, water3$caustic_dose) # test different ways to input chemical
})



# blend_waters helpers ----
# Check blend_waters_once outputs are the same as base function, blend_waters
# Check that output is a data frame

test_that("blend_waters_once works", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions()

  water2 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 20)

  blend1 <- blend_waters(waters = c(water1, water2), ratios = c(.4, .6))

  blend2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6))


  blend3 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    mutate(ratio1 = .4,
           ratio2 = .6) %>%
    blend_waters_once(waters = c("balanced_water", "dosed_chem_water"), ratios = c("ratio1", "ratio2"))

  expect_equal(blend1@ph, blend2$ph)
  expect_equal(blend1@temp, blend2$temp)
  expect_equal(blend1@alk, blend2$alk)
  expect_true(is.data.frame(blend2))
  expect_equal(blend2$ph, blend3$ph) # test different ways to input chemical
})


# Test that blend_waters_chain outputs are the same as base function, blend_waters
# Test that output is a column of water class lists, and changing the output column name works
# Check that this function can be piped to the next one
test_that("blend_waters_chain works", {
  water1 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions()

  water2 <- define_water(7.9, 20, 50, 50, 50, 20, 20, 30, 20, 0, 0) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 20)

  blend1 <- blend_waters(waters = c(water1, water2), ratios = c(.4, .6))

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6))

  blend2 <- purrr::pluck(water2, 5, 1)

  water3 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    mutate(ratio1 = .4,
           ratio2 = .6) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c("ratio1", "ratio2"))

  blend3 <- purrr::pluck(water3,7, 1)

  expect_equal(blend1, blend2)
  expect_s4_class(blend2, "water") #check class
  expect_equal(blend2, blend3) # test different ways to input chemical
})
