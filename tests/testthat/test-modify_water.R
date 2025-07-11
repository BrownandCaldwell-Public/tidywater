test_that("Modify water errors when arguments are wrong", {
  water0 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(modify_water(water0, "br", 50))
  expect_error(modify_water(water0, "br", 50, "cm-1"))
  expect_error(modify_water(water0, "br", "50", "ug/L"))
  expect_error(modify_water(water0, "bromide", 50, "ug/L"))
  expect_error(modify_water(water_df, "br", 50, "ug/L"))
})


test_that("Modify water works", {
  water0 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  water1 <- modify_water(water0, "br", 50, "ug/L")
  water2 <- modify_water(water0, "na", 50, "mg/L")

  expect_s4_class(water1, "water")
  expect_equal(water1@br, convert_units(50, "br", "ug/L"))
  expect_equal(water0@tds, water1@tds)
  expect_equal(water2@na, convert_units(50, "na", "mg/L"))
  expect_equal(water0@ph, water2@ph)
})

test_that("Modify water works with multiple inputs", {
  water0 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  
  water1 <- modify_water(water0, slot = c("br", "na"), value = c(50, 50), units = c("ug/L", "mg/L"))
  
  expect_s4_class(water1, "water")
  expect_equal(water1@br, convert_units(50, "br", "ug/L"))
  expect_equal(water1@na, convert_units(50, "na", "mg/L"))
  expect_equal(water0@ph, water1@ph)
})

test_that("Modify water chain takes and returns correct argument types and classes.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_chain("test") %>%
    mutate(bromide = 50)

  water1 <- modify_water_chain(water0, "test", "modified", "br", bromide, "ug/L")
  water2 <- modify_water_chain(water0, "test", "modified", "br", 50, "ug/L")

  expect_error(modify_water_chain(water_df, "br", 50, "ug/L"))
  expect_error(modify_water_chain(water0, 50, "ug/L"))
  expect_s4_class(water1$modified[[1]], "water")
})

test_that("Modify water chain works with multiple inputs.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_chain("test") %>%
    mutate(bromide = 50, sodium = 50)
  
  water1 <- modify_water_chain(water0, "test", "modified", 
                               slot = c("br", "na"), value = c("bromide", "sodium"), units = c("ug/L", "mg/L")) %>%
    pluck_water(input_waters = "modified", parameter = c("br", "na"))
  
  expect_s4_class(water1$modified[[1]], "water")
  expect_true(all.equal(convert_units(water1$modified_br, "br", "M", "ug/L"), water0$bromide))
  expect_true(all.equal(convert_units(water1$modified_na, "na", "M", "mg/L"), water0$sodium))
})
