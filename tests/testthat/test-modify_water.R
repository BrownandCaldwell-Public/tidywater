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
  water3 <- modify_water(water0, "no2", 1, "mg/L N")

  expect_s4_class(water1, "water")
  expect_equal(water1@br, convert_units(50, "br", "ug/L"))
  expect_equal(water0@tds, water1@tds)
  expect_equal(water2@na, convert_units(50, "na", "mg/L"))
  expect_equal(water0@ph, water2@ph)
  # 1 mg/L as N / 14.0067 g/mol N / 1000 mg/g
  expect_equal(water3@no2, 1 / 14.0067 / 1000)
})

test_that("Modify water works with multiple inputs", {
  water0 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  water1 <- modify_water(water0, slot = c("br", "na"), value = c(50, 50), units = c("ug/L", "mg/L"))

  expect_s4_class(water1, "water")
  expect_equal(water1@br, convert_units(50, "br", "ug/L"))
  expect_equal(water1@na, convert_units(50, "na", "mg/L"))
  expect_equal(water0@ph, water1@ph)
})

test_that("Modify water df takes and returns correct argument types and classes.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df("test") %>%
    mutate(bromide = 50)

  water1 <- modify_water_df(water0, "test", "modified", "br", bromide, "ug/L")
  water2 <- modify_water_df(water0, "test", "modified", "br", 50, "ug/L")

  expect_error(modify_water_df(water_df, "br", 50, "ug/L"))
  expect_error(modify_water_df(water0, 50, "ug/L"))
  expect_equal(water1$modified[[1]]@br, water2$modified[[1]]@br)
  expect_s4_class(water1$modified[[1]], "water")
})

test_that("Modify water df works with multiple inputs.", {
  testthat::skip_on_cran()

  water1 <- water_df %>%
    define_water_df() %>%
    mutate(
      bromide = 50,
      sodium = 60
    ) %>%
    modify_water_df(output_water = "mod1", slot = "br", value = bromide, units = "ug/L") %>%
    modify_water_df("mod1", "mod2", slot = "na", value = sodium, units = "mg/L")

  water2 <- water_df %>%
    define_water_df() %>%
    modify_water_df(
      slot = c("br", "na"),
      value = c(50, 60),
      units = c("ug/L", "mg/L")
    )

  expect_s4_class(water2$modified[[1]], "water")
  expect_equal(water1$mod2, water2$modified) # can input to function or as col
  expect_equal(water2$modified[[1]]@br, convert_units(50, "br", "ug/L"))
  expect_equal(water2$modified[[1]]@na, convert_units(60, "na", "mg/L"))
  expect_equal(nrow(water2), nrow(water_df))
})

test_that("Modify water converts organics and DBPs to stored units.", {
  water0 <- define_water(ph = 7, temp = 25, alk = 100, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_equal(modify_water(water0, "toc", 3000, "ug/L")@toc, 3)
  expect_equal(modify_water(water0, "doc", 2500, "ug/L")@doc, 2.5)
  expect_equal(modify_water(water0, "bdoc", 500, "ug/L")@bdoc, 0.5)
  expect_equal(modify_water(water0, "chcl3", 0.05, "mg/L")@chcl3, 50)
  expect_equal(modify_water(water0, "haa5", 0.06, "mg/L")@haa5, 60)
  expect_equal(modify_water(water0, "tbaa", 5, "ug/L")@tbaa, 5)
  expect_equal(modify_water(water0, "tbaa", 0.005, "mg/L")@tbaa, 5)
})

test_that("Modify water df applies arguments passed as constants.", {
  testthat::skip_on_cran()
  water0 <- define_water(ph = 7, alk = 50, na = 20, cl = 20, tds = 200)
  df <- data.frame(id = 1:2)
  df$defined <- list(water0, water0)
  df$bromide <- c(50, 100)

  water1 <- modify_water_df(df, slot = "na", value = 40, units = "mg/L")
  water2 <- modify_water_df(df, slot = "br", value = bromide, units = "ug/L")

  expect_equal(water1$modified[[1]]@na, convert_units(40, "na", "mg/L"))
  expect_equal(water2$modified[[1]]@br, convert_units(50, "br", "ug/L"))
  expect_equal(water2$modified[[2]]@br, convert_units(100, "br", "ug/L"))
  expect_error(modify_water_df(df, slot = "na", value = 40))
})
