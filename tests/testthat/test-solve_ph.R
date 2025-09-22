# Solve pH ----

test_that("Solve pH returns correct pH with no chemical dosing.", {
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
    water2 <- define_water(ph = 5, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
    water3 <- define_water(ph = 10, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
  })

  water4 <- define_water(6.7, 20, 20, 70, 10, 10, 10, 10, 10, 10)
  water5 <- define_water(7.5, 20, 100, 70, 10, 10, 10, 10, 10)
  water6 <- define_water(7.5, 20, 20, 70, 10, 10, 10, 10, 10)
  water7 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10)

  water8 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water9 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water10 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, tot_po4 = 3)
  water11 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, free_chlorine = 3)
  water12 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, tot_nh3 = 3)

  expect_equal(solve_ph(water1), water1@ph)
  expect_equal(solve_ph(water2), water2@ph)
  expect_equal(solve_ph(water3), water3@ph)
  expect_equal(solve_ph(water4), water4@ph)
  expect_equal(solve_ph(water5), water5@ph)
  expect_equal(solve_ph(water6), water6@ph)
  expect_equal(solve_ph(water7), water7@ph)
  expect_equal(solve_ph(water8), water8@ph)
  expect_equal(solve_ph(water9), water9@ph)
  expect_equal(solve_ph(water10), water10@ph)
  expect_equal(solve_ph(water11), water11@ph)
  expect_equal(solve_ph(water12), water12@ph)
})

# Backend Comparison Tests ----

test_that("R and Rust backends produce identical results for basic water conditions", {
  # Test various water conditions without doses
  test_waters <- list(
    define_water(ph = 7, temp = 25, alk = 100, ca = 50, mg = 10, na = 20, k = 5, cl = 30, so4 = 40),
    define_water(ph = 5, temp = 20, alk = 50, ca = 30, mg = 5, na = 10, k = 2, cl = 15, so4 = 20),
    define_water(ph = 9, temp = 30, alk = 200, ca = 80, mg = 20, na = 40, k = 8, cl = 60, so4 = 80),
    define_water(ph = 6.5, temp = 15, alk = 75, ca = 40, mg = 8, na = 15, k = 3, cl = 25, so4 = 30),
    define_water(ph = 8.5, temp = 35, alk = 150, ca = 60, mg = 15, na = 30, k = 6, cl = 45, so4 = 50)
  )

  for (i in seq_along(test_waters)) {
    water <- test_waters[[i]]
    ph_r <- solve_ph(water, backend = "r")
    ph_rust <- solve_ph(water, backend = "rust")

    expect_equal(ph_r, ph_rust, tolerance = 0.01,
                 label = paste("Water condition", i, "- R:", ph_r, "Rust:", ph_rust))
  }
})

test_that("R and Rust backends produce very close results with chemical doses", {
  # Base water for testing
  water <- define_water(ph = 7.5, temp = 25, alk = 120, ca = 60, mg = 15, na = 25, k = 6, cl = 35, so4 = 50)

  # Test various dose combinations
  dose_combinations <- list(
    list(so4_dose = 0.001),
    list(na_dose = 0.002),
    list(ca_dose = 0.003),
    list(mg_dose = 0.001),
    list(cl_dose = 0.002),
    list(mno4_dose = 0.0005),
    list(no3_dose = 0.001),
    list(so4_dose = 0.001, ca_dose = 0.002),
    list(na_dose = 0.002, mg_dose = 0.001),
    list(so4_dose = 0.002, na_dose = 0.001, ca_dose = 0.003),
    list(so4_dose = 0.001, na_dose = 0.001, ca_dose = 0.002, mg_dose = 0.001, cl_dose = 0.001)
  )

  for (i in seq_along(dose_combinations)) {
    doses <- dose_combinations[[i]]

    # Call with R backend
    ph_r <- do.call(solve_ph, c(list(water = water, backend = "r"), doses))

    # Call with Rust backend
    ph_rust <- do.call(solve_ph, c(list(water = water, backend = "rust"), doses))

    # Allow slightly larger tolerance for complex dose combinations
    tolerance <- 0.01

    expect_equal(ph_r, ph_rust, tolerance = tolerance,
                 label = paste("Dose combination", i, "- R:", ph_r, "Rust:", ph_rust,
                             "Doses:", paste(names(doses), doses, sep = "=", collapse = ", ")))
  }
})

test_that("R and Rust backends handle edge cases consistently", {
  # Test with phosphate
  water_po4 <- define_water(ph = 7, temp = 25, alk = 100, ca = 50, mg = 10, na = 20, k = 5,
                           cl = 30, so4 = 40, tot_po4 = 0.005)
  ph_r_po4 <- solve_ph(water_po4, backend = "r")
  ph_rust_po4 <- solve_ph(water_po4, backend = "rust")
  expect_equal(ph_r_po4, ph_rust_po4, tolerance = 0.01)

  # Test with free chlorine
  water_cl <- define_water(ph = 7, temp = 25, alk = 100, ca = 50, mg = 10, na = 20, k = 5,
                          cl = 30, so4 = 40, free_chlorine = 2)
  ph_r_cl <- solve_ph(water_cl, backend = "r")
  ph_rust_cl <- solve_ph(water_cl, backend = "rust")
  expect_equal(ph_r_cl, ph_rust_cl, tolerance = 0.01)

  # Test with ammonia
  water_nh3 <- define_water(ph = 7, temp = 25, alk = 100, ca = 50, mg = 10, na = 20, k = 5,
                           cl = 30, so4 = 40, tot_nh3 = 1)
  ph_r_nh3 <- solve_ph(water_nh3, backend = "r")
  ph_rust_nh3 <- solve_ph(water_nh3, backend = "rust")
  expect_equal(ph_r_nh3, ph_rust_nh3, tolerance = 0.01)

  # Test extreme temperature
  water_hot <- define_water(ph = 7, temp = 40, alk = 100, ca = 50, mg = 10, na = 20, k = 5,
                           cl = 30, so4 = 40)
  ph_r_hot <- solve_ph(water_hot, backend = "r")
  ph_rust_hot <- solve_ph(water_hot, backend = "rust")
  expect_equal(ph_r_hot, ph_rust_hot, tolerance = 0.01)

  water_cold <- define_water(ph = 7, temp = 5, alk = 100, ca = 50, mg = 10, na = 20, k = 5,
                            cl = 30, so4 = 40)
  ph_r_cold <- solve_ph(water_cold, backend = "r")
  ph_rust_cold <- solve_ph(water_cold, backend = "rust")
  expect_equal(ph_r_cold, ph_rust_cold, tolerance = 0.01)
})

test_that("Statistical comparison of R and Rust backends across many conditions", {
  # Generate random water conditions for statistical testing
  set.seed(42)  # For reproducible tests
  n_tests <- 200

  ph_differences <- numeric(n_tests)

  for (i in 1:n_tests) {
    # Generate random but realistic water parameters
    water <- define_water(
      ph = runif(1, 5, 10),
      temp = runif(1, 5, 40),
      alk = runif(1, 20, 300),
      ca = runif(1, 10, 150),
      mg = runif(1, 2, 50),
      na = runif(1, 5, 100),
      k = runif(1, 1, 20),
      cl = runif(1, 5, 200),
      so4 = runif(1, 5, 150),
      tot_po4 = runif(1, 0, 0.01)
    )

    # Add random doses
    doses <- list(
      so4_dose = runif(1, 0, 0.005),
      na_dose = runif(1, 0, 0.005),
      ca_dose = runif(1, 0, 0.005),
      mg_dose = runif(1, 0, 0.003),
      cl_dose = runif(1, 0, 0.005)
    )

    # Calculate pH with both backends
    ph_r <- do.call(solve_ph, c(list(water = water, backend = "r"), doses))
    ph_rust <- do.call(solve_ph, c(list(water = water, backend = "rust"), doses))

    ph_differences[i] <- abs(ph_r - ph_rust)
  }

  # Statistical checks
  mean_diff <- mean(ph_differences)
  max_diff <- max(ph_differences)

  # Mean difference should be reasonable (allowing for numerical precision differences)
  expect_lt(mean_diff, 0.02,
           label = paste("Mean absolute difference:", round(mean_diff, 4)))

  # Maximum difference should be reasonable (some edge cases may have larger differences)
  expect_lt(max_diff, 0.02,
           label = paste("Maximum absolute difference:", round(max_diff, 4)))

  # At least 99% of results should be within 0.01 pH units (allowing for random variation)
  within_tolerance <- sum(ph_differences <= 0.01) / n_tests
  expect_gte(within_tolerance, 0.99,
            label = paste("Proportion within 0.01 pH units:", round(within_tolerance, 3)))
})

test_that("Rust backend provides performance improvement", {
  water <- define_water(ph = 7, temp = 25, alk = 100, ca = 50, mg = 10, na = 20, k = 5, cl = 30, so4 = 40)

  # Warm up both backends
  solve_ph(water, backend = "rust")
  solve_ph(water, backend = "r")

  # Time a reasonable number of calls
  n_calls <- 100

  rust_time <- system.time({
    for(i in 1:n_calls) {
      solve_ph(water, backend = "rust")
    }
  })

  r_time <- system.time({
    for(i in 1:n_calls) {
      solve_ph(water, backend = "r")
    }
  })

  # Rust should be faster (allow some margin for test variability)
  speedup <- r_time[["elapsed"]] / rust_time[["elapsed"]]
  # Must be at least 10x faster for test to pass
  expect_gt(speedup, 10,
           label = paste("Speedup factor:", round(speedup, 2), "x"))
})
