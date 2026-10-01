test_that("species order correctly extracted", {
  fri_polygons_species_order <- mutate_species_order(fri_polygons["SPCOMP"], "SPCOMP")

  expect_equal(fri_polygons_species_order[1,]$SP_NO_1, "BW")
  expect_equal(fri_polygons_species_order[1,]$SP_NO_2, "SB")
  expect_equal(fri_polygons_species_order[1,]$SP_NO_3, "PT")
})

test_that("existing species order columns are replaced", {
  x <- mutate_species_order(fri_polygons["SPCOMP"], "SPCOMP")
  x$SPCOMP[1] <- "PJ 100"
  x <- mutate_species_order(x, "SPCOMP")

  expect_equal(x[1,]$SP_NO_1, "PJ")
  expect_true(is.na(x[1,]$SP_NO_2))
  expect_false(any(duplicated(names(x))))
})
