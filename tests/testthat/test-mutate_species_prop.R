test_that("species prop correctly extracted", {
  fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")

  expect_equal(fri_polygons_species_prop[1,]$BW, 40)
  expect_equal(fri_polygons_species_prop[1,]$SB, 40)
  expect_equal(fri_polygons_species_prop[1,]$PT, 20)
})

test_that("existing species prop columns are replaced", {
  x <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
  x$SPCOMP[1] <- "PJ 100"
  x <- mutate_species_prop(x, "SPCOMP")

  expect_equal(x[1,]$PJ, 100)
  expect_equal(x[1,]$BW, 0)
  expect_false(any(duplicated(names(x))))
})
