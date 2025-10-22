test_that("species prop correctly extracted", {
  fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")

  expect_equal(fri_polygons_species_prop[1,]$BW, 40)
  expect_equal(fri_polygons_species_prop[1,]$SB, 40)
  expect_equal(fri_polygons_species_prop[1,]$PT, 20)
})
