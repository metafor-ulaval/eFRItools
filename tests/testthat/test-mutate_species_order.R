test_that("species order correctly extracted", {
  fri_polygons_species_order <- mutate_species_order(fri_polygons["SPCOMP"], "SPCOMP")

  expect_equal(fri_polygons_species_order[1,]$SP_NO_1, "BW")
  expect_equal(fri_polygons_species_order[1,]$SP_NO_2, "SB")
  expect_equal(fri_polygons_species_order[1,]$SP_NO_3, "PT")
})
