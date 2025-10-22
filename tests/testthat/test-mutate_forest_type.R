test_that("forest type correctly extracted", {
  fri_polygons_species <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
  fri_polygons_forest_type <- mutate_forest_type(fri_polygons_species)

  expect_equal(fri_polygons_forest_type[1,]$PROP_CONIFEROUS, 40)
  expect_equal(fri_polygons_forest_type[1,]$PROP_DECIDUOUS, 60)
})
