test_that("functional group correctly extracted", {
  fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
  fri_polygons_species_order <- mutate_species_order(fri_polygons_species_prop, "SPCOMP")
  fri_polygons_forest_type <- mutate_forest_type(fri_polygons_species_order)
  fri_polygons_functional_group <- mutate_functional_group(fri_polygons_forest_type)

  expect_equal(fri_polygons_functional_group[2,]$FUNCTIONAL_GROUP_3, "Softwood")
  expect_equal(fri_polygons_functional_group[2,]$FUNCTIONAL_GROUP_5, "Jack Pine Dominated")
})
