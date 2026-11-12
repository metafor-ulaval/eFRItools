test_that("forested proportion is correct", {
  x <- mutate_proportion(fri_polygons, terra::rast(landcover), prefix = "landcover", simplify = FALSE, keep_all = TRUE)
  fri_polygons_prop_forested <- mutate_prop_forested(x, prefix = "landcover", forested_class = c("Sparse_Treed",
                                                                                                 "Treed_Upland",
                                                                                                 "Deciduous_Treed",
                                                                                                 "Mixed_Treed",
                                                                                                 "Coniferous_Treed",
                                                                                                 "Plantations_Treed_Cultivated",
                                                                                                 "Hedge_Rows",
                                                                                                 "Tallgrass_Woodland"))

  expect_equal(fri_polygons_prop_forested$PROPFORESTED[1], 99.3153935)
})
