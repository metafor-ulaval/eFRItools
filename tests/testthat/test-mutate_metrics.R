test_that("metrics successfully extracted", {
  fri_polygons %>%
    sf::st_geometry() %>%
    sf::st_as_sf() %>%
    mutate_metrics(terra::rast(metrics)) %>%
    dplyr::pull(slope) %>%
    mean() %>%
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.743835)
})

test_that("differents crs", {
  fri_polygons %>%
    sf::st_geometry() %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326) %>%
    mutate_metrics(terra::rast(metrics)) %>%
    dplyr::pull(slope) %>%
    mean() %>%
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.743835)
})
