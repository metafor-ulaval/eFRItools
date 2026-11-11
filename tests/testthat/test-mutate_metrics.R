test_that("metrics successfully extracted", {
  fri_polygons |>
    mutate_metrics(terra::rast(metrics), fun = "median") |>
    dplyr::pull(slope_median) |>
    mean() |>
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.753967)
})

test_that("differents crs", {
  fri_polygons |>
    sf::st_transform(4326) |>
    mutate_metrics(terra::rast(metrics), fun = "median") |>
    dplyr::pull(slope_median) |>
    mean() |>
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.753967)
})

test_that("other summary functions are still available", {
  fri_polygons |>
    mutate_metrics(terra::rast(metrics)[[c("slope", "dens")]], fun = "mean") -> x

  expect_true(all(c("slope_mean", "dens_mean") %in% names(x)))
  expect_equal(x$slope_mean,
               exactextractr::exact_extract(terra::rast(metrics)[["slope"]], sf::st_transform(fri_polygons, sf::st_crs(terra::rast(metrics))), "mean", progress = FALSE))
})

test_that("median of several layers keeps the layer order", {
  y <- terra::rast(metrics)[[c("slope", "dens")]]
  x_all <- mutate_metrics(fri_polygons, y, fun = "median")
  x_dens <- mutate_metrics(fri_polygons, y[["dens"]], fun = "median")

  expect_equal(x_all$dens_median, x_dens$dens_median)
})
