#' Segmentation of a raster into polygons
#'
#' `generic_region_merging` calls the GenericRegionMerging software from Orfeo ToolBox (OTB).
#' OTB must be installed on your computer first. See \url{https://www.orfeo-toolbox.org/CookBook-7.0/index.html}
#'
#' @param metrics SpatRaster; multiband raster to segment from the `terra` package.
#' @param thresh,spec,spat numeric; threshold, spectral weight and spatial weight of GRM. See
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param method character; homogeneity criterion of GRM. See
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param clean_nodata logical; perform a 3 pixels majority filter to remove small nodata holes.
#' @param otb_dir character; folder where OTB is installed. Likely "C:/OTB/bin" on Windows.
#' @param ofile character; path where the GRM raster is saved.
#' @returns
#' Polygons of the segmentation from the `sf` package
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' }
generic_region_merging = function(metrics,
                                  ofile = tempfile(fileext = ".tif"),
                                  thresh = 50,
                                  spec = 0.5,
                                  spat = 0.5,
                                  method = "bs",
                                  clean_nodata = TRUE,
                                  otb_dir)
{
  pntr <- tryCatch({ metrics@pntr }, error = function(e) { metrics@cpp })

  ifile <- pntr$filenames()
  ifile <- unique(ifile)
  if (length(ifile) != 1 || !file.exists(ifile))
  {
    ifile = tempfile(fileext = ".tif")
    terra::writeRaster(metrics, ifile)
  }

  ifile <- normalizePath(ifile, mustWork = FALSE, winslash = "/")
  ofile <- normalizePath(ofile, mustWork = FALSE, winslash = "/")
  otb_dir <- normalizePath(otb_dir, mustWork = FALSE, winslash = "/")

  # Paths are quoted to support spaces
  cmd <- paste0(shQuote(paste0(otb_dir, "/otbcli_GenericRegionMerging"), type = "cmd"), " -in ", shQuote(ifile, type = "cmd"), " -out ", shQuote(ofile, type = "cmd"), " -criterion ", method, " -threshold ", thresh, " -cw ", spec, " -sw ", spat)
  cat(cmd, "\n")
  system(cmd)

  cat("Masking the result\n")
  o <- terra::rast(ofile)
  grm <- terra::mask(o, metrics[[1]])

  terra::writeRaster(grm, ofile, overwrite = TRUE)
  grm <- terra::rast(ofile)

  # Clean nodata holes
  if(clean_nodata == TRUE){
    grm_cleaned <- terra::focal(grm, w = 3, fun = "modal", na.policy = "only", na.rm = T)
    grm_reduced <- terra::focal(grm_cleaned, w = 3, fun = "modal", na.policy = "omit", na.rm = F)
    grm_reduced[!is.na(grm)] <- grm
    grm <- grm_reduced

    terra::writeRaster(grm, gsub("\\.tif$", "_cleaned.tif", ofile), overwrite = TRUE)
  }

  # Convert into polygons
  polygons <- terra::as.polygons(grm)
  polygons$grm <- NULL

  # Clean one pixel polygons
  areas <- terra::expanse(polygons)
  small <- areas < prod(terra::res(metrics))*1.5
  if (any(small))
  {
    polygons <- terra::combineGeoms(polygons[!small], polygons[small])
    polygons$ID <- 1:length(polygons)
    polygons <- terra::aggregate(polygons, by = "ID")
    polygons$ID <- NULL
    polygons$agg_n <- NULL
  }

  polygons <- sf::st_as_sf(polygons)

  return(polygons)
}
