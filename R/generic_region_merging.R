#' Segmentation of a raster into polygons
#'
#' `generic_region_merging` calls the GenericRegionMerging software from Orfeo ToolBox (OTB).
#' OTB must be installed on your computer first. See \url{https://www.orfeo-toolbox.org/CookBook-7.0/index.html}
#'
#' @param input SpatRaster. Multiband raster to segment
#' @param thresh,spec,spat numeric. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param method string. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param clean_nodata bolean. Perform a 3 pixels majority filter to remove nodata of small holes
#' @param otb_dir string. Directory location of OTB (where you installed OTB earlier).
#' Likely "C:/OTB/bin" on Windows
#' @param ofile string. The path where to save the output of GRM
#' @return the raster produced by OTB
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' }
generic_region_merging = function(input,
                                  ofile = tempfile(fileext = ".tif"),
                                  thresh = 50,
                                  spec = 0.5,
                                  spat = 0.5,
                                  method = "bs",
                                  clean_nodata = TRUE,
                                  otb_dir = "C:/OTB/bin")
{
  pntr <- tryCatch({ input@pntr }, error = function(e) { input@cpp })

  ifile <- pntr$filenames()
  ifile <- unique(ifile)
  if (length(ifile) != 1 || !file.exists(ifile))
  {
    ifile = tempfile(fileext = ".tif")
    terra::writeRaster(input, ifile)
  }

  ifile <- normalizePath(ifile, mustWork = FALSE, winslash = "/")
  ofile <- normalizePath(ofile, mustWork = FALSE, winslash = "/")
  otb_dir <- normalizePath(otb_dir, mustWork = FALSE, winslash = "/")

  cmd <- paste0(otb_dir, "/otbcli_GenericRegionMerging -in ", ifile, " -out ", ofile, " -criterion ", method, " -threshold ", thresh, " -cw ", spec, " -sw ", spat)
  cat(cmd, "\n")
  system(cmd)

  cat("Masking the result")
  o <- terra::rast(ofile)
  grm <- terra::mask(o, input[[1]])

  terra::writeRaster(grm, ofile, overwrite = TRUE)
  grm <- terra::rast(ofile)

  # Clean nodata holes
  if(clean_nodata == TRUE){
    grm_cleaned <- terra::focal(grm, w = 3, fun = "modal", na.policy = "only", na.rm = T)
    grm_reduced <- terra::focal(grm_cleaned, w = 3, fun = "modal", na.policy = "omit", na.rm = F)
    grm_reduced[!is.na(grm)] <- grm
    grm <- grm_reduced
  }

  # Convert into polygons
  polygons <- terra::as.polygons(grm)
  polygons$grm <- NULL

  # Clean one pixel polygons
  areas <- terra::expanse(polygons)
  small <- areas < prod(terra::res(input))*1.5
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
