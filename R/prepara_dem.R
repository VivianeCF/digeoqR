#' Title
#'
#' @param limite Limite da área do srtm
#' @param EPSG Projeçao usada
#' @param z Nível de zoom para recuperar o SRTM
#' @param cellsize
#'
#' @return
#' @export
#'
#' @examples
prepara_dem <- function(limite = "inputs/campo/area_srtm.shp",
                        EPSG = 4326,
                        z = 11,
                        cellsize = 0.0002777777776748480143) {
  # Set up a projection (Brazil SIRGAS-2000 projection)
  area <- sf::st_read(limite)
  area <- sf::st_transform(area, crs = EPSG)
  # Define a bbox that will encompass the catchments of the study area
  area_bbox <- sf::st_bbox(area,
                           crs = st_crs(EPSG))
  area_loc <- sf::st_as_sfc(area_bbox) |> sf::st_sf()

  # Retrieve elevation data as raster
  dem_raw <-
    elevatr::get_elev_raster(area_loc,  z = z, clip = "bbox") # ~30m resolution

  # Project and define spatial resolution:
  # dem_100m  <- stars::st_warp(stars::st_as_stars(dem_raw), cellsize = cellsize,
  #                             crs = st_crs(EPSG))
  # names(dem_100m) <- "warp"
  dem_raw <- stars::st_as_stars(dem_raw)

  # Set negative values (ocean) to NA
  # dem_100m[dem_100m < 0] <- NA

  # Write to file
  stars::write_stars(dem_raw,
                     file.path("inputs/imagens/", "srtm.tif"))


}
