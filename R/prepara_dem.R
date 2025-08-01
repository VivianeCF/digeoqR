#' Prepara imagem de SRTM para a modelagem de terreno
#'
#' Recupera dados de elevação do terreno da Amazon Web Services Terrain Tile
#' didoníveis em https://registry.opendata.aws/terrain-tiles.
#' Para mais detalhes visite o repositório https://github.com/jhollist/elevatr
#'
#'
#' @param EPSG Projeçao usada
#' @param z Nível de zoom para recuperar o SRTM
#' @param dir_out Diretório de saída
#' @param dir_in
#' @param limite_srtm
#'
#' @return
#' @export
#'
#' @examples
prepara_dem <- function(dir_in = "inputs/campo/",
                        limite_srtm = "area_srtm.shp",
                        dir_out = "outputs/",
                        EPSG = 4326,
                        z = 11) {

  # Configura a projeção  do Brasil: SIRGAS-2000
  area <- sf::st_read(paste0(dir_in, limite_srtm))
  area <- sf::st_transform(area, crs = EPSG)

 # Define os vértices (bbox) que irá abranger as bacias da área de estudo
  area_bbox <- sf::st_bbox(area,
                           crs = st_crs(EPSG))
  area_loc <- sf::st_as_sfc(area_bbox) |> sf::st_sf()

  # Recupera os dados de elevação como raster
  dem_raw <-
    elevatr::get_elev_raster(area,  z = z, clip = "locations") # ~30m resolução

  # Projeta e define a resolução espacial
   dem_raw <- stars::st_as_stars(dem_raw)

  # Salva srtm no diretório escolhido
  stars::write_stars(dem_raw,
                     file.path(dir_out, "srtm.tif"))
  return(dem_raw)

}
