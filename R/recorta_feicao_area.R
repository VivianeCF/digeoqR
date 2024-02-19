#' Recorta a feição pela área
#'
#' Ferramenta que extrai a shape da feição (geologia, solo...) a partir de uma
#' feição de escala menor. Usei o mapa geológico do Brasil ao milhonésimo do SGB.
#'
#' @param dir_in Diretório de entrada das feições
#' @param dir_out Diretório da feição de saída
#' @param area Nome do arquivo shape da área
#' @param feicao_in Nome da shape da feição de entrada
#' @param feicao_out Nome da shape da feição de saída
#' @param chave Nome da chave para a dissolução da feição recortada
#'
#' @return
#' @export
#'
#' @examples
recorta_feicao_area <- function(dir_in = "inputs/campo/",
                                dir_out = "outputs/",
                                area = "area_srtm",
                                feicao_in = "geologia_br",
                                feicao_out = "geologia_area",
                                chave = "SIGLA"){

    suppressMessages({sf::sf_use_s2(FALSE)})

    area_sf <- sf::read_sf(paste0(dir_in, area, ".shp"))
    feicao_sf <- sf::read_sf(paste0(dir_in, feicao_in, ".shp"))

    area_sf <- sf::st_transform(area_sf, crs = 4326)
    feicao_sf <- sf::st_transform(feicao_sf, crs = 4326)


    crop_feicao <-
      suppressMessages({suppressWarnings({sf::st_crop(feicao_sf, area_sf)})})

    nc_dissolve <-
      suppressMessages({crop_feicao %>% dplyr::group_by(get(chave)) %>%
      dplyr::summarize()})

    # plot(nc_dissolve)

    feicao_edit <-
      suppressWarnings({suppressMessages({sf::st_join(crop_feicao, nc_dissolve,
                                                      by = chave)})})
    feicao_edit <- feicao_edit[,-ncol(feicao_edit)]
    sf::write_sf(feicao_edit, paste0(dir_out, feicao_out, ".shp"),
                  delete_layer = TRUE)

    return( feicao_edit)
 }