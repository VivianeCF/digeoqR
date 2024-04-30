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

    df_geo <- as.data.frame(crop_feicao)
    df_geo <- df_geo[, -ncol(df_geo)]

    nc_dissolve <-  sf::st_union(crop_feicao, by_feature = TRUE)

    nc_dissolve <- nc_dissolve %>%
     dplyr::group_by(get(chave)) %>%
     dplyr::summarise()

    colnames(nc_dissolve)[1] <- chave

    sf_geo <- dplyr::left_join(nc_dissolve,df_geo, by= chave)
    # ggplot2::ggplot(nc_dissolve) + ggplot2::geom_sf(fill = "grey")

    sf::write_sf(sf_geo, paste0(dir_out, feicao_out,"_", "area", ".shp"),
                  delete_layer = TRUE)

    return(sf_geo)
 }
