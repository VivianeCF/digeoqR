#'Extrai a drenagem do SRTM
#'
#'A partir da imagem SRTM, rios, massa d'água é gerado um modelo de drenagem.
#'    A densidade de drenagem é dada pelo valor de threshold, quanto maior
#'este valor, menor a densidade dos cursos dos rios.
#'
#' @param threshold Numero de células mínimo para o traçado das drenagens
#' @param fase Fase do levantamento geoquímico (1= Planejamento, 2 = Pós campo)
#' @param bacia_minima Área mínima das bacias planejadas
#' @param bacia_maxima Área máxima das bacias planejadas
#' @param snap_dist Deslocamento máximo do ponto até a drenagem
#' @param min_length Comprimento mínimo da drenagem
#' @param max_ordem Máxima ordem do rio para a busca do snap point
#' @param EPSG crs da camada
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo
#' @param dir_out Diretório de saída
#' @param dem Modelo digital de terreno (ex. SRTM)
#' @param bases_model Bases para a modelagem das bacias e drenagem pela
#' função gera_bases_model
#' @param ex_campo Dados de campo extraídos pela função extrai_dados_campo
#' @param gera_est Objeto da saída da função gera_estacoes
#' @param fonte_shp Valor lógico se oas esstações forem shapefile
#' @param arquivo_shp Caso for shape fornecer o caminho do arquivo
#' @param wbt_wd diretório onde estão salvos os dados da modelagem do DEM
#'
#' @return Rasters da preparação das imagens para a extração do modelo de
#' drenagem. Shapes e rasters das drenagens e trechos drenagems com as ordens
#' de strahler.
#'
#' @export
#'
#' @examples
#' # modelo_bacias()
modela_bacias <- function(fase = 2,
                          EPSG = 4326, dem, bases_model, gera_est,
                          ex_campo, classe_am,
                          fonte_shp = FALSE, arquivo_shp,
                          bacia_minima = 4,
                          bacia_maxima = 100,
                          threshold = 250,
                          snap_dist = "0.02",
                          min_length = "0.02",
                          max_ordem = 3, dir_out = "outputs/", wbt_wd = "outputs/modelo/")
{
  ### GERA DRENAGENS--------------------------------------------------------------
  nm_classe <- c("Concentrado de bateia",
                 "Sedimento de corrente",
                 "Solo",
                 "Rocha",
                 "Água")
  abrev_classe <- c("cb", "sc", "solo", "rocha", "agua" )

  out <- list()
  # wbt_wd <- tempfile()
  # dir.create(wbt_wd)
  options("rgdal_show_exportToProj4_warnings" = "none")

  ## Lê limite da area do projeto
  area <- bases_model[["limite da área folha"]]

  ## Lê massa d'água
  massa_dagua <- bases_model[["massa de água"]]

  ## Lê área urbana
  area_urbana <- bases_model[["área urbana"]]

  ## Lê rios da área do projeto
  rios_ibge = bases_model[["rios"]]

    ## Lê rios e grava no temp
  sf::write_sf(bases_model[["rios"]], paste0(wbt_wd,"rios.shp"))



#  if(fase == 2){
#    ## Lê DEM e grava no temp
#    stars::write_stars(dem,
#                       file.path( wbt_wd, "srtm.tif"))
#
#    options("rgdal_show_exportToProj4_warnings" = "none")
#    whitebox::wbt_rasterize_streams(
#     "rios.shp",
#     base = "srtm.tif",
#     output = "network_topage.tif",
#     nodata = 0,
#     wd = wbt_wd
#   )
#
#   # Burn this river network on the DEM
#   # We will neglect the effect of the road embankments at this DEM resolution of 100m
#   # by creating an empty shapefile for roads
#   sf::write_sf(
#     sf::st_sfc(sf::st_multilinestring(), crs = EPSG),
#     file.path(wbt_wd, "roads.shp"),
#     delete_layer = TRUE,
#     quiet = TRUE
#   )
#
#   whitebox::wbt_burn_streams_at_roads(
#     dem = "srtm.tif",
#     streams = "rios.shp",
#     roads = "roads.shp",
#     output = "dem_100m_burn.tif",
#     wd = wbt_wd
#   )
#   # Remove the depressions on the DEM
#   whitebox::wbt_fill_depressions(dem = "dem_100m_burn.tif",
#                                  output = "dem_fill.tif",
#                                  wd = wbt_wd)
#
#   # Flow direction raster
#   whitebox::wbt_d8_pointer(dem = "dem_fill.tif",
#                            output = "d8.tif",
#                            wd = wbt_wd)
#
#   # Compute flow accumulation
#   whitebox::wbt_d8_flow_accumulation(
#     input = "d8.tif",
#     pntr = TRUE,
#     output = "facc.tif",
#     wd = wbt_wd
#   )
#
#   # Extract a stream network (threshold = 1 km2) consistent with flow direction
#   whitebox::wbt_extract_streams(
#     flow_accum =  "facc.tif",
#     threshold = threshold,
#     # 100 cells for 1 km2
#     output = "network_1km2.tif",
#     zero_background = TRUE,
#     wd = wbt_wd
#   )
#
#   whitebox::wbt_remove_short_streams(
#     d8_pntr = "d8.tif",
#     streams = "network_1km2.tif",
#     output = "network_d8.tif",
#     min_length = min_length,
#     wd = wbt_wd
#   )
#
#   output_d8_pntr <- "d8.tif"
#   #
#   whitebox::wbt_raster_streams_to_vector("network_d8.tif",
#                                          "d8.tif",
#                                          output = "network_d8.shp",
#                                          wd = wbt_wd)
#   stream_model <- sf::read_sf(file.path(wbt_wd, "network_d8.shp"))
#   # class(crs_wgs84)
#
#   # cat(crs_wgs84$wkt)
#   sf::st_crs(stream_model) <- EPSG
#
#   # Gera drenagem classificada strahler
#   output_order <- "strahler_order.tif"
#   whitebox::wbt_strahler_stream_order("d8.tif",
#                                       "network_d8.tif",
#                                       output_order,
#                                       wd = wbt_wd)
#   output_drenagem_ord <- "stream_strahler.shp"
#   whitebox::wbt_raster_streams_to_vector(output_order,
#                                          "d8.tif",
#                                          output_drenagem_ord,
#                                          wd = wbt_wd)
#   stream_strahler <-
#     sf::read_sf(file.path(wbt_wd, "stream_strahler.shp"))
#   sf::st_crs(stream_strahler) <- EPSG
#
#   sf::write_sf(stream_strahler,
#                "outputs/stream_strahler.shp",
#                delete_layer = TRUE)
#   out[[1]] <- stream_strahler
#
#   sf::write_sf(stream_model, "outputs/stream_model.shp",
#                delete_layer = TRUE)
#   out[[2]] <- stream_model
# }
  ### GERA BACIAS A PARTIR DOS PONTOS ------------------------------
 ## Lê estaçoes da área do projeto
  if (fase == 1) {
    wbt_wd <- gera_est[[5]]
    sf::write_sf(gera_est[["estacoes geradas"]],
                 paste0(wbt_wd,  "estacoes.shp"))
  } else{
    if (fonte_shp == FALSE) {
      pontos <- ex_campo[["estações"]]
    } else {
      pontos <-  sf::read_sf(arquivo_shp)
      sf::st_transform(pontos, crs = EPSG)
    }
    # pontos <- pontos[pontos$CLASSE == nm_classe[classe_am],]
    sf::write_sf(pontos, paste0(wbt_wd,  "estacoes.shp"))
  }

  # Desloca pontos para a drenagem
  ## Snap metodo Jenson
  stream <- "network_d8.tif"
  output_snap <- "snappoints.shp"

  r = stars::read_stars(paste0(wbt_wd, "strahler_order.tif"))
  r <- r >= max_ordem
  stars::write_stars(r, paste0(wbt_wd, "strahler_order_1_4.tif"))

  whitebox::wbt_jenson_snap_pour_points("estacoes.shp",
                                        "strahler_order_1_4.tif",
                                        output_snap,
                                        snap_dist,
                                        wd = wbt_wd)


  # Cria bacias a partir dos pontos deslocados

  output_ws <-  "bacias.tif"
  whitebox::wbt_watershed("d8.tif",
                          output_snap,
                          output_ws,
                          wd = wbt_wd)

  # Converte raster de bacias em shape
  output_bacias <-  "bacias.shp"
  whitebox::wbt_raster_to_vector_polygons(output_ws,
                                          output_bacias,
                                          wd = wbt_wd)

  # Cria bacias não aninhadas
  output_unest <- "unested_bacias.tif"
  whitebox::wbt_unnest_basins("d8.tif",
                              output_snap,
                              output_unest,
                              wd = wbt_wd)


  # Converte raster de  bacias não anihadas em shape
  l_output_ws <- NA
  l_output_bacias <- NA
  list_files <-
    paste0(wbt_wd, list.files(wbt_wd, pattern = ".*unested_bacias"))

  for (i in seq(list_files)) {
    l_output_ws[i] <-  paste0(wbt_wd, "unested_bacias_", i, ".shp")
  }
  l_output_ws <- rev(l_output_ws)


  for (i in seq(list_files)) {
    l_output_bacias[i] <-
      paste0(wbt_wd, "unested_bacias_", i, ".tif")
  }


  for (i in seq(1, length(l_output_ws))) {
    whitebox::wbt_raster_to_vector_polygons(l_output_bacias[i],
                                            l_output_ws[i], wd = wbt_wd)

  }
  # Cria uma lista dos nomes dos arquivos
  #  das bacias não aninhadas
  shapefile_list <- lapply(l_output_ws, sf::read_sf)

  # Mescla as bacias não aninhadas
  bacias_area <- do.call(rbind, shapefile_list)

  # Remove arquivos unested

  to_be_deleted <- list.files(wbt_wd, pattern = "unested")
  file.remove(paste0(wbt_wd, to_be_deleted))

  # Salva o arquivo mesclado das bacias

  # wbt_wd <- paste0("outputs/")

  # Calcula a área das bacias não aninhadas



  # # Salva bacias com os cálculos da área
  sf::sf_use_s2(FALSE)
  # Calcula área
  sf::st_crs(bacias_area) <- EPSG
  bacias_area$Area_sqm <-
    as.numeric(round(sf::st_area(bacias_area) / 1000000, 0))

  sf::write_sf(bacias_area,
               paste0(dir_out,
                      "bacias_area", "_", abrev_classe[classe_am],".shp"),
               delete_layer = TRUE)



  if (fase == 1) {
    # Filtra bacias pelas áreas min e max
    bacias_area <-
      bacias_area[bacias_area$Area_sqm <= bacia_maxima &
                    bacias_area$Area_sqm >= bacia_minima, ]

    # Lê pontos  planejados
    pontos = sf::read_sf(paste0(wbt_wd, "snappoints.shp"))

    # # Arruma a sequência do campo VALUE
    pontos$VALUE <- seq(1:nrow(pontos))
    # Filtra pelos pontos com bacias
    pontos <- pontos %>% dplyr::arrange(VALUE)
    bacias_area <- bacias_area %>% dplyr::arrange(VALUE)
    pontos <- pontos[pontos$VALUE %in% bacias_area$VALUE, ]

    # Arruma a sequência do campo VALUE
    pontos$VALUE <- seq(1:nrow(pontos))
    bacias_area$VALUE <- seq(1:nrow(bacias_area))
    sf::st_crs(bacias_area) <- EPSG
    sf::write_sf(bacias_area,
                 paste0(dir_out, "bacias_area_plan.shp"),
                 delete_layer = TRUE)
    out[[3]] <- bacias_area

    sf::write_sf(pontos[, c("VALUE")],
                 paste0(dir_out,  "estacoes_plan.shp"),
                 delete_layer =
                   TRUE)
    out[[4]] <- pontos[, c("VALUE")]

    sf::write_sf(pontos[, c("VALUE")],
                 paste0(wbt_wd, "estacoes_plan.shp"),
                 delete_layer =
                   TRUE)
    # Cria bacias a partir dos pontos deslocados e filtrados
    output_ws <-  "bacias.tif"

    whitebox::wbt_watershed(
      d8_pntr = "d8.tif",
      pour_pts = "estacoes_plan.shp",
      output =  output_ws,
      wd = wbt_wd
    )

    # Converte raster de bacias em shape
    output_bacias <- "bacias.shp"
    whitebox::wbt_raster_to_vector_polygons(output_ws,
                                            output_bacias, wd = wbt_wd)
    bacias <- sf::read_sf(paste0(wbt_wd, output_bacias))
    sf::write_sf(bacias, paste0(dir_out, "bacias_plan.shp"), delete_layer = TRUE)
    out[[5]] <- bacias
  } else{
    out[[3]] <- bacias_area
    output_bacias <- "bacias"
    bacias <- sf::read_sf(paste0(wbt_wd, output_bacias, ".shp"))
    sf::st_crs(bacias) <- EPSG
    sf::write_sf(bacias, paste0(dir_out, output_bacias, "_", abrev_classe[classe_am], ".shp"), delete_layer = TRUE)
    out[[4]] <- bacias
  }

  # Mapa
  m <- ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggspatial::layer_spatial(
      bacias,
      fill = "yellow",
      lwd = 0.7,
      color = "green"
    ) +
    ggspatial::layer_spatial(rios_ibge, color = "blue") +
    ggspatial::layer_spatial(area, fill = NA, lwd = 0.5) +
    ggspatial::layer_spatial(pontos, color = "red") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggspatial::annotation_scale(location = "bl", style = "ticks") +
    ggspatial::annotation_north_arrow(location = "br") +
    ggplot2::labs(caption = paste0(
      "* Pontos vermelhos = pontos de coleta planejados.",
      " N= ",
      nrow(pontos)
    ))
  if (fase == 1) {
    nome_mapa <- "mapa_amostragem_planejada.png"
  } else{
    nome_mapa <- paste0("mapa_amostragem_executada","_", abrev_classe[classe_am],".png")
  }
  png(
    paste0(dir_out, nome_mapa),
    units = "cm",
    width = 16,
    height = 16,
    res = 300
  )
  print(m)
  dev.off()
if(fase == 1){
  out[[6]] <- m
  names(out) <- c("stream strahler", "stream model",
                  "bacias area plan", "estacoes plan", "bacias plan", "mapa plan")
  }else{
    out[[5]] <- m
    names(out) <- c("stream strahler", "stream model", "bacias area", "bacias", "mapa amostragem")
    }

return(out)
}
