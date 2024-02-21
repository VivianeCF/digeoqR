#'Extrai a drenagem do SRTM
#'
#'A partir da imagem SRTM, rios, massa d'água é gerado um modelo de drenagem.
#'    A densidade de drenagem é dada pelo valor de threshold, quanto maior
#'este valor, menor a densidade dos cursos dos rios.
#'
#' @param rios Shape dos rios obtidos por cartas digitais, fotografia aérea ou
#' dados do IBGE
#' @param massa_dagua Shape da massa d'água, obtidos por cartas digitais,
#' fotografia aérea ou dados do IBGE
#' @param threshold Numero de células mínimo para o traçado das drenagens
#' @param fase Fase do levantamento geoquímico (1= Planejamento, 2 = Pós campo)
#' @param bacia_minima Área mínima das bacias planejadas
#' @param bacia_maxima Área máxima das bacias planejadas
#' @param limite Polígonono da área do projeto
#' @param snap_dist Deslocamento máximo do ponto até a drenagem
#' @param min_length Comprimento mínimo da drenagem
#' @param max_ordem Máxima ordem do rio para a busca do snap point
#' @param EPSG crs da camada
#' @param limite_srtm
#' @param dir_bol
#' @param classe_am
#' @param analise
#' @param dir_base
#' @param tipo_base
#' @param base_campo
#' @param dir_out
#' @param ref_ucc
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
                          EPSG = 4326,
                          bacia_minima = 4,
                          bacia_maxima = 100,
                          rios = "inputs/campo/rios_ibge.shp",
                          massa_dagua = "inputs/campo/massa_dagua.shp",
                          limite = "inputs/campo/carta_100M.shp",
                          limite_srtm = "area_srtm.shp",
                          threshold = 250,
                          snap_dist = 0.02,
                          min_length = 0.02,
                          max_ordem = 3, dir_bol = NULL, classe_am = NULL, analise = NULL, dir_base = NULL,
                          tipo_base = NULL, base_campo = NULL, dir_out = "outputs/", ref_ucc = NULL)
{
  ### GERA DRENAGENS--------------------------------------------------------------
  nm_classe <- c("Concentrado de bateia",
                 "Sedimento de corrente",
                 "Solo",
                 "Rocha",
                 "Água")
  abrev_classe <- c("cb", "sc", "solo", "rocha", "agua" )

  out <- list()
  wbt_wd <- tempfile()
  dir.create(wbt_wd)
  options("rgdal_show_exportToProj4_warnings" = "none")

  bases <- gera_bases_model()

  ## Lê limite da area do projeto
  area <- bases[["limite da área folha"]]

  ## Lê massa d'água
  massa_dagua <- bases[["massa de água"]]

  ## Lê área urbana
  area_urbana <- bases[["área urbana"]]

  ## Lê rios da área do projeto
  rios_ibge = bases[["rios"]]

    ## Lê rios e grava no temp
  sf::write_sf(bases[["rios"]], paste0(wbt_wd,"\\","rios.shp"))



 if(fase == 2){
   ## Lê DEM e grava no temp
   dem <- prepara_dem()
   stars::write_stars(dem,
                      file.path( wbt_wd, "srtm.tif"))

   options("rgdal_show_exportToProj4_warnings" = "none")
   whitebox::wbt_rasterize_streams(
    "rios.shp",
    base = "srtm.tif",
    output = "network_topage.tif",
    nodata = 0,
    wd = wbt_wd
  )

  # Burn this river network on the DEM
  # We will neglect the effect of the road embankments at this DEM resolution of 100m
  # by creating an empty shapefile for roads
  sf::write_sf(
    sf::st_sfc(sf::st_multilinestring(), crs = 4326),
    file.path(wbt_wd, "roads.shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )

  whitebox::wbt_burn_streams_at_roads(
    dem = "srtm.tif",
    streams = "rios.shp",
    roads = "roads.shp",
    output = "dem_100m_burn.tif",
    wd = wbt_wd
  )
  # Remove the depressions on the DEM
  whitebox::wbt_fill_depressions(dem = "dem_100m_burn.tif",
                                 output = "dem_fill.tif",
                                 wd = wbt_wd)

  # Flow direction raster
  whitebox::wbt_d8_pointer(dem = "dem_fill.tif",
                           output = "d8.tif",
                           wd = wbt_wd)

  # Compute flow accumulation
  whitebox::wbt_d8_flow_accumulation(
    input = "d8.tif",
    pntr = TRUE,
    output = "facc.tif",
    wd = wbt_wd
  )

  # Extract a stream network (threshold = 1 km2) consistent with flow direction
  whitebox::wbt_extract_streams(
    flow_accum =  "facc.tif",
    threshold = threshold,
    # 100 cells for 1 km2
    output = "network_1km2.tif",
    zero_background = TRUE,
    wd = wbt_wd
  )

  whitebox::wbt_remove_short_streams(
    d8_pntr = "d8.tif",
    streams = "network_1km2.tif",
    output = "network_d8.tif",
    min_length = min_length,
    wd = wbt_wd
  )

  output_d8_pntr <- "d8.tif"
  #
  whitebox::wbt_raster_streams_to_vector("network_d8.tif",
                                         "d8.tif",
                                         output = "network_d8.shp",
                                         wd = wbt_wd)
  stream_model <- sf::read_sf(file.path(wbt_wd, "network_d8.shp"))
  # class(crs_wgs84)

  # cat(crs_wgs84$wkt)
  sf::st_crs(stream_model) <- EPSG

  # Gera drenagem classificada strahler
  output_order <- "strahler_order.tif"
  whitebox::wbt_strahler_stream_order("d8.tif",
                                      "network_d8.tif",
                                      output_order,
                                      wd = wbt_wd)
  output_drenagem_ord <- "stream_strahler.shp"
  whitebox::wbt_raster_streams_to_vector(output_order,
                                         "d8.tif",
                                         output_drenagem_ord,
                                         wd = wbt_wd)
  stream_strahler <-
    sf::read_sf(file.path(wbt_wd, "stream_strahler.shp"))
  sf::st_crs(stream_strahler) <- EPSG

  sf::write_sf(stream_strahler,
               "outputs/stream_strahler.shp",
               delete_layer = TRUE)
  out[[1]] <- stream_strahler

  sf::write_sf(stream_model, "outputs/stream_model.shp",
               delete_layer = TRUE)
  out[[2]] <- stream_model
}
  ### GERA BACIAS A PARTIR DOS PONTOS ------------------------------



  ## Lê estaçoes da área do projeto
 if(fase == 1){
   ex_campo = gera_estacoes(dir_shp,
                            dir_out ,
                            limite,
                            limite_srtm,
                            area_urbana,
                            EPSG,
                            rios,
                            massa_dagua,
                            threshold,
                            min_length,
                            max_ordem)
   wbt_wd <- ex_campo[[5]]
   sf::write_sf(ex_campo[["estacoes geradas"]],
                paste0(wbt_wd, "/", "estacoes.shp"))

 } else{
  ex_campo <- extrai_dados_campo(tipo_base,
                                 dir_base,
                                 base_campo,
                                 dir_os,
                                 EPSG,
                                 dir_out)
  pontos <- ex_campo[["estações"]]
  pontos <- pontos[pontos$CLASSE == nm_classe[classe_am], ]
  sf::write_sf(pontos, paste0(wbt_wd, "/", "estacoes.shp"))

}
  # Desloca pontos para a drenagem
  ## Snap metodo Jenson
  stream <- "network_d8.tif"
  output_snap <- "snappoints.shp"

  r = stars::read_stars(paste0(wbt_wd, "\\", "strahler_order.tif"))
  r[r > max_ordem] = NA
  stars::write_stars(r, paste0(wbt_wd, "\\", "strahler_order_1_4.tif"))

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
  path = paste0(wbt_wd, "\\")

  # Converte raster de  bacias não anihadas em shape
  l_output_ws <- NA
  l_output_bacias <- NA
  list_files <-
    paste0(path, list.files(path, pattern = ".*unested_bacias"))

  for (i in seq(list_files)) {
    l_output_ws[i] <-  paste0(path, "unested_bacias_", i, ".shp")
  }
  l_output_ws <- rev(l_output_ws)


  for (i in seq(list_files)) {
    l_output_bacias[i] <-
      paste0(path, "unested_bacias_", i, ".tif")
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
  to_be_deleted <- list.files(path, pattern = "unested")
  file.remove(paste0(path, to_be_deleted))

  # Salva o arquivo mesclado das bacias

  # path <- paste0("outputs/")

  # Calcula a área das bacias não aninhadas



  # # Salva bacias com os cálculos da área
  sf::sf_use_s2(FALSE)
  bacias_area$Area_sqm <-
    as.numeric(round(sf::st_area(bacias_area) / 1000000, 0))
  # Calcula área
  sf::write_sf(bacias_area,
               paste0("outputs/",
                      "bacias_area", "_", abrev_classe[classe_am],".shp"),
               delete_layer = TRUE)



  if (fase == 1) {
    # Filtra bacias pelas áreas min e max
    bacias_area <-
      bacias_area[bacias_area$Area_sqm <= bacia_maxima &
                    bacias_area$Area_sqm >= bacia_minima, ]

    # Lê pontos  planejados
    pontos = sf::read_sf(paste0(wbt_wd, "\\", "snappoints.shp"))

    # # Arruma a sequência do campo VALUE
    pontos$VALUE <- seq(1:nrow(pontos))
    # Filtra pelos pontos com bacias
    pontos <- pontos %>% dplyr::arrange(VALUE)
    bacias_area <- bacias_area %>% dplyr::arrange(VALUE)
    pontos <- pontos[pontos$VALUE %in% bacias_area$VALUE, ]

    # Arruma a sequência do campo VALUE
    pontos$VALUE <- seq(1:nrow(pontos))
    bacias_area$VALUE <- seq(1:nrow(bacias_area))
    sf::write_sf(bacias_area,
                 paste0("outputs/", "bacias_area_plan.shp"),
                 delete_layer = TRUE)
    out[[3]] <- bacias_area

    sf::write_sf(pontos[, c("VALUE")],
                 paste0("outputs/",  "estacoes_plan.shp"),
                 delete_layer =
                   TRUE)
    out[[4]] <- pontos[, c("VALUE")]

    sf::write_sf(pontos[, c("VALUE")],
                 paste0(wbt_wd, "\\", "estacoes_plan.shp"),
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
    bacias <- sf::read_sf(paste0(wbt_wd, "//", output_bacias))
    sf::write_sf(bacias, paste0("outputs/bacias_plan.shp"), delete_layer = TRUE)
    out[[5]] <- bacias
  } else{
    out[[3]] <- bacias_area
    output_bacias <- "bacias"
    bacias <- sf::read_sf(paste0(wbt_wd, "//", output_bacias, ".shp"))
    sf::write_sf(bacias, paste0("outputs/", output_bacias, "_", abrev_classe[classe_am], ".shp"), delete_layer = TRUE)
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
    paste0("outputs/", nome_mapa),
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
