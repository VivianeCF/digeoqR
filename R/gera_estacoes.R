
#'Gera estações nas confluências dos rios
#'
#'Desenvolvido para automatizar o processo de escolha do locais de amostragem
#'que devem ser a 100 m acima dos pontos de confluência dos rios. Também são
#'excluídos locais nas áreas urbanas e de massa d'água. Todos os arquivos devem
#'ser no formato shapefile.
#'
#'@param rios nome do arquivo os rios
#'@param limite nome do arquivo da área do projeto
#'@param massa_dagua nome do arquivo da massa d'água
#'@param area_urbana nome do arquivo da área urbana
#' @param estacoes Estações geradas a partir da drenagem modelada
#' @param srtm DEM da área
#' @param EPSG Sistema de coordenadas
#' @param threshold Valor de corte da área das bacias modeladas
#' @param min_length Comprimento mínimo do curso do rio
#' @param max_ordem Ordem Strhaler máxima para a validaçãon da estação
#' @param dir_campo Diretório dos dados espaciais
#' @param dir_dem Diretório da imagem tif srtm
#' @param dir_out Diretório de saída
#'
#'@return Imagem (.png) do mapa de planejamento preliminar.
#'Arquivo da shape (shp) das estações.
#'@export
#'
#' @examples
#' #gera_estacoes()
#'
gera_estacoes <-
  function(dir_campo = "inputs/campo/", dir_dem = "inputs/imagens/",
           dir_out = "outputs/",
           limite = "carta_100M.shp",
           area_urbana = "area_urbana.shp",
           estacoes = "estacoes_geradas.shp",
           srtm = "srtm.tif",
           EPSG = 4326,
           rios = "rios_ibge.shp",
           massa_dagua = "massa_dagua.shp",
           threshold = 250,
           min_length = 0.02,
           max_ordem = 4)
  {
    ### GERA DRENAGENS--------------------------------------------------------------
    base = paste0(dir_dem, srtm)
    wbt_wd <- tempdir(check = TRUE)
    options("rgdal_show_exportToProj4_warnings" = "none")
    whitebox::wbt_rasterize_streams(
      paste0(dir_campo, rios),
      base = base,
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
    network_topage <- sf::read_sf(paste0(dir_campo, rios))
    sf::write_sf(
      network_topage,
      file.path(wbt_wd, "network_topage.shp"),
      delete_layer = TRUE,
      quiet = TRUE
    )
    whitebox::wbt_burn_streams_at_roads(
      dem = base,
      streams = "network_topage.shp",
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
    whitebox::wbt_strahler_stream_order(output_d8_pntr,
                                        "network_d8.tif",
                                        output_order,
                                        wd = wbt_wd)
    output_drenagem_ord <- "stream_strahler.shp"
    whitebox::wbt_raster_streams_to_vector(output_order,
                                           output_d8_pntr,
                                           output_drenagem_ord,
                                           wd = wbt_wd)
    stream_strahler <-
      sf::read_sf(file.path(wbt_wd, "stream_strahler.shp"))
    sf::st_crs(stream_strahler) <- EPSG
    sf::write_sf(stream_strahler,
                 "outputs/stream_strahler.shp",
                 delete_layer = TRUE)

    sf::write_sf(stream_model, "outputs/stream_model.shp", delete_layer = TRUE)

    # suppressWarnings(suppressMessages(library(sf)))
    ## Extrair vértices dos rios
    ## Lê shape dos rios
    ## Gera ESTAÇÕES ---------------------------------------------------------------
    ## Lê limite da area do projeto
    area <- sf::read_sf(paste0(dir_campo, limite))

    ## Lê massa d'água
    massa_dagua <- sf::read_sf(paste0(dir_campo,massa_dagua))

    ## Lê área urbana
    area_urbana <- sf::read_sf(paste0(dir_campo,area_urbana))

    out <- list()
    ## Encontrar vértices das junções

    # Simplify river shapefile
    shape_river_simple <- stream_model %>%
      sf::st_as_sf() %>%
      suppressMessages({
        sf::st_union()
      })

    # Convert shapefile to point object
    river_to_points <- suppressWarnings({
      shape_river_simple %>%
        sf::st_as_sf() %>%
        sf::st_cast("POINT") %>%
        dplyr::mutate(id = 1:nrow(.))
    })

    # Check points that overlap
    joins_selection <- river_to_points %>%
      sf::st_equals() %>%
      Filter(function(x) {
        length(x) > 2
      }, .) %>%
      lapply(., FUN = min) %>%
      unlist() %>%
      unique()

    # Filter original point shapefile to retain only confluences
    river_joins <- river_to_points %>%
      dplyr::filter(id %in% joins_selection)

    ## Criar buffer circular das junções (100 metros de raio)
    # Buffer around the points
    joins_area <-
      suppressMessages({
        suppressWarnings({
          sf::st_buffer(river_joins, dist = 0.01 / 10)
        })
      })

    ## Remover sobreposição buffer dos rios
    linhas <- stream_model  %>% sf::st_transform(4326)
    outlet <- joins_area  %>% sf::st_transform(4326)
    sf::sf_use_s2(FALSE)
    res_ls <-
      suppressWarnings({
        suppressMessages({
          sf::st_difference(linhas, sf::st_union(outlet))
        })
      })
    ep <- lwgeom::st_endpoint(res_ls)
    p <- as.data.frame(do.call("rbind", ep))
    pontos_originais <-
      sf::st_as_sf(p, coords = c("V1", "V2"), crs = 4326)


    ## Pontos selecionados pelo filtro de localização --------------------------
    ## Extrair pontos dentro da carta 100 mil
    area <- sf::st_transform(area, 4326)
    pontos_area <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_originais, area)
        })
      })
    pontos_area$id <- seq(1, nrow(pontos_area))

    ## Remover pontos dentro das áreas alagadas
    massa_dagua <- sf::st_transform(massa_dagua, 4326)
    pontos_area_remover1 <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_area, massa_dagua)
        })
      })
    ## Remover pontos dentro de áreas urbanas
    area_urbana <- sf::st_transform(area_urbana, 4326)
    pontos_area_remover2 <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_area, area_urbana)
        })
      })
    if ((nrow(pontos_area_remover2) + nrow(pontos_area_remover1)) > 0) {
      pontos_remover <- rbind(pontos_area_remover1, pontos_area_remover2)
      pontos_area <-
        pontos_area[!(pontos_area$id %in% pontos_remover$id),]
    }

    # Mapa
    m <- ggplot2::ggplot() +
      # ggplot2::coord_fixed() +
      ggspatial::layer_spatial(res_ls) +
      ggspatial::layer_spatial(area, fill = NA, lwd = 0.6) +
      ggspatial::layer_spatial(pontos_area, color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      ggspatial::annotation_scale(location = "bl", style = "ticks") +
      ggspatial::annotation_north_arrow(location = "br") +
      ggplot2::labs(caption = paste0(
        "* Pontos vermelhos = pontos de coleta planejados.",
        " N= ",
        nrow(pontos_area)
      ))

    png(
      paste0("outputs/", "mapa_plan_ini.png"),
      units = "cm",
      width = 16,
      height = 16,
      res = 300
    )
    print(m)
    dev.off()
    # ## Salvar dados espaciais gerados ----------------------------------------
    sf::write_sf(pontos_area,
                 paste0(dir_out, estacoes),
                 overwrite = TRUE)
  }
