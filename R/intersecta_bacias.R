#' Intersecta polígonos das bacias com outra feição
#'
#' Cada bacia é recortada pelos polígonos da geologia. Esta função está
#' configurada para a shape do mapa geológico ao milhonésimo 2014 do SGB-CPRM.
#'
#' @param limite Limite da área do projeto
#' @param bacias bacias obtidas a partir da função gera_bacias
#' @param feicao_rec polígonos de algum tema (geologia,
#' solo, geofísica, geomorfologia, etc)
#' @param estacoes Estações de coleta
#' @param dir_out Diretório de saída onde serão gravados as planilhas .csv
#' @param tipo_leg Tipo de legenda 1= pela SIGLA 2 = pelo RANGE
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo, 5 = água
#' @param dir_in
#' @param nome_xml
#'
#' @return
#' Bacias
#' @export
#'
#' @examples
#' #intersecta_bacias()
intersecta_bacias <- function(dir_in = "inputs/campo/",
                              dir_out = "outputs/", bases_model,
                              bacias,
                              feicao_rec,
                              tipo_leg = 2,
                              estacoes,
                              classe_am = 2,
                              nome_xml = "geologia")
{
  abrev <- c("cb", "sc", "solo", "rocha", "agua")
  nm_classe <- c("Concentrado de bateia",
                 "Sedimento de corrente",
                 "Solo",
                 "Rocha",
                 "Água")

  options(encoding = "latin1")
  out <- list()
  area <- bases_model[["limite da área folha"]]
  centroide <-
    sf::st_coordinates(suppressWarnings({
      sf::st_centroid(area)
    }))
  zone <- as.numeric(floor((centroide[, 1] + 180) / 6) + 1)
  ## import data
  spy_grid <- bacias

  # sf::st_crs(spy_grid) <-
  spy_grid <- sf::st_transform(spy_grid,
                               paste0(
                                 "+proj=utm +zone=",
                                 zone,
                                 " +south +datum=WGS84 +units=m +no_defs"
                               ))

  spy_grid$Area_bacia <-
    round(sf::st_area(spy_grid, byid = TRUE) / 1000000, 3)
  # spy_grid@data <- spy_grid@data[,1:8]
  spy_poly = geologia
  spy_poly <- sf::st_transform(spy_poly,
                               paste0(
                                 "+proj=utm +zone=",
                                 zone,
                                 " +south +datum=WGS84 +units=m +no_defs"
                               ))

if(tipo_leg == 2){
  spy_poly <- spy_poly %>%
    dplyr::group_by(RANGE) %>%
    dplyr::summarize()
}

  legenda <- prepara_legenda(feicao_rec, dir_out,
                             dir_in , nome_xml)
  codlito <- legenda[[tipo_leg]]

  mydata <- estacoes

  spy_grid <-
    dplyr::left_join(spy_grid , data.frame(mydata), by = "VALUE")
  spy_grid <- spy_grid %>%
    sf::st_buffer(dist = 0)
  area_lito <-
    suppressWarnings({
      sf::st_intersection(spy_poly, spy_grid)
    })
  area_lito <- sf::st_transform(
    area_lito,
    paste0(
      "+proj=utm +zone=",
      zone,
      " +south +datum=WGS84 +units=m +no_defs"
    )
  )

  area_lito$Area_lito <-
    as.numeric(round(sf::st_area(area_lito, byid = TRUE) / 1000000, 3))
  area_lito$Pct_area <-
    as.numeric(round(area_lito$Area_lito / area_lito$Area_bacia * 100, 2))
  df <- as.data.frame(area_lito)

  if(tipo_leg == 1){
    chave <- "SIGLA"
  }else{
    chave <- "RANGE"
  }

  select_campos <- c(
    "VALUE",
    "Area_sqm",
    "Area_bacia",
    "N_LAB",
    "NUM_CAMPO",
    "LONG_DEC"  ,
    "LAT_DEC",
    "UTM_LESTE" ,
    "UTM_NORTE",
    "MC" ,
    "PROJETO",
    "CLASSE",
    "LOTE",
    "REQUERENTE",
    "DATA",
    chave,
    "Area_lito",
    "Pct_area"
  )
  select_campos <- colnames(df)[colnames(df) %in% select_campos]
  write.csv2(
    df[, select_campos],
    paste0(dir_out,"lito_bacia_crop.csv"),
    row.names = FALSE,
    fileEncoding = "latin1"
  )
  out[[1]] <- df[, select_campos]
  sf::st_agr(spy_poly) = "constant"

  #spy_poly <- shapefile("lito_ls")
  ## per grid cell, identify ID covering largest area
  lst <- lapply(1:nrow(spy_grid), function(i) {
    # create polygons subset based on current grid cell
    spy_poly_crp <- sf::st_crop(spy_poly, spy_grid[i, ])

    # case 1: no polygon intersects with current cell
    if (is.null(spy_poly_crp)) {
      out <- data.frame(matrix(ncol = ncol(spy_poly), nrow = 1))
      names(out) <- names(spy_poly)
      return(out)
      # case 2: one polygon intersects with current cell
    } else if (nrow(spy_poly_crp) == 1)  {
      return(spy_poly_crp)
      # case 3: multiple polygons intersect with current cell
      # -> choose sub-polygon with largest area
    } else {
      areas <- sf::st_area(spy_poly_crp, byid = TRUE)
      index <- which.max(areas)
      return(spy_poly_crp[index, ])
    }
  })

  ## to 'data.frame'
  # do.call("rbind", lst)
  ##ID <- seq(1:nrow(spy_grid@data))
  dc <- cbind(do.call("rbind", lst), spy_grid)
  if(tipo_leg == 2){  colnames(dc)[1] <- "SIGLA"}

    dc <-
    dplyr::right_join(codlito[, c("SIGLA", "RGB", "Geo_cod")], dc,  by = "SIGLA")

  mylitho <- dc[, c("VALUE", "Area_bacia", "Geo_cod")]
  mylitho$Area_bacia <- as.numeric(mylitho$Area_bacia)
  write.csv2(mylitho,
             paste0(dir_out,"mylitho.csv"),
             row.names = FALSE,
             fileEncoding = "latin1")
  out[[2]] <- mylitho
  return(out)
}
