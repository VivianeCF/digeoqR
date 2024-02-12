
#' Intersecta polígonos das bacias com outra feição
#'
#'Cada bacia é recortada pelos polígonos da feição de sobreposição, que pode ser
#' de mapeamento.
#'
#' @param limite Limite da área do projeto
#' @param bacias bacias obtidas a partir da função gera_bacias
#' @param feicao polígonos de algum tema (geologia,
#' solo, geofísica, geomorfologia, etc)
#' @param legenda Rótulos das unidades da feição
#' @param estacoes Estações de coleta
#' @param dir_campo Diretório dos dados espaciais e legenda
#' @param dir_out Diretório de saída onde serão gravados as planilhas .csv
#'
#' @return
#' Bacias
#' @export
#'
#' @examples
#' #intersecta_bacias()
intersecta_bacias <- function(dir_campo = "inputs/campo/",
                              dir_out = "outputs/",
                              limite = "carta_100M",
                              bacias = "bacias_area",
                              feicao = "geologia",
                              tipo_leg = 1,
                              estacoes = "estacoes",
                              file_shp = "inputs/campo/geologia.shp",
                              file_xml = "inputs/diversos/geologia.xml")
{

  options(encoding = "latin1")
  out <- list()
  area <- sf::read_sf(paste0(dir_campo, limite, ".shp"))
  centroide <-
    sf::st_coordinates(suppressWarnings({
      sf::st_centroid(area)
    }))
  zone <- as.numeric(floor((centroide[, 1] + 180) / 6) + 1)
  ## import data
  spy_grid <- sf::read_sf(paste0(dir_campo, bacias, ".shp"))
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
  spy_poly = sf::read_sf(paste0(dir_campo, feicao, ".shp"))
  spy_poly <- sf::st_transform(spy_poly,
                               paste0(
                                 "+proj=utm +zone=",
                                 zone,
                                 " +south +datum=WGS84 +units=m +no_defs"
                               ))
  legenda <- prepara_legenda(file_shp, file_xml )
  codlito <- legenda[[tipo_leg]]

  mydata <- sf::read_sf(paste0(dir_campo, estacoes, ".shp"))

  spy_grid <-
    dplyr::left_join(spy_grid , data.frame(mydata), by = "VALUE")
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
    "SIGLA",
    "NOME",
    "Area_lito",
    "Pct_area"
  )

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


    dc <-
    dplyr::right_join(codlito[, c("SIGLA", "RGB", "Geo_cod")], dc,  by =  "SIGLA")
  # colnames(dc)
  select_campos2 <-
    c(
      "SIGLA",
      "RGB" ,
      "Geo_cod"  ,
      "OBJECTID"  ,
      "ID_UNIDADE",
      "HIERARQUIA" ,
      "NOME"  ,
      "AMBIENTE_T",
      "SUB_AMBIEN",
      "SIGLA_PAI" ,
      "NOME_PAI"  ,
      "LEGENDA"   ,
      "ESCALA"  ,
      "MAPA" ,
      "LITOTIPOS" ,
      "RANGE"   ,
      "IDADE_MIN" ,
      "IDADE_MAX" ,
      "EON_MIN"  ,
      "EON_MAX"   ,
      "ERA_MIN"  ,
      "ERA_MAX"   ,
      "SISTEMA_MI",
      "SISTEMA_MA",
      "EPOCA_MIN" ,
      "EPOCA_MAX" ,
      "SIGLAS_HIS",
      "GRUPO"  ,
      "SHAPE.AREA" ,
      "SHAPE.LEN" ,
      "VALUE" ,
      "Area_sqm"  ,
      "Area_bacia",
      "N_LAB"  ,
      "NUM_CAMPO",
      "LONG_DEC",
      "LAT_DEC"  ,
      "UTM_LESTE" ,
      "UTM_NORTE" ,
      "MC",
      "PROJETO"   ,
      "CLASSE"    ,
      "LOTE" ,
      "REQUERENTE",
      "DATA"
    )
  dc$Area_bacia <- as.numeric(dc$Area_bacia)
  write.csv2(
    dc[, select_campos2],
    paste0(dir_out, "mylitho_completa.csv"),
    row.names = FALSE,
    fileEncoding = "latin1"
  )
 out[[2]] <- dc[, select_campos2]
  mylitho <- dc[, c("VALUE", "Area_bacia", "Geo_cod")]
  mylitho$Area_bacia <- as.numeric(mylitho$Area_bacia)
  write.csv2(mylitho,
             paste0(dir_out,"mylitho.csv"),
             row.names = FALSE,
             fileEncoding = "latin1")
  out[[3]] <- mylitho
  return(out)
}
