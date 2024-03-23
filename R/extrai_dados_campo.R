#' Extrai dados de campo do banco de dados
#'
#' Uma base de dados é preparada com as informações da localização das amostras
#' e os nomes de campo e laboratório. Esta função está implementada para
#' diferentes bases de dados: FCAMPO, SURVEY123 e QFIELD. Usa as planilhas de OS
#' (Ordem de Serviço) para recuperar a relação número de campo e número
#' de laboratório.
#'
#' @param tipo_base Tipo de base de dados: 1 = FCAMPO, 2 = SURVEY123, 3 = QFIELD
#' @param dir_in Diretório da base de dados de campo
#' @param base_campo Nome da base de dados de campo
#' @param dir_os Diretório das planilhas de OS
#' @param dir_out Diretório de saída
#' @param EPSG Sistema de coordenadas
#'
#' @return
#' Retorna uma tabela com os dados de campo
#' @export
#'
#' @examples
#'
extrai_dados_campo <- function(tipo_base,
                               dir_in,
                               base_campo,
                               dir_os,
                               EPSG,
                               dir_out) {
# Base de dados do fcampo (mdb) ------------------------------------------------

  if (tipo_base == 1) {
    con <-
      RODBC::odbcDriverConnect(
        paste(
          "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)}",
          paste0("DBQ=", dir_in, base_campo),
          "Trusted_Connection=Yes" ,
          sep = ";"
        )
      )


    dados <- RODBC::sqlFetch(con, 'AM_DADOS')
    local <- RODBC::sqlFetch(con, 'AM_LOCALIZACAO')
    unida <- dplyr::left_join(dados, local, by = "AMOSTRA")
    os <- RODBC::sqlFetch(con, 'BB_PROJETO')
    unida_os <- dplyr::left_join(unida, os, by = "OS")
    select <- colnames(unida_os)[c(1, 2, 5, 32, 33, 46, 47, 48, 63)]
    df_base <- unida_os[, select]
    colnames(df_base)[3] <- "COD_CLASSE"
    am_lote <- RODBC::sqlFetch(con, 'AM_LOTE')
    am_lote_amostra <- RODBC::sqlFetch(con, 'AM_LOTE_AMOSTRA')
    colnames(am_lote_amostra)[2] <- "COD_AM_LOTE"
    am_lote_amostra <- am_lote_amostra[, -1]
    bb_dados_classe <- RODBC::sqlFetch(con, 'BB_DADOS_CLASSE')
    df_base <-
      dplyr::left_join(df_base, bb_dados_classe, by = "COD_CLASSE")
    df_base <- df_base[, -11]
    df_base <- df_base[, -3]
    am_lote_amostra <-
      dplyr::left_join(am_lote_amostra, am_lote, by = "COD_AM_LOTE")
    am_lote_amostra <- am_lote_amostra[, -1]
    df_base <-
      dplyr::left_join(df_base, am_lote_amostra, by = "AMOSTRA")
    colnames(df_base)[1] <- "N_LAB"
  }
  RODBC::odbcCloseAll()
  out <- list()
  # Base de dados do SURVEY123
  if(tipo_base == 2){
    # Lê arquivos do geodatabase
    # Lista o que está na base
    df <- sf::st_layers( paste0(dir_in, base_campo, ".gdb"))

    # Recupera nome da layer de pontos
    pontos <- df[["name"]][1]

    # Lê os dados da Base (pontos e amostras)
    pontos <- sf::st_read(paste0(dir_in, base_campo, ".gdb"),
                      layer = pontos, quiet = TRUE)
    amostras <- sf::st_read(paste0(dir_in, base_campo, ".gdb"),
                          layer = "amostras", as_tibble = TRUE, quiet = TRUE)
    amostras <- dplyr::select(amostras, !c("globalid", "created_date",
                                           "created_user", "last_edited_date",
                                           "last_edited_user"))
    # Une os dados de campo às amostras
    df_base <-
      dplyr::inner_join(pontos, amostras, by = c("uniquerowid"= "parentrowid"))

    xy <- sf::st_coordinates(df_base)
    df_base <- data.frame(xy, df_base)
    colnames(df_base)[1:2] <- c("LONG_DEC", "LAT_DEC" )
    df_base <- dplyr::select(df_base, -"shape")

}
# Base de dados do QFIELD
    if(tipo_base == 3){
      # Lê arquivo no geopackage
      # Dados das amostras
      amostras <- sf::st_read(paste0(dir_in, base_campo, ".gpkg"),
                          layer = "amostras", as_tibble = TRUE, quiet = TRUE)
      amostras <- dplyr::select(amostras, !c("objectid", "globalid", "created_date",
                                             "created_user", "last_edited_date",
                                             "last_edited_user"))
      # Dados espaciais
      pontos <- sf::st_read(paste0(dir_in, base_campo, ".gpkg"),
                               layer = "pontos_de_coleta", quiet = TRUE)
      df_base <-
        dplyr::inner_join(pontos, amostras, by = c("uniquerowid"= "parentrowid"))
      df_base <- as.data.frame(df_base)
      # xy <- sf::st_coordinates(df_base)
      # df_base <- data.frame(xy, df_base)
      # colnames(df_base)[1:2] <- c("longitude", "latitude" )
      df_base <- dplyr::select(df_base, -"geom")

    }

  if(tipo_base %in% 2:3){
    lista_osq <- list()
    lista_osm <- list()
    dir_os <- "inputs/os/"
    os <- list.files(dir_os)
    os_mineral <- os[stringr::str_detect(os, "mineral")]
    os_quimica <- os[stringr::str_detect(os, "quimica")]
    for(i in 1:length(os_quimica)){
      lista_osq[[i]] <- readxl::read_xlsx(paste0(dir_os, os_quimica[i]),
                                         sheet = "Dados Amostras")
    }
    for(i in 1:length(os_mineral)){
      lista_osm[[i]] <- readxl::read_xlsx(paste0(dir_os, os_mineral[i]),
                                         sheet = "Dados Amostras")
    }
    os <- do.call(dplyr::bind_rows,lista_osq)
    ncampo <- os[!is.na(os$...4),4]
    nlab <- os[!is.na(os$...7),7]
    df <- data.frame(ncampo, nlab)
    df <- drop_na(df)
    df <- df[df$...4 != "Número de Campo", ]
    colnames(df) <- c("NUM_CAMPO", "NUM_LAB")
    df_base1 <-
      dplyr::inner_join(df_base, df, by = c("cd_numero_campo"= "NUM_CAMPO"))

    os <- do.call(dplyr::bind_rows,lista_osm)
    ncampo <- os[!is.na(os$...4),3]
    nlab <- os[!is.na(os$...7),4]
    df <- data.frame(ncampo, nlab)
    df <- drop_na(df)
    df <- df[df$...3 != "Número de Campo", ]
    colnames(df) <- c("NUM_CAMPO", "NUM_LAB")
    df_base2 <-
      dplyr::inner_join(df_base, df, by = c("cd_numero_campo"= "NUM_CAMPO"))
    df_base <- data.frame(rbind(df_base1, df_base2))
    df_base <-  df_base[, -ncol(df_base)]
  }
  df_base$LONG_DEC <- round(as.numeric(gsub(",", ".", df_base$LONG_DEC, fixed = TRUE)), 5)
  df_base$LAT_DEC <- round(as.numeric(gsub(",", ".", df_base$LAT_DEC, fixed = TRUE)), 5)

  out[[1]] <- df_base

  # Cria dados espaciais
  r <-  EPSG
  # df_base <- df_base[df_base$CLASSE == nm_classe[classe_am],]

    dados_campo_st <-
    sf::st_as_sf(df_base,
                 coords = c("LONG_DEC", "LAT_DEC"),
                 crs = r, remove = FALSE )
  out[[2]] <- dados_campo_st
  sf::st_write(
    dados_campo_st,
    paste0(dir_out, "estacoes", ".shp"),
    driver = "ESRI Shapefile",
    delete_layer = TRUE
  )
  names(out) <- c("dados de campo", "estações" )
  return(out)
}
