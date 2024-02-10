#' Extrai dados de campo do banco de dados
#'
#' Uma base de dados é preparada com as informações da localização das amostras
#' e os nomes de campo e laboratório. Esta função só está implementada para o
#' tipo_base = 1 (FCAMPO)
#'
#' @param tipo_base Tipo de base de dados: 1 = FCAMPO, 2 = SURVEY123, 3 = QFIELD
#' @param dir_base Diretório da base de dados de campo
#' @param base_campo Nome da base de dados de campo
#' @param dir_os Diretório das OS(ordens de serviço)
#'
#' @return
#' Retorna uma tabela com os dados de campo
#' @export
#'
#' @examples
#'
extrai_dados_campo <- function(tipo_base, dir_base,  base_campo, dir_os) {
# Base de dados do fcampo (mdb) ------------------------------------------------
    if (tipo_base == 1) {
    con <-
      RODBC::odbcDriverConnect(
        paste(
          "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)}",
          paste0("DBQ=", dir_base, base_campo),
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
    out <- data.frame()
    out <- df_base
  }
  RODBC::odbcCloseAll()
# Base de dados do SURVEY123
  if(tipo_base == 2){
    # Lê arquivos do geodatabase
    # Lista o que está na base
    df <- st_layers( paste0(dir_base, base_campo, ".gdb"))

    # Recupera nome da layer de pontos
    pontos <- df[["name"]][1]

    # Lê os dados da Base (pontos e amostras)
    pontos <- st_read(paste0(dir_base, base_campo, ".gdb"),
                      layer = pontos, quiet = TRUE)
    amostras <-   st_read(paste0(dir_base, base_campo, ".gdb"),
                          layer = "amostras", as_tibble = TRUE, quiet = TRUE)

    # Une os dados de campo às amostras
    df_base <-
      dplyr::inner_join(pontos, amostras, by = c("uniquerowid"= "parentrowid"))

  }
# Base de dados do QFIELD
    if(tipo_base == 3){
      # Lê arquivo no geopackage
      # Dados das amostras
      amostras <- st_read(paste0(dir_base, base_campo, ".gpkg"),
                          layer = "amostras", as_tibble = TRUE, quiet = TRUE)
      # Dados espaciais
      pontos <- st_read(paste0(dir_base, base_campo, ".gpkg"),
                               layer = "pontos_de_coleta", quiet = TRUE)
      df_base <-
        dplyr::inner_join(pontos, amostras, by = c("uniquerowid"= "parentrowid"))

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
    out <- data.frame()
    out <- df_base
}
  return(out)
}
