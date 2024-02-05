#' Extrai dados da legenda de um arquivo de estilo xml
#'
#' @param file Endereço e nome do arquivo
#' @param find1 Nó de busca para a cor
#' @param indice Índice dos registros da legenda xml
#' @param value_cor Nome da variável com as cores
#' @param find2 Nó de busca para os nomes das siglas
#' @param value_name Nome da variável dos nomes das siglas
#' @param value_sub Caracteres para substituir no nome das siglas
#'
#' @return
#' @export
#'
#' @examples
resgata_legenda_xml <-
  function(file_xml ,
           find1 = "//symbol/layer" ,
           indice = "id",
           value_cor = "value",
           find2 = "//symbol" ,
           value_name = "name",
           value_sub = "geologia ") {
    # Lê arquivo xml de estilo do QGIS
    Lines <- xml2::read_xml(file_xml)

    # Extrai o valores de id do nó symbol/layer
    recs <- xml2::xml_find_all(Lines, find1)
    id <- trimws(xml2::xml_attr(recs, indice))

    # Extrai valores de cor
    n <- length(recs)
    cor <- 0
    for (i in 1:n) {
      cor[i] <-
        xml2::xml_attrs(xml2::xml_child(xml2::xml_child(recs[[i]], 1), 2))[[value_cor]]
    }
    # data frame das cores
    df_cor <- data.frame(id, cor)

    # Extrai valores de id e sigla das unidades do nó symbol
    recs2 <- xml2::xml_find_all(Lines, find2)
    nome <- 0
    n <- length(recs2)
    id <- 0
    for (i in 1:n) {
      nome[i] <-  xml2::xml_attrs(recs2[[i]], 1)[[value_name]]
      id[i] <-
        trimws(xml2::xml_attr(xml2::xml_child(recs2[[i]], 2), indice))
    }
    nome <-
      gsub(value_sub,
           "",
           nome,
           fixed = TRUE)

    # data frame dos nomes
    df_nome <- data.frame(id, nome)
    df <- dplyr::inner_join(df_nome, df_cor, by = "id")
    df <- df[!is.na(df$cor),]

    df <- tidyr::separate_wider_delim(
      df,
      cols = cor,
      delim = ",",
      names = c("r", "g", "b", "a"),
      too_few = "align_start"
    )
    df$r <- as.numeric(df$r)
    df$g <- as.numeric(df$g)
    df$b <- as.numeric(df$b)
    df$a <- as.numeric(df$a)
    df$rgb <- paste0(df$r, ",", df$g, ",", df$b)
    n <- nrow(df)
    rgb <- 0
    for (i in 1:n) {
      rgb[i] <- DescTools::RgbToHex(c(df$r[i], df$g[i], df$b[i]))
    }
    df$col_hex <- rgb
    return(df)
  }
