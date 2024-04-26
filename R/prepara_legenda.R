#' Cria legenda apartir da shape da feição
#'
#' Este processamento usa a shape e xml do mapa das unidades litoestratigráficas
#'do Brasil (2014) obtidos do GEOSGB (SGB-CPRM).
#'
#' @param dir_in Diretório dos arquivos shape e xml
#' @param feicao Nome da feição shape e xml, o nome do xml deve ser igual
#' ao da shape
#'
#' @return Tabelas com atrubutos da shape da geologia e cores das unidades
#' (col_hex) e índice da geologia (Geo_Reg) em duas opções uma baseada no campo
#' SIGLA e outra baseada no campo RANGE.
#'
#' @export
#'
#' @examples
#' #legenda <- prepara_legenda()
#' #legenda

prepara_legenda <- function(feicao_rec, dir_out,
                            dir_in = "inputs/campo/", nome_xml = "geologia") {
  out <- list()
  lito_geo <- feicao_rec
  lito_geo <- lito_geo[!is.na(lito_geo$SIGLA), ]

  mylegend <- unique(lito_geo)
  mylegend <- lito_geo[!duplicated(lito_geo$SIGLA), ]
  mylegend <- mylegend[order(mylegend$IDADE_MIN), ]
  mylegend <- as.data.frame(mylegend)
  legenda <- resgata_legenda_xml(dir_xml = dir_in,
                                 nome_xml = nome_xml )
  colnames( legenda) <- gsub("nome", "SIGLA",colnames( legenda)  )
  legenda_geo <- dplyr::left_join(mylegend,
                             legenda,
                             by = "SIGLA")

  # Retira valores nulos
  legenda_geo <- legenda_geo[!is.na(legenda_geo$col_hex),]

  # Arruma campo col_hex
  legenda_geo[nchar(legenda_geo$col_hex) == 6, "col_hex"] <-
    gsub("#", "#0", legenda_geo[nchar(legenda_geo$col_hex) == 6, "col_hex"])
  legenda_geo[nchar(legenda_geo$col_hex) == 5, "col_hex"] <-
    gsub("#", "#00", legenda_geo[nchar(legenda_geo$col_hex) == 5, "col_hex"], fixed = TRUE)

  # Ordena pela idade mínima
  legenda_geo <- legenda_geo[order(legenda_geo$"IDADE_MIN"), ]

  # Gera códigos para cada SIGLA
  legenda_geo$Geo_Reg <- 1:nrow(legenda_geo)
  # Arruma nomes das colunas
  df <- legenda_geo[, c("SIGLA", "NOME", "ERA_MIN", "col_hex", "Geo_Reg")]
  colnames(df)[2:5] <- c("DESCRICAO", "IDADE", "RGB", "Geo_cod")
  out[[1]] <- df

  # Legenda agrupada pelo campo RANGE
  col_range <-
    colouR::avgHex(df = legenda_geo,
                   group_col = 'RANGE',
                   hex_col = 'col_hex')

  colnames(col_range)[2] <- "col_hex2"
  col_range <- col_range[, -3]
  range <-
    data.frame(RANGE = legenda_geo$RANGE, IDADE_MIN = legenda_geo$IDADE_MIN)
  range <- unique(range)
  range <- range[order(range$IDADE_MIN), ]
  range$Geo_Reg2 <- 1:nrow(range)
  col_range <-
    base::merge(range, col_range,  by.x = "RANGE", by.y = "group")
  col_range <- col_range[,-2]
  legenda_geo <-
    dplyr::left_join(legenda_geo, col_range, by = "RANGE")
  legenda_geo$col_hex <- toupper(legenda_geo$col_hex)
  legenda_geo <- legenda_geo %>%
    dplyr::relocate(col_hex, .after = Geo_Reg)
  # Arruma nomes das colunas
  df <- legenda_geo %>%
    dplyr::group_by(RANGE) %>%
    dplyr::mutate(LITOTIPOS = paste0(LITOTIPOS, collapse = ", "))

  df <- df[, c("RANGE", "LITOTIPOS", "ERA_MIN", "col_hex2", "Geo_Reg2")]
  df <- unique(df)

  colnames(df)[c(1,2:5)] <- c("SIGLA","DESCRICAO", "IDADE", "RGB", "Geo_cod")

  # Retira nomes de litotipos iguais da descrição em cada RANGE
  df$DESCRICAO <- sapply(strsplit(df$DESCRICAO, ", "),
                         function(x) paste(unique(x), collapse = ", "))
  out[[2]] <- df

  return(out)
}
