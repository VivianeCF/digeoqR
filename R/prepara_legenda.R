#' Cria legenda apartir da shape da feição
#'
#' @param file_shp Endereço e nome do arquivo da geologia
#' @param file_xlm Endereço e nome do arquivo xlm
#' @return Tabela com atrubutos da shape da geologia e cores das unidades (col_hex)
#' e índice da geologia (Geo_Reg)
#' @export
#'
#' @examples
#' #legenda <- prepara_legenda()
#' #legenda

prepara_legenda <- function(file_shp = "inputs/campo/geologia.shp",
                            file_xml = "inputs/diversos/geologia.xml") {
  lito_geo <- sf::st_read(file_shp)
  lito_geo <- lito_geo[!is.na(lito_geo$SIGLA), ]

  mylegend <- unique(lito_geo)
  mylegend <- lito_geo[!duplicated(lito_geo$SIGLA), ]
  mylegend <- mylegend[order(mylegend$IDADE_MIN), ]
  mylegend <- as.data.frame(mylegend)
  legenda <- resgata_legenda_xml(file_xml)
  legenda_geo <- base::merge(mylegend,
                             legenda,
                             by.x = "SIGLA",
                             by.y = "nome",
                             all.x = TRUE)

  legenda_geo[nchar(legenda_geo$col_hex) == 6, "col_hex"] <-
    gsub("#", "#0", legenda_geo[nchar(legenda_geo$col_hex) == 6, "col_hex"])
  legenda_geo <- legenda_geo[order(legenda_geo$"IDADE_MIN"), ]
  legenda_geo$Geo_Reg <- 1:nrow(legenda_geo)
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
  return(legenda_geo)
}
