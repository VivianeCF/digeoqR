#' Prepara os dados brutos
#'
#' Padroniza os LDs para o mínimo
#'
#' @param data dados brutos
#'
#' @return
#' @export
#'
#' @examples
prep_dados_brutos <- function(data){
  colnames(data)[c(2,4:5)] <- c("NLAB", "LONGITUDE", "LATITUDE")
  data <- data %>% dplyr::select(-ID)
  out <- list()
  data$Lab_orig <- data$Lab
  data$Lab_orig <- stringr::str_trim(data$Lab_orig)
  data <-  data %>% dplyr::select(!Lab)
  Lab_orig <-  c("Absorção Atômica - Água Régia Invertida",
                 "Absorção Atômica - EDTA a Frio",
                 "Absorção Atômica - HNO3 a Quente",
                 "Colorimetria",
                 "Absorção Atômica - H3PO4",
                 "Absorção Atômica/Geração de Hidretos - Água Régia a Quente",
                 "Espectrografia Ótica de Emissão",
                 "Absorção Atômica - Fusão Ácida",
                 "Medidor de Íon Específico - Semiquantitativa",
                 "Medidor de Íon Específico - HCl Diluído a Frio",
                 "ICP-MS - Água Régia",
                 "ICP-AES - Digestão água régia",
                 "Espectrofotometria de Absorção Atômica - Ácido Nítrico a quente (HN0₃)",
                 "Espectrofotometria de Absorção Atômica - Ácido bromídrico (HBr)",
                 "Abserção Atômica / Geração de Hidretos (AAGH) - Ácidos fortes em forno de microondas",
                 "Absorção Atômica - HBr, Br",
                 "Medidor de Íon Específico",
                 "Absorção Atômica - HNO3, HF, HCl, HClO4",
                 "Absorção Atômica - Sublimação KI",
                 "Cromatografia - Semiquantitativa",
                 "ICP-MS - Digestão Multiácida",
                 "Fusão de 50g - Fire Assay",
                 "ICP-MS - Digestão Água Régia",
                 "Eletrodo de Íon Específico F - Fusão e Dissolução",
                 "1F",
                 "",
                 "ICM14B",
                 "FAA505",
                 "FAI515",
                 "Espectrofotometria de Absorção Atômica - Ácido Nítrico a quente (HN03)",
                 "Espectrografia Ótica de Emissão - Semiquantitativa",
                 "NA",
                 "Água régia_ICP-MS + ICP-AES",
                 "SCR33",
                 "ICM40B",
                 "ICP-AES",
                 "AAS19V")

  Lab <- c("AA", "AA", "AA", "COL", "AA", "AA", "EE", "AA", "IE", "IE", "EE",
           "EE", "AA", "AA", "AA", "AA", "IE", "AA", "AA", "CR", "EE", "FA",
           "EE", "IE", "EE", "", "EE", "FA", "FA", "AA", "EE", "EE", "EE",
           "EE", "EE", "EE", "AA")

  bb_lab <- data.frame(Lab_orig ,
                       Lab)
  data <-  dplyr::left_join(data,bb_lab,  by = "Lab_orig")
  data <- data %>% dplyr::relocate(c(Lab, Lab_orig), .after = CLASSE)
  data[,11: ncol(data)] <- data[,11: ncol(data)] %>%
    dplyr::mutate_all(stringr::str_replace_all, " ", "")
  data$ORDEM <- as.numeric(1:nrow(data))

  # Importar informações faltantes do Au ppb do Au_ppm e converter
  if(!"AU_PPB" %in% colnames(data)){
    data$AU_PPB <- NA
  }

  para_mudar <- data[is.na(data$AU_PPB), c("ORDEM", "AU_PPM")]
  para_mudar$Q <- as.character(stringr::str_extract(para_mudar$AU_PPM, pattern = "<"))
  unique(para_mudar$Q)
  para_mudar[is.na(para_mudar$Q), "Q"] <- ""
  para_mudar$value <- gsub( "<", "", para_mudar$AU_PPM, fixed = TRUE)
  para_mudar$value <-  gsub( ">", "", para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( ",", ".", para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "ND", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I.S.", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I,S,", NA, para_mudar$value, fixed = TRUE)

  para_mudar$value <- as.numeric(para_mudar$value)*1000
  para_mudar$AU_PPB <- paste0(para_mudar$Q, para_mudar$value)
  unique(para_mudar$AU_PPB)

  data[data$ORDEM %in% para_mudar$ORDEM, "AU_PPB"] <- para_mudar$AU_PPB

  data <- data %>% dplyr::select(-"ORDEM")


  data[data == "ND"] <- NA
  data[data == "N.A."] <- NA
  data[data == "I.S."] <- NA
  data[data == "I,S,"] <- NA
  data[data == ""] <- NA
  data[data == "0"] <- NA
  data[data == "NA"] <- NA

  data_b <- data
  data_b_pivo <- data_b %>% tidyr::pivot_longer(cols=11:ncol(data_b),
                                                names_to = "Analito",
                                                values_to = "Valor")
## Corrige nome dos analitos
  data_b_pivo$Analito <- gsub(".", "", data_b_pivo$Analito, fixed = TRUE)

  df_m <- as.data.frame(data_b_pivo)
  df_m <- df_m[!is.na(df_m$Valor),]
  df_m$Q <- as.character(stringr::str_extract(df_m$Valor,
                                              pattern = "<"))

  df_m <- df_m[!is.na(df_m$Q), ]
  df_a <- aggregate(df_m[, c( "Valor")],
                    by = list(Lab=df_m$Lab, FOLHA=df_m$FOLHA, Analito=df_m$Analito),
                    FUN = max)

  colnames(df_a)[4] <- "Valor"


  df <- list()
  for(i in 1:nrow(df_a)){
    analito <- df_a[ i, "Analito"]
    folha <- df_a[ i, "FOLHA"]
    lab <- df_a[i, "Lab"]

    fq <- df_m %>% dplyr::filter(Analito == analito & FOLHA == folha & Lab == lab )
    fa <- df_a %>% dplyr::filter(Analito == analito & FOLHA == folha & Lab == lab )
    fq$Valor <- fa$Valor
    df[[i]] <- fq
  }
  df_q <- do.call(rbind,df)

  df_n <- data_b_pivo[data_b_pivo$NLAB %in% df_q$NLAB,]
  df_q <- df_q[,-13]

  df_long <- rbind(df_q, df_n)
  df_long <- df_long[order(df_long$NLAB),]
  out[[1]] <- tidyr::pivot_wider(df_long, names_from = "Analito",
                     values_from = "Valor", values_fn = max)
  out[[2]] <- tidyr::pivot_wider(data_b_pivo, names_from = "Analito",
                                 values_from = "Valor", values_fn = max)

  return(out)
}
