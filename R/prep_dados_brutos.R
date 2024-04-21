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
  # Renomeia colunas
  colnames(data)[c(2,4:5)] <- c("NLAB", "LONGITUDE", "LATITUDE")
  # Retira coluna ID
  data <- data %>% dplyr::select(-ID)
  # Cria lista de saída
  out <- list()
  # Cria coluna para receber método original
  data$Lab_orig <- data$Lab
  # Retira espaços do início da string
  data$Lab_orig <- stringr::str_trim(data$Lab_orig)
  # Retira coluna Lab
  data <-  data %>% dplyr::select(!Lab)
  # Vetor com métodos originais
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

  # Códigos simplificados de cada método
  Lab <- c("AA", "AA", "AA", "COL", "AA", "AA", "EE", "AA", "IE", "IE", "EE",
           "EE", "AA", "AA", "AA", "AA", "IE", "AA", "AA", "CR", "EE", "FA",
           "EE", "IE", "EE", "", "EE", "FA", "FA", "AA", "EE", "EE", "EE",
           "EE", "EE", "EE", "AA")
  # Lista da relação método original e código simplificado
  bb_lab <- data.frame(Lab_orig, Lab)
  # Une dados a lista acima pelo método original
  data <-  dplyr::left_join(data,bb_lab,  by = "Lab_orig")
  # Muda os campos de método para depois da classe
  data <- data %>% dplyr::relocate(c(Lab, Lab_orig), .after = CLASSE)

  # Retira espaços vazios das strings das colunas dos elementos
  data[,11: ncol(data)] <- data[,11: ncol(data)] %>%
    dplyr::mutate_all(stringr::str_replace_all, " ", "")

  # Cria coluna ordem para chaveamento
  data$ORDEM <- as.numeric(1:nrow(data))

  # Cria coluna Au_ppb se não tiver nos dados
  if(!"AU_PPB" %in% colnames(data)){
    data$AU_PPB <- NA
  }

  # Importar informações faltantes do Au ppb do Au_ppm e converter
  # Filtra base pela ausência de dados e seleciona as colunas
  para_mudar <- data[is.na(data$AU_PPB), c("ORDEM", "AU_PPM")]
  # Cria coluna de qualificador
  para_mudar$Q <- as.character(stringr::str_extract(para_mudar$AU_PPM, pattern = "<"))
  # Substitui NA por vazio
  para_mudar[is.na(para_mudar$Q), "Q"] <- ""
  # Substitui strings não numéricas do campo - value = Au_ppm
  para_mudar$value <- gsub( "<", "", para_mudar$AU_PPM, fixed = TRUE)
  para_mudar$value <-  gsub( ">", "", para_mudar$value, fixed = TRUE) # não tem
  para_mudar$value  <- gsub( ",", ".", para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "ND", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I.S.", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I,S,", NA, para_mudar$value, fixed = TRUE)
  # Transforma em value em numérico e multiplica por 1000 - value = Au_ppb
  para_mudar$value <- as.numeric(para_mudar$value)*1000
  # Retoma valor qaulificado do Au_ppb
  para_mudar$AU_PPB <- paste0(para_mudar$Q, para_mudar$value)
  # Substitui valores de AU_PPB dos dados pelo mudado
  data[data$ORDEM %in% para_mudar$ORDEM, "AU_PPB"] <- para_mudar$AU_PPB
  # Remove coluna ORDEM
  data <- data %>% dplyr::select(-"ORDEM")

  # Substitui strings inválidas por NA
  data[data == "ND"] <- NA
  data[data == "N.A."] <- NA
  data[data == "I.S."] <- NA
  data[data == "I,S,"] <- NA
  data[data == ""] <- NA
  data[data == "0"] <- NA
  data[data == "NA"] <- NA

  # Armazena dados modificados em data_b
  data_b <- data
  # Rotaciona dados modificados
  data_b_pivo <- data_b %>% tidyr::pivot_longer(cols=11:ncol(data_b),
                                                names_to = "Analito",
                                                values_to = "Valor")
  # Corrige nome dos analitos
  data_b_pivo$Analito <- gsub(".", "", data_b_pivo$Analito, fixed = TRUE)
  # Transforma em data frame e ransfer dados par df_m
  df_m <- as.data.frame(data_b_pivo)
  # Elimina linhas com Valor NA
  df_m <- df_m[!is.na(df_m$Valor),]
  # Cria coluna com qualificador
  df_m$Q <- as.character(stringr::str_extract(df_m$Valor,
                                              pattern = "<"))

  # Cria dados filtrados para não qualificados
  df_n <- df_m[is.na(df_m$Q), ]

  # Elimina linhas com Q NA
  df_m <- df_m[!is.na(df_m$Q), ]
  # Agrupa valores qualificados e e retém valor máximo no campo Valor
  df_a <- aggregate(df_m[, c( "Valor")],
                    by = list(Lab=df_m$Lab, FOLHA=df_m$FOLHA, Analito=df_m$Analito),
                    FUN = max)

  # Atibui nome da coluna Valor
  colnames(df_a)[4] <- "Valor"

  # cria variável para lista dos dados filtrados
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

  # Une os dados de cada filtro
  df_q <- do.call(rbind,df)
  # Seleciona linhas dos dados não qualificados

  # Une linhas com dados qualificados e não qualificados
  df_long <- rbind(df_q, df_n)

  # Retira coluna Q
  df_long <- df_long[,-13]

  ## Retira valores NA
  df_long <-  df_long[!is.na(df_long$Valor), ]

  # Cria contador
  df_long$Count <- 1

  # Retira dados agrupados pelo número de dados >5
  df_long <- df_long %>% dplyr::group_by(FOLHA, Lab, Analito) %>%
    dplyr::mutate(Var=sum(Count)) %>%
    dplyr::filter(Var>5) %>% dplyr::select(c(-Var, -Count))

  # Ordena pelo NLAB
  df_long <- df_long[order(df_long$NLAB),]

  # Rotaciona dados arrumados
  out[[1]] <- tidyr::pivot_wider(df_long, names_from = "Analito",
                     values_from = "Valor", values_fn = max)

  # Rotaciona dados originais
  out[[2]] <- tidyr::pivot_wider(data_b_pivo, names_from = "Analito",
                                 values_from = "Valor", values_fn = max)

  return(out)
}
