#' Lê boletins de análises mineralométricas
#'
#' @param dir_bol Diretório dos boletins analíticos ex: "inputs/mineral/B/"
#'
#' @return Retorna uma lista com todos os dados do boletim: resultados
#'   analíticos, condições analíticas, requisição das análises, preparação das
#'   amostras, separação grav-magnética.
#' @export
#'
#' @examples
#' #le_boletim_mineral()
le_boletim_mineral <- function(dir_bol) {
  #Ler Dados na planilha csv
  #as planilhas da GEOSOL vem com formato numérico com decimal . e separador ;
  #substitui no editor-MS antes . por , e  salva csv separados por virguala
  #(I.S. e N.A. são mantidos)
  # Dois layouts de boletins são admitidos. Ele funciona para boletins de 2015
  # O código precisa ser testado nos boletins mais recentes
  # Devem ter extenção xls com todas as informações do boletim


  ## Diretórios de entrada dos dados
  classes <-
    c("Concentrado de bateia",
      "Sedimento de corrente",
      "Rocha",
      "Solo")
  cod_classes <- c("B", "S", "R",  "L")
  nome_bol <-
    c("CONCENTRADO DE BATEIA", "SEDIMENTO CORRENTE", "ROCHA",
      "SOLO")

  ## Lista dos arquivos xls no diretório
  list_bol <- paste0(dir_bol, list.files(dir_bol, pattern = "*.xls"))
  datalist = list()
  datalist2 = list()
  datalist3 = list()
  datalist4 = list()
  datalist5 = list()

  # Lê arquivos xls e extrai dados dos boletins
  for (i in seq(list_bol)) {
    # for (i in 26:95) {

    df_tudo = readxl::read_xls(list_bol[i], .name_repair = "unique_quiet")

    # Controle de layout de boletins
    # fracao é tudo que foi pesado na separaçao mag/grav e o código de restrição
    # ad e ad2 é artifício para a posição de leitura
    # Se for preciso pode ser implementado novos layouts

    if (df_tudo[19, 16] == "Semi Mag") {
      ad <- 3
      ad2 <- 2

      fracao <-
        c(
          "Peso_ini",
          "Peso_frac",
          "P_conc_ma03",
          "P_bat_me03",
          "Magn_im",
          "03A",
          "05A",
          "075A",
          "N_magn",
          "Me03mm",
          "T_Pesados",
          "N_Analis",
          "Semi Mag",
          "No Mag",
          paste0(
            "Total Analisado
(Mineralogia + Sep.Magn",
            "\u00e9",
            "tico)"
          ),
          "Codigo P/I"
        )

    } else{
      ad <- 0
      ad2 <- 0

      fracao <-
        c(
          "Peso_ini",
          "Peso_frac",
          "P_conc_ma03",
          "P_bat_me03",
          "Magn_im",
          "03A",
          "05A",
          "075A",
          "N_magn",
          "Me03mm",
          "T_Pesados",
          "N_Analis",
          "Codigo P/I"
        )

    }

    # Calcula número de coluna e número de linhas do boletim original
    n <- ncol(df_tudo)
    r <- nrow(df_tudo)
    # Extrai valores dos metadados administrativos
    # É importante sempre deixar estes dados na mesma posição na planilha excel

    laboratorio <- colnames(df_tudo)[12]
    analise <- df_tudo[2, 12]
    analise <- gsub("RESULTADOS DE ", "", analise, fixed = TRUE)
    cliente <- df_tudo[3, 1]
    cliente <- gsub("Solicitado por: ", "", cliente, fixed = TRUE)
    data_criacao_arquivo <- df_tudo[r - 1, 8]
    data_criacao_arquivo <-
      gsub("Data: ", "",  data_criacao_arquivo, fixed = TRUE)
    n_job <- df_tudo[8, 1]
    n_job <-
      gsub(paste0("N", "\u00fa", "mero do consignment: "),
           "",
           n_job,
           fixed = TRUE)
    no_amostras <- df_tudo[12 - ad2, 12 - ad]
    no_amostras <-

      gsub(paste0("N", "\u00ba", " Total de Amostras: "),
           "",
           no_amostras,
           fixed = TRUE)

    responsavel <- df_tudo[13 - ad2, 12 - ad]
    responsavel <- gsub("Resp.: ", "", responsavel, fixed = TRUE)
    projeto <- df_tudo[14 - ad2, 12 - ad]
    projeto <- gsub("Projeto: ", "", projeto, fixed = TRUE)
    projeto <- gsub("CC: ", "", projeto, fixed = TRUE)
    projeto <- gsub("  ", " ", projeto, fixed = TRUE)
    projeto <- stringr::str_split(projeto, " ", simplify = TRUE)
    nm_projeto <- projeto[1]
    centro_custo <- projeto[length(projeto)]
    ship <- df_tudo[15 - ad2, 12 - ad]
    ship <- gsub("Contrato: ", "", ship, fixed = TRUE)
    numero_po <- df_tudo[16 - ad2, 12 - ad]
    numero_po <- gsub("Lote: ", "", numero_po, fixed = TRUE)
    numero_po <- gsub("RA: ", "", numero_po, fixed = TRUE)
    numero_po <- stringr::str_split(numero_po, " ", simplify = TRUE)
    lote <-  numero_po[1]
    ra <- numero_po[length(numero_po)]
    analito <- as.character(t(df_tudo[17, (17 + ad):(n - 1)]))
    analito[1:3] <- c("OURO_05", "OURO_05_1", "OURO_1")
    analito <- analito[!is.na(analito)]
    metodo <- as.character(rep(analise, length(analito)))
    metodo_prep <- rep("", length(fracao))
    unidades <- rep("pct", length(analito))
    unidades[1:3] <- rep("pta", 3)
    unidades_prep <- c(rep("g", length(fracao) - 1), "")

    MDL <- rep("", length(analito))
    MDL_prep <- rep("", length(fracao))

    boletim <- df_tudo[20:r, ]

    boletim <- boletim[!is.na(boletim[, 3]),]

    job_boletim <- as.character(rep(n_job, nrow(boletim)))

    boletim <- cbind(boletim, job_boletim)

    # Cria tabela das condições analíticas
    condicoes_analiticas <-
      data.frame(metodo, analito, unidades, MDL, n_job)
    condicoes_analiticas <-
      condicoes_analiticas[!is.na(condicoes_analiticas$analito),]


    condicoes_preparacao <-
      data.frame(metodo_prep, fracao, unidades_prep,
                 MDL_prep, n_job)


    nome2 <- c('metodo', 'analito', 'unidades', 'MDL', 'BOLETIM')
    colnames(condicoes_analiticas) <- nome2
    colnames(condicoes_preparacao) <- nome2



    nome <-
      c("SEQ", "NUM_CAMPO", "N_LAB", fracao, analito, "SEQ", "BOLETIM")

    nome <- toupper(nome)

    colnames(boletim) <- nome
    preparacao <- boletim[, 1:(14 + ad)]
    preparacao[, 4:ncol(preparacao)] <-
      apply(preparacao[, 4:ncol(preparacao)],
            2, as.numeric)

    boletim <- boletim[,-4:-(length(fracao) + 3)]

    boletim <- boletim[,c("SEQ", "NUM_CAMPO", "N_LAB", analito,"BOLETIM") ]

    # Cria tabela com informações do boletim
    info_boletim <- data.frame(
      laboratorio,
      analise,
      cliente,
      data_criacao_arquivo,
      n_job,
      no_amostras,
      responsavel,
      nm_projeto,
      centro_custo,
      ship,
      lote,
      ra
    )
    colnames(info_boletim) <- c(
      paste0("Laborat", "\u00f3", "rio"),
      paste0("M", "\u00e9", "todo"),
      "Cliente",
      "Data do arquivo",
      "Boletim" ,
      "No. de amostras",
      paste0("Respons", "\u00e1", "vel"),
      "Projeto",
      "Centro de custo",
      "Contrato",
      "Lote",
      paste0("Requisi", "\u00e7", "", "\u00e3", "o de an", "\u00e1", "lise")
    )
    boletim <- boletim[colSums(!is.na(boletim)) > 0]

    condicoes_analiticas <-
      condicoes_analiticas[condicoes_analiticas$analito %in%
                             colnames(boletim)[3:ncol(boletim)],-4]
    condicoes_preparacao <- condicoes_preparacao[, c(-1, -4)]

    # Adiciona dados extraídos as listas
    datalist[[i]] <- info_boletim
    datalist2[[i]] <- condicoes_analiticas
    datalist3[[i]] <- boletim
    datalist4[[i]] <- condicoes_preparacao
    datalist5[[i]] <- preparacao
  }

  ## Constroi as tabelas de dados extraídos
  ib = do.call(plyr::rbind.fill, datalist)
  ca = do.call(plyr::rbind.fill, datalist2)
  df = do.call(plyr::rbind.fill, datalist3)
  tp = do.call(plyr::rbind.fill, datalist4)
  dfp <- do.call(plyr::rbind.fill, datalist5)

  # Retira colunas
  df <- df[,colnames(df)[!(colnames(df) %in% c("NA.2", "NUM_CAMPO"))]]

  # Coloca Boletim na última coluna
  df <- df %>% dplyr::relocate(BOLETIM, .after = last_col())
  # Padroniza n_lab
  df$N_LAB <- gsub("-", "", df$N_LAB)
  df$N_LAB <- gsub(" ", "", df$N_LAB)
  # Substiui valores do campo BOLETIM
  df$BOLETIM <-
    gsub(paste0("N", "\u00fa", "mero do consignment: "),
         "",
         df$BOLETIM,
         fixed = TRUE)

  # Padroniza os valores nulos
  df <- data.frame(lapply(df, function(x) {
    gsub("N.A.", NA, x)
  }))

  # Pivoteia a tabela do boletim
  df1 <- df %>%
    tidyr::pivot_longer(cols = 3:(ncol(df) - 1),
                        names_to = "analito",
                        values_to = "valor")

  # Elimina linhas com campo valor = NA
  df1 <- df1[!is.na(df1$valor), ]

  # Padroniza nomes dos minerais - tira acentuação
  # Isso pode não ser necessário se adotarmos nomenclatura mineral padronizada
  df1$analito <- gsub("\u00c1", "A", df1$analito)
  df1$analito <- gsub("\u00c3", "A", df1$analito)
  df1$analito <- gsub("\u00c2", "A", df1$analito)
  df1$analito <- gsub("\u00c9", "E", df1$analito)
  df1$analito <- gsub("\u00ca", "E", df1$analito)
  df1$analito <- gsub("\u00cd", "I", df1$analito)
  df1$analito <- gsub("\u00d3", "O", df1$analito)
  df1$analito <- gsub(
    "ESPINELIO.C.TEXTURA.DUVIDOSA............SPD.",
    "ESPINELIO.C.TEXTURA.DUVIDOSA.SPD",
    df1$analito,
    fixed = TRUE
  )
  df1$analito <- gsub(".1", "", df1$analito, fixed = TRUE)
  df1$analito <- gsub(
    "CROMO.ESPINELIOS...........KIMBERLITICOS.",
    "CROMO.ESPINELIOS.KIMBERLITICOS",
    df1$analito,
    fixed = TRUE
  )
  df1$analito <-
    gsub("MAT..ORGANICA", "MAT.ORGANICA", df1$analito, fixed = TRUE)
  df1$analito <- gsub("MATERIAL..MARINHO",
                      "MATERIAL.MARINHO",
                      df1$analito,
                      fixed = TRUE)
  df1$analito <- gsub("ARGILO..MINERAL", "ARGILO.MINERAL",
                      df1$analito, fixed = TRUE)
  df1$analito <- gsub("AGREGADO..MICACEO",
                      "AGREGADO.MICACEO",
                      df1$analito,
                      fixed = TRUE)
  df1$analito <-
    gsub("HORBLENDA.", "HORNBLENDA", df1$analito, fixed = TRUE)
  df1$analito <-
    gsub("HORBLENDA", "HORNBLENDA", df1$analito, fixed = TRUE)
  df1$analito <-
    gsub("ILMENITA.KIMBERLITICO",
         "ILMENITA.KIMBERLITICA",
         df1$analito,
         fixed = TRUE)
  df1$analito <-
    gsub("BIODETRITOS", "BIO.DETRITOS", df1$analito, fixed = TRUE)
  df1$analito <-
    gsub("SIDERITE", "SIDERITA", df1$analito, fixed = TRUE)
  df1$analito <-
    gsub("GANHITA", "GAHNITA", df1$analito, fixed = TRUE)

  # Pivoteia tabela de dados arrumados com valores originais (classe)
  dfclass <- tidyr::pivot_wider(df1, names_from = "analito",
                                values_from = "valor")

  # Transforma valores das classes em % (menos para o ouro)
  # Hoje os boletins estão com essas classes, entre parentes as que usei aqui:
  # TABELA SEMIQUANTITATIVA
  # CÓDIGO TEOR		Significado
  # 85		75 - 100 %
  # 60		50 - 74 % (50 - 75 %)
  # 40		25 - 49 % (25 - 50 %)
  # 15		5 - 24 % (5 - 25 %)
  # 03		1 - 4 % (1 - 5 %)
  # 01		< 1%

  df2 <- df1 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1" &
          analito != "OURO_1" &
          valor == "15",
        "5 - 25 %",
        valor
      )
    )

  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1" &
          analito != "OURO_1" &
          valor == "1",
        "< 1 %",
        pct
      )
    )
  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1"
        &
          analito != "OURO_1" & valor == "3",
        "1 - 5 %",
        pct
      )
    )

  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1"
        &
          analito != "OURO_1" & valor == "40",
        "25 - 50 %",
        pct
      )
    )

  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1"
        &
          analito != "OURO_1" & valor == "60",
        "50 - 75 %",
        pct
      )
    )

  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1"
        &
          analito != "OURO_1" & valor == "15",
        "5 - 25 %",
        pct
      )
    )
  df2 <- df2 %>%
    plyr::mutate(
      pct = ifelse(
        analito != "OURO_05" &  analito != "OURO_05_1"
        &
          analito != "OURO_1" & valor == "85",
        "75 - 100 %",
        pct
      )
    )
  # Copia os valores em Pct para o campo valor
  df2$valor <- df2$pct
  # Elimina o campo valor_pct
  df2 <- df2[, -ncol(df2)]
  # Retira linhas com valores = 0
  df2 <- df2[df2$valor != 0,]

  # Pivoteia os dados para forma de colunas extendidas

  dfpct <-
    tidyr::pivot_wider(df2, names_from = "analito", values_from = "valor")

  # Pivoteia os dados para forma de linhas extendidas
  dfpct_pivo <-  dfpct %>%
    tidyr::pivot_longer(
      cols = 4:ncol(dfpct),
      names_to = "analito",
      values_to = "valor"
    )


  # Elimina linhas com valor=NA
  dfpct_pivo <- dfpct_pivo[!is.na(dfpct_pivo$valor), ]
  out <- list()
  out[[1]] <- dfpct # transformados
  out[[2]] <- dfpct_pivo # transformados pivotados
  out[[3]] <- dfclass # brutos
  out[[4]] <- df1 # brutos pivotados
  out[[5]] <- ca # informacões das condições analíticas
  out[[6]] <- tp # informações da preparação
  out[[7]] <- dfp # resultado da separação grav/mag
  out[[8]] <- ib # informação administrativa do boletim
  names(out) <-
    c(
      "dados transformados",
      "dados transformados pivotados",
      "dados brutos",
      "dados brutos pivotados",
      paste0("condi", "\u00e7", "\u00f5", "es anal", "\u00ed", "ticas"),
      paste0(
        "informa",
        "\u00e7",
        "\u00f5",
        "es da prepara",
        "\u00e7",
        "\u00e3",
        "o"
      ),
      paste0("separa", "\u00e7", "\u00e3", "o grav mag"),
      paste0("informa", "\u00e7", "\u00e3", "o do boletim")
    )
  return(out)
}
