#' Lê boletins de análises químicas
#'
#' Esta função lê os boletins csv e extrai as informações na forma de tabelas
#' que são gravadas no diretório de saída.
#'
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo, 5 = água
#' @param dir_bol Diretório dos boletins analíticos ex: "inputs/quimica/R/"
#' @param ref_ucc Planilha com valores da Concentração Média da Crosta Superior
#' Rudnick e Gao 2004
#'
#' @return Retorna uma lista com todos os dados do boletim: resultados
#'   analíticos, condições analíticas, QA/QC, requisição das análises e
#'   preparação das amostras.
#' @export
#' @examples
#' # le_boletim_quimica()
le_boletim_quimica <- function(classe_am, dir_bol, ref_ucc) {
  # library(tidyverse)
  # require(rgr)
  options(OutDec = ",")
  # tipo <- "SEDIMENTO CORRENTE"

  ## Diretórios de entrada dos dados
  classes <-
    c("Concentrado de bateia",
      "Sedimento de corrente",
      "Rocha",
      "Solo",
      "\u00c1gua")
  cod_classes <- c("B", "S",   "L", "R", "A")
  nome_bol <-
    c("CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "SOLO",
      "ROCHA",

      "\u00c1GUA")

  ## Gera o camionho para os arquivos
  ## Entrada

  list_bol <- paste0(dir_bol, list.files(dir_bol, pattern = "*.csv"))

  ## Cria listas com cada informação do boletim
  datalist = list()
  datalist2 = list()
  datalist3 = list()
  datalist4 = list()
  datalist5 = list()
  out <- list()
  ## Condição para leitura do boletim
  if (nome_bol[classe_am] == "ROCHA") {
    ini = 6
  } else {
    ini = 12
  }


  ## Ler cada boletim do diretório e extrair as informações
  for (i in 1:length(list_bol)) {
    df_tudo = read.csv2(
      list_bol <- paste0(dir_bol, list.files(dir_bol, pattern = "*.csv"))[i],
      header = F,
      as.is = T,
      fill = TRUE,
      encoding = "latin1"
    )

    n <- ncol(df_tudo)
    r <- nrow(df_tudo)
    status <- df_tudo[1, 6]
    laboratorio <- df_tudo[1, 1]
    cliente <- df_tudo[2, 2]
    data_criacao_arquivo <- df_tudo[3, 2]
    n_job <- df_tudo[4, 2]
    lote <- df_tudo[7, 2]
    no_amostras <- df_tudo[5, 2]
    projeto <- df_tudo[6, 2]
    ship <- df_tudo[7, 2]
    numero_po <- df_tudo[8, 2]
    metodo <- t(df_tudo[11, ini:n])
    # Artifício para caso o método tenha aproximadamente o mesmo nome
    metodo <- paste0(metodo, "xx")
    analito <- t(df_tudo[12, ini:n])
    analito <- gsub("_", ".", analito)
    unidades <- tolower(t(df_tudo[13, ini:n]))
    unidades <- gsub("%", "pct", unidades)
    MDL <- t(df_tudo[14, ini:n])
    boletim <- df_tudo[16:r, c(1:3, 5, ini:n)]

    ## Condição para extrai condições de preparação
    if (nome_bol[classe_am] != "ROCHA") {
      fracao <- t(df_tudo[12, 6:11])
      metodo_prep <- t(df_tudo[11, 6:11])
      unidades_prep <- t(df_tudo[13, 6:11])
      unidades_prep <- gsub("%", "pct", unidades_prep)
      MDL_prep <- t(df_tudo[14, 6:11])
      preparacao <- df_tudo[16:r, c(1:3, 5, 6:11)]
      job_prep <- rep(n_job, nrow(preparacao))
      preparacao <- data.frame(preparacao, job_prep)
      colnames(preparacao) <-
        c(
          "sample_ID",
          "cod_am",
          "N_LAB",
          "classe_am",
          paste0(fracao, "_", unidades_prep, "_", metodo_prep),
          "Boletim"
        )
      condicoes_preparacao <-
        data.frame(metodo_prep, fracao, unidades_prep,
                   MDL_prep, n_job)

      ## Arruma o nome da tabela
      var.name <-
        c('metodo', 'analito', 'unidades', 'MDL', 'Boletim')
      colnames(condicoes_preparacao) <- var.name

      # adiciona às listas
      datalist5[[i]] <- preparacao
      datalist4[[i]] <- condicoes_preparacao
    }

    lotes <- rep(lote, nrow(boletim))
    job_boletim <- rep(n_job, nrow(boletim))
    boletim <- cbind(boletim, job_boletim, lotes)
    #cria tabela das condições analíticas
    LAB <- rep(laboratorio, length(metodo))
    condicoes_analiticas <-
      data.frame(metodo, analito, unidades, MDL, n_job, LAB)
    var.name <-
      c('metodo',
        'analito',
        'unidades',
        'MDL',
        'Boletim',
        "Laborat\u00f3rio")
    colnames(condicoes_analiticas) <- var.name
    colnames(boletim) <-
      c(
        "sample_ID",
        "cod_am",
        "N_LAB",
        "classe_am",
        paste0(analito, "_", unidades, "_", metodo),
        "Boletim",
        "Lote"
      )
    #cria tabela com informações do boletim
    info_boletim <- data.frame(
      status,
      laboratorio,
      cliente,
      data_criacao_arquivo,
      n_job,
      no_amostras,
      projeto,
      ship
    )
    colnames(info_boletim) <- c(
      "status",
      "laboratorio",
      "cliente",
      "data do arquivo",
      "Boletim" ,
      "no. de amostras",
      "projeto",
      "entrega dos resultados"
    )
    # addiciona às listas
    datalist[[i]] <- info_boletim
    datalist2[[i]] <- condicoes_analiticas
    datalist3[[i]] <- boletim


  }

  ib = do.call(plyr::rbind.fill, datalist)
  ca = do.call(plyr::rbind.fill, datalist2)

  df = do.call(plyr::rbind.fill, datalist3)



  colnames(df) <- gsub("%", "pct", colnames(df))

  # Colocar Boletim e lote no final
  df <- df %>% dplyr::relocate(Boletim, .after = last_col())
  df <- df %>% dplyr::relocate(Lote, .after = last_col())

  # Padronizar nome de laboratório
  df$N_LAB <- gsub("-", "", df$N_LAB)
  df$N_LAB <- gsub(" ", "", df$N_LAB)

  # Arrumar nomes dos analitos e inserir coluna dos métodos
  lista_metodos <-
    c(unique(ca$metodo)) # lista dos métodos com artifícios
  ca$metodo <-
    gsub("xx", "", ca$metodo) # tira artifícios do nome dos métodos
  df_metodo <- list()
  for (i in seq(lista_metodos)) {
    df_metodo[[i]] <- dplyr::select(df,
                                    contains(paste0("_", lista_metodos[i])))
    metodo <- rep(lista_metodos[i], nrow(df))
    metodo <-
      gsub("xx", "", metodo) ## retorna nome original do método
    df_metodo[[i]] <-
      data.frame(df[, c(1:4, ncol(df) - 1, ncol(df))],
                 metodo, df_metodo[[i]])
    colnames(df_metodo[[i]]) <-
      gsub(paste0("_", lista_metodos[i]),
           "",
           colnames(df_metodo[[i]]),
           fixed = TRUE)

  }

  ## Une as linhas de cada método
  df <- do.call(plyr::rbind.fill, df_metodo)

  # Tirar registros de branco de preparação
  df_sc <-
    df[df$classe_am == nome_bol[classe_am] & df$N_LAB != "BRANCO_PREP",]


  ## Retira linhas sem N_LAB
  df_sc <- df_sc[!is.na(df_sc$N_LAB),]

  ## Substitui valores nulos por NA
  df_sc <- data.frame(lapply(df_sc, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))
  df_sc <- data.frame(lapply(df_sc, function(x) {
    gsub("I,N,F,", NA, x, fixed = TRUE)
  }))

  ## Pivoteia os dados analíticos
  df_bruto_pivo <- df_sc %>%
    tidyr::pivot_longer(
      cols = 8:ncol(df_sc),
      names_to = "analito",
      values_to = "valor"
    )

  ## Retira valores com NA
  df_bruto_pivo <- df_bruto_pivo[!is.na(df_bruto_pivo$valor), ]

  ## Volta para a forma inicioal (sem NA)
  dpivo <-
    tidyr::pivot_wider(df_bruto_pivo,
                       names_from = "analito",
                       values_from = "valor")


  df_bk <-
    df[df$classe_am == "BRANCO_PREP" | df$N_LAB == "BRANCO_PREP",]

  df_rp <- df[df$classe_am == "REP" | df$classe_am == "DUP" &
                df$N_LAB != "BRANCO_PREP",]

  df_sd <- df[df$classe_am == "STD",]

  QAQC_orig <- rbind(df_rp, df_bk, df_sd)
  QAQC_orig <- QAQC_orig[, c(-4:-11)]


  # transformação < para -
  # Substitui dados qualificados
  ### Substitui srting < por - e elimina >
  df_sc_transf <- data.frame(lapply(dpivo, function(x) {
    gsub("<", "-", x, fixed = TRUE)
  }))


  QAQC_transf <- data.frame(lapply(QAQC_orig, function(x) {
    gsub("<", "-", x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub(">", "", x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub("I.S.", NA, x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub(">", "", x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("<NA>", NA, x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("--", NA, x, fixed = TRUE)
  }))
  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("I.S.", NA, x, fixed = TRUE)
  }))

  df_sc_transf <-
    df_sc_transf[df_sc_transf$N_LAB != "BRANCO_PREP", ]

  ## Substitui todos os dados ausentes codificados como -9999 por NAs
  # e  valores negativos que representam os valores menores do que valor
  # de detecção por Abs(valor)/2

  QAQC_transf <-
    as.data.frame(apply(QAQC_transf, 2, function(x)
      gsub(",", "\\.", x)))
  QAQC_transf <-
    QAQC_transf %>% dplyr::mutate(dplyr::across(4:ncol(QAQC_transf),
                                                ~ as.numeric(.)))
  QAQC_05ld <- rgr::ltdl.fix.df(QAQC_transf)

  df_sc_transf <-
    as.data.frame(apply(df_sc_transf, 2, function(x)
      gsub(",", "\\.", x)))
  df_sc_transf <-
    df_sc_transf %>% dplyr::mutate(dplyr::across(8:ncol(df_sc_transf),
                                                 ~ as.numeric(.)))

  df_sc_05ld <- rgr::ltdl.fix.df(df_sc_transf)

  if (nome_bol[classe_am] != "ROCHA") {
    tp = do.call(plyr::rbind.fill, datalist4)
    dfp <- do.call(plyr::rbind.fill, datalist5)
    df_peneira <-
      dfp[dfp$classe_am == nome_bol[classe_am] &
            dfp$N_LAB != "BRANCO_PREP" &
            dfp$cod_am == "SMP", ]
    out[[9]] <- df_peneira # Pesos da preparação
    out[[10]] <- tp # Informações da preparação

  } else {
    out[[9]] <- data.frame()
    out[[10]] <- data.frame()
  }
  out[[2]] <- df_sc_05ld # dados transformados <ld


  ## Pivoteia os dados transformados
  df2 <- df_sc_05ld %>%
    tidyr::pivot_longer(
      cols = 8:ncol(df_sc_05ld),
      names_to = "analito",
      values_to = "valor"
    )

  ## Retira linhas com valor = NA
  df2 <- df2[!is.na(df2$valor), ]

  ## Cria colunas analito e unidade
  df2 <- df2 %>%
    tidyr::separate(analito, c("analito", "unidade"), "_")
  ## Corrige nomes dos laboratórios
  ca$'Laborat\u00f3rio' <-
    gsub(
      paste0(
        "ACME ANALYTICAL LABORATORIES LTD. 852 E.",
        " HASTINGS ST. VANCOUVER BC "
      ),
      "ACME",
      ca$'Laborat\u00f3rio'
    )
  ca$'Laborat\u00f3rio' <- gsub("SGS del Peru S.A.C." ,
                                "SGS GEOSOL", ca$'Laborat\u00f3rio')

  ca$'Laborat\u00f3rio' <-
    gsub("SGS GEOSOL LABORAT\u00d3RIOS LTDA." ,
         "SGS GEOSOL",
         ca$'Laborat\u00f3rio')

  # Cria tabela com a relação de boletim e laboratório
  lab_bol <- unique(ca[, c('Boletim', 'Laborat\u00f3rio')])

  # out[[11]] <- df_sc_transf # planilha com sinais -

  # ### Substitui valores <LD por 0.5LD e > LD pelo valor de LD
  # write.table(QAQC_transf, paste0(path2, tolower(classe_am),"_qaqc_prep.csv"),
  #             sep=";", dec="," , row.names = FALSE,  quote = FALSE,
  #             fileEncoding = "latin1" )

  # Lê condições analíticas
  ref =  ca
  ref = unique(ref[, c("analito", "unidades", "MDL")])
  ref$MDL <- as.numeric(gsub(",", ".",  ref$MDL))
    count_decimals = function(x) {
    #length zero input
    if (length(x) == 0) return(numeric())

    # Conta casas decimais
    x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
    x_int = floor(x) %>% abs() %>% nchar()
    x_nchr = x_nchr - 1 - x_int
    x_nchr[x_nchr < 0] = 0

    x_nchr
  }


  df <- ref  %>% dplyr::group_by(analito) %>%
    dplyr::summarize(min = min(MDL))

  df <- dplyr::left_join(df, ref,  by = "analito")

  df$MDL <- df$min
  ref <- df %>% dplyr::select(-"min")
  ref$DIG <- count_decimals(ref$MDL)
  # Lê UCC dos elementos
  ucc <- read.csv2(ref_ucc)
  ref$nome_analito <- paste0(ref$analito,"_", ref$unidades)
  ucc$nome_analito <- paste0(ucc$Elemento,"_", ucc$Unidade)
  ref <- dplyr::left_join(ref, ucc[, c("nome_analito", "UCC")],
                          by = "nome_analito")
  ref <- unique(ref)


  out[[4]] <- df2 # dados transformados pivotados
  out[[7]] <- ref # dados de informação do boletim
  out[[8]] <- lab_bol # dados da relação boletim e laboratório
  out[[1]] <- dpivo # dados analíticos brutos
  out[[6]] <- QAQC_transf # dados de qaqc transformados
  out[[5]] <- QAQC_orig # dados de qaqc bruto
  out[[3]] <- df_bruto_pivo # dados analíticos brutos pivotados
  names(out) <- c(
    "dados brutos",
    "dados transformados",
    "dados brutos pivotados",
    "dados transformados pivotados",
    "dados qaqc bruto",
    "dados qaqc transformados",
    "informa\u00e7\u00e3o do boletim",
    "boletim e laborat\u00f3rio",
    "pesos da prepara\u00e7\u00e3o",
    "informa\u00e7\u00f5es da prepara\u00e7\u00e3o"
  )

  return(out)

}
