#' Recupera dados geoquímicos do Rigeo
#'
#' @param folhas Articulação das folhas. Dica: gerar grid das folhas no QGIS com
#' o plugin DSGTools - Grid Algorithms - Gerar Grid Sistematicamente
#' @param dir diretório dos arquivos zip baixados do Rigeo
#'
#' @return
#' @export
#'
#' @examples
#' #arruma_dados_rigeo()
arruma_dados_rigeo <- function(folhas = "inputs/campo/folhas.shp", dir = "inputs/projetos/") {
  #Ler arquivo das áreas--------------------------------------------------------
  folhas_po <- sf::st_read(folhas, quiet = TRUE)
  colnames(folhas_po)[1] <- "layer"

  ## Definições dos diretórios--------------------------------------------------
  arquivos_rigeo <-
    list.files(dir, pattern = "\\.zip$", full.names = TRUE)
  info <- tolower(c("projeto_amostragem","projeto_publicacao","classe","centro_custo","num_campo",
  "num_Lab","data_visita","Laboratório","abertura","leitura","job"))
  ## Criar variáveis------------------------------------------------------------
  res_sc <- list()
  res_cb <- list()
  res_cb_gq <- list()
  res_r <- list()
  res_l <- list()
  res_m <- list()
  coord_cb <- 0
  coord_sc <- 0
  coord_cb_gq <- 0
  coord_r <- 0
  coord_l <- 0
  coord_m <- 0
  min <- list()
  lista_pivo <- list()
  ## Campos para a tabela final
  selecionadas <-
    c(
      "ID",
      "NUM_LAB",
      "layer",
      "LONGITUDE",
      "LATITUDE",
      "NUM_CAMPO",
      "PROJETO",
      "CLASSE",
      "Lab"
    )

  ## Ler arquivos Zip e gerar listas de dados de cada classe de amostra---------
  for (i in 1:length(arquivos_rigeo)) {

    ind_dup <- 0
    observacao <- 0
    observacao2 <- 0
    # Create temp files
    temp <- tempfile()
    zip::unzip(arquivos_rigeo[i], exdir = temp)
    campo <- sf::read_sf(temp)
    coord <- sf::st_coordinates(campo)[, c(1, 2)]
    colnames(coord) <- c("Longitude", "Latitude")
    campo <- data.frame(coord, campo)


    x <- unzip(arquivos_rigeo[i], list = TRUE)
    x <- x %>%
      dplyr::filter((stringr::str_detect(Name, 'xlsx')))
    x_org <- x
    x$Name <- iconv(x$Name,  "IBM437",  "UTF-8")
    from <- list.files(temp, full.names = TRUE, pattern = "xlsx")
    file.rename(from, paste0(temp, "/", x$Name))


    filtered_df_bateia <-
      dplyr::filter(x,
                    grepl("Mineralometria Concentrado de Bateia",
                          Name, ignore.case = TRUE),
                    fixed = TRUE)[1, 1]
    filtered_df_bateia2 <-
      dplyr::filter(x,
                    grepl(
                      "Mineralometria_Concentrado_de_Bateia",
                      Name,
                      ignore.case = TRUE
                    ))[1, 1]
    filtered_df_bateia3 <-
      dplyr::filter(x,
                    grepl(
                      "Mineralometria de Concentrado de Bateia",
                      Name,
                      ignore.case = TRUE
                    ))[1, 1]
    filtered_df_bateia4 <-
      dplyr::filter(x,
                    grepl(
                      "Mineralometria_de_Concentrado_de_Bateia",
                      Name,
                      ignore.case = TRUE
                    ))[1, 1]
    filtered_df_bateia <- c(
      filtered_df_bateia,
      filtered_df_bateia2,
      filtered_df_bateia3,
      filtered_df_bateia4
    )
    filtered_df_bateia <-
      filtered_df_bateia[!is.na(filtered_df_bateia)]

    if (length(filtered_df_bateia) != 0) {
      concentrado_filtro <- filtered_df_bateia
      if (!is.na(filtered_df_bateia)) {
        data_cb <-
          readxl::read_xlsx(paste0(temp, "/", concentrado_filtro),
                             col_types = "text", .name_repair = "minimal")
        colnames(data_cb) <- tolower(colnames(data_cb))
        duplicado <- colnames(data_cb)[duplicated(colnames(data_cb))]
        if(length(duplicado)>0){
          ind_dup <- grep(duplicado, colnames(data_cb))
          }

        if(length(ind_dup) == 2){
        df1 <- data_cb[, ind_dup[1]]
        df2 <- data_cb[, ind_dup[2]]
        df <- dplyr::coalesce(df1,df2)
        data_cb <- data.frame(data_cb[, c(-ind_dup[1], -ind_dup[2])], df)
        colnames(data_cb)
        }

        observacao <- grep("observacao", colnames(data_cb), ignore.case = TRUE)
        observacao2 <- grep("observação", colnames(data_cb), ignore.case = TRUE)

        if(length(observacao)>0){
          data_cb <- data_cb[, -observacao]
        }
        if(length(observacao2)>0){
          data_cb <- data_cb[, -observacao2]
        }
        data_cb_join <-
         as.data.frame( dplyr::inner_join(data_cb, campo, by = c("num_lab" = "Num_Lab")))
        names(data_cb_join) <- toupper(names(data_cb_join))
        names(data_cb_join) <-
          stringr::str_replace(names(data_cb_join), "_PCT", "")

        data_cb_join <- data.frame(data_cb_join)
        m <- toupper(colnames(data_cb)[!(colnames(data_cb) %in% info)])
        min[[i]] <- data.frame(mineral=stringr::str_replace(m, "_PCT", ""))

        res_cb[[i]] <- data_cb_join
      }
    }

    filtered_df_sedimento <-
      dplyr::filter(x,
             grepl("Geoquimica Sedimento de Corrente",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_sedimento2 <-
      dplyr::filter(x,
             grepl("Geoquimica de Sedimento de Corrente",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_sedimento3 <-
      dplyr::filter(x,
             grepl("Geoquimica_Sedimento_de_Corrente",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_sedimento4 <-
      dplyr::filter(x,
             grepl(
               "Analise_Química_Sedimento_de_Corrente",
               Name,
               ignore.case = TRUE
             ))[1, 1]

    filtered_df_sedimento <-
      c(
        filtered_df_sedimento,
        filtered_df_sedimento2,
        filtered_df_sedimento3,
        filtered_df_sedimento4
      )
    filtered_df_sedimento <-
      filtered_df_sedimento[!is.na(filtered_df_sedimento)]


    if (length(filtered_df_sedimento) != 0) {
      sedimento_filtro <-  filtered_df_sedimento
      if (!is.na(filtered_df_sedimento)) {
        data_sc <- readxl::read_excel(paste0(temp, "/", sedimento_filtro),
                                      col_types = "text")
        colnames(data_sc) <- tolower(colnames(data_sc))
        data_sc_join <-
          dplyr::inner_join(data_sc, campo, by = c("num_lab" = "Num_Lab"))
        names(data_sc_join) <- toupper(names(data_sc_join))
        data_sc_join <- as.data.frame(data_sc_join)
        colnames(data_sc_join) <-
          gsub("SC._PPM", "SC_PPM", colnames(data_sc_join))
        res_sc[[i]] <-  data_sc_join
      }
    }

    filtered_df_bateia_gq <-
      dplyr::filter(x,
             grepl("Geoquimica Concentrado de Bateia",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq2 <-
      dplyr::filter(x,
             grepl("Geoquimica_Concentrado_de_Bateia",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq3 <-
      dplyr::filter(x,
             grepl(
               "Analise_Química_Concentrado_de_Bateia",
               Name,
               ignore.case = TRUE
             ))[1, 1]
    filtered_df_bateia_gq <-
      c(filtered_df_bateia_gq,
        filtered_df_bateia_gq2,
        filtered_df_bateia_gq3)
    filtered_df_bateia_gq <-
      filtered_df_bateia_gq[!is.na(filtered_df_bateia_gq)]

    if (length(filtered_df_bateia_gq) != 0) {
      concentrado_filtro_gq <- filtered_df_bateia_gq
      if (!is.na(filtered_df_bateia_gq)) {
        data_cb_gq <-
          readxl::read_excel(paste0(temp, "/", concentrado_filtro_gq),
                             col_types = "text")
        colnames(data_cb_gq)[6] <- "num_lab"
        data_cb_gq_join <- dplyr::inner_join(data_cb_gq, campo,
                                             by = c("num_lab" = "Num_Lab"))
        data_cb_gq_join <- as.data.frame(data_cb_gq_join)
        names(data_cb_gq_join) <- toupper(names(data_cb_gq_join))
        data_cb_gq_join <- as.data.frame(data_cb_gq_join)
        res_cb_gq[[i]] <-  data_cb_gq_join

      }

    }
    filtered_df_solo <- dplyr::filter(x, grepl("Geoquimica Solo",
                                        Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo2 <- dplyr::filter(x, grepl("Geoquimica de Solo",
                                         Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo3 <- dplyr::filter(x, grepl("Geoquimica_Solo",
                                         Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo4 <- dplyr::filter(x, grepl("Analise_Química_Solo",
                                         Name, ignore.case = TRUE))[1, 1]


    filtered_df_solo <- c(filtered_df_solo,
                          filtered_df_solo2,
                          filtered_df_solo3,
                          filtered_df_solo4)
    filtered_df_solo <- filtered_df_solo[!is.na(filtered_df_solo)]


    if (length(filtered_df_solo) != 0) {
      solo_filtro <- filtered_df_solo
      if (!is.na(filtered_df_solo)) {
        data_l <- readxl::read_excel(paste0(temp, "/", solo_filtro),
                                     col_types = "text")
        colnames(data_l) <- tolower(colnames(data_l))
        data_l_join <-
          dplyr::inner_join(data_l, campo, by = c("num_lab" = "Num_Lab"))
        names(data_l_join) <- toupper(names(data_l_join))
        data_l_join <- as.data.frame(data_l_join)
        res_l[[i]] <-  data_l_join
      }
    }


    filtered_df_rocha <- dplyr::filter(x, grepl("Geoquimica Rocha",
                                         Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha2 <- dplyr::filter(x, grepl("Geoquimica de Rocha",
                                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha3 <- dplyr::filter(x, grepl("Geoquimica_Rocha",
                                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha4 <- dplyr::filter(x, grepl("Analise_Química_Rocha",
                                          Name, ignore.case = TRUE))[1, 1]

    filtered_df_rocha <- c(filtered_df_rocha,
                           filtered_df_rocha2,
                           filtered_df_rocha3,
                           filtered_df_rocha4)
    filtered_df_rocha <-
      filtered_df_rocha[!is.na(filtered_df_rocha)]

    if (length(filtered_df_rocha) != 0) {
      rocha_filtro <- filtered_df_rocha
      if (!is.na(filtered_df_rocha)) {
        data_r <- readxl::read_excel(paste0(temp, "/", rocha_filtro),
                                     col_types = "text")
        colnames(data_r) <- tolower(colnames(data_r))
        data_r_join <-
          dplyr::inner_join(data_r, campo, by = c("num_lab" = "Num_Lab"))
        names(data_r_join) <- toupper(names(data_r_join))
        data_r_join <- as.data.frame(data_r_join)
        res_r[[i]] <-  data_r_join
      }
    }


    filtered_df_minerio <-
      dplyr::filter(x,
             grepl("Geoquimica_Mineral_Minerio",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_minerio2 <-
      dplyr::filter(x,
             grepl("Geoquimica de Mineral Minerio",
                   Name, ignore.case = TRUE))[1, 1]
    filtered_df_minerio3 <-
      dplyr::filter(x,
             grepl("Analise_Química_Mineral_Minerio",
                   Name, ignore.case = TRUE))[1, 1]

    filtered_df_minerio <-
      c(filtered_df_minerio,
        filtered_df_minerio2,
        filtered_df_minerio3)
    filtered_df_minerio <-
      filtered_df_minerio[!is.na(filtered_df_minerio)]


    if (length(filtered_df_minerio) != 0) {
      minerio_filtro <- filtered_df_minerio
      if (!is.na(filtered_df_minerio)) {
        data_m <- readxl::read_excel(paste0(temp, "/", minerio_filtro),
                                     col_types = "text")
        colnames(data_m) <- tolower(colnames(data_m))
        data_m_join <-
          dplyr::inner_join(data_m, campo, by = c("num_lab" = "Num_Lab"))
        names(data_m_join) <- toupper(names(data_m_join))
        data_m_join <- as.data.frame(data_m_join)
        res_m[[i]] <-  data_m_join
      }
    }
    print(arquivos_rigeo[i])
    print(i)
  }


  ## Arrumar dados de Sedimento de Corrente - Análises químicas-----------------
  if (length(res_sc) > 0) {
    tables_sc <- do.call(plyr::rbind.fill, res_sc)
    tables_sc <-
      data.frame(lapply(tables_sc, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_sc, "outputs/integrada_rigeo_sc.csv",
               row.names = FALSE)

    #Organizar nomes dos campos para Sedimento de corrente
    tables_sc <- as.data.frame(tables_sc)

    tables_sc <- tables_sc %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)
    ID <- seq(1:nrow(tables_sc))
    tables_sc <- data.frame(ID, tables_sc)

    ## Prepara coordenadas
    tables_sc$LONGITUDE <-
      as.numeric(gsub(",", ".", tables_sc$LONGITUDE,
                      fixed = TRUE))
    tables_sc$LATITUDE <-
      as.numeric(gsub(",", ".", tables_sc$LATITUDE,
                      fixed = TRUE))
    tables_sc[is.na(tables_sc$LATITUDE), "LATITUDE"] <-
      tables_sc[is.na(tables_sc$LATITUDE), "LAT"]
    tables_sc[is.na(tables_sc$LONGITUDE), "LONGITUDE"] <-
      tables_sc[is.na(tables_sc$LONGITUDE), "LONG"]
    ## Extrai códigos das folhas das estações (wgs84)
    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- tables_sc[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = tables_sc,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)
    estacoes_folhas_sc <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))

    elem <-
      dplyr::select(estacoes_folhas_sc, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem, !contains("COMPOS"))
    selec <- estacoes_folhas_sc[, selecionadas]
    estacoes_folhas_sc <- data.frame(selec, elem)

    colnames(estacoes_folhas_sc)[3] <- "FOLHA"
    colnames(estacoes_folhas_sc)[7] <- "PROJETO"
    colnames(estacoes_folhas_sc)[2] <- "N_LAB"
    colnames(estacoes_folhas_sc)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_sc))

    ## Gera dataframe organizado
    estacoes_folhas_sc  <-
      data.frame(estacoes_folhas_sc [, 1:9], BASE,
                 estacoes_folhas_sc [, 10:ncol(estacoes_folhas_sc)])

    ## Salva arquivo final do Rigeo
    write.table(
      estacoes_folhas_sc ,
      "outputs/sc_tidy.csv",
      row.names = FALSE,
      na = "",
      sep = ";",
      dec = ".",
      quote = TRUE
    )
    colnames(estacoes_folhas_sc)
    #Pivoteia e une as bases
    lista_pivo[[1]] <-
      tidyr::pivot_longer(
        estacoes_folhas_sc,
        cols = 11:ncol(estacoes_folhas_sc),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Arrumar dados de Concentrado de Bateia - Mineralometria--------------------
  if (length(res_cb) > 0) {

    tables_cb <- do.call(plyr::rbind.fill, res_cb)
    minerais <- do.call(rbind, min)
    minerais <- sort(unique(minerais$mineral))

    # Isso pode não ser necessário se adotarmos nomenclatura mineral padronizada
    colnames(tables_cb) <- gsub("\u00c1", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00c3", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00c2", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00c9", "E", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00ca", "E", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00cd", "I", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("\u00d3", "O", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("-", "_", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub(".", "_", colnames(tables_cb), fixed = TRUE)

    # Isso pode não ser necessário se adotarmos nomenclatura mineral padronizada
    minerais <- gsub("\u00c1", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c3", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c2", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c9", "E", minerais, fixed = TRUE)
    minerais <- gsub("\u00ca", "E", minerais, fixed = TRUE)
    minerais <- gsub("\u00cd", "I", minerais, fixed = TRUE)
    minerais <- gsub("\u00d3", "O", minerais, fixed = TRUE)
    minerais <- gsub("...31", "", minerais, fixed = TRUE)
    minerais <- gsub("...32", "", minerais, fixed = TRUE)
    minerais <- gsub("COL_TAN_", "COL_TAN", minerais, fixed = TRUE)
    minerais <- gsub("-", "_", minerais, fixed = TRUE)
    minerais <- gsub(".", "_", minerais, fixed = TRUE)
    minerais <- unique(minerais)
    minerais <- minerais[!(minerais %in% c("OBSERVACAO", "P_CNC", "P_TOTAL"))]
    # info

    tables_cb <- tables_cb[, c(toupper(info[-8]),c("LONGITUDE", "LATITUDE"), minerais)]

    tables_cb <- tables_cb %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)

# colnames(tables_cb) <- gsub(".", "_",colnames(tables_cb), fixed = TRUE)
    write.csv2(tables_cb, "outputs/integrada_rigeo_cb.csv", row.names = FALSE)


    colnames(tables_cb)
    dfp <- tidyr::pivot_longer(
      data = tables_cb,
      cols =  minerais,
      names_to = "Mineral",
      values_to = "value"
    )

    dfp$value <- gsub(" ", "", dfp$value)
    dfp$value <- gsub("%", "", dfp$value)
    dfp$value <- gsub("%", "", dfp$value)

    dfp$value <- stringr::str_replace(dfp$value, "5-50", "5 - 50 %")
    dfp$value <- stringr::str_replace(dfp$value, "<1", "< 1 %")
    dfp$value <-
      stringr::str_replace(dfp$value, "75-100", "75 - 100 %")
    dfp$value <- stringr::str_replace(dfp$value, "5-25", "5 - 25 %")
    dfp$value <- stringr::str_replace(dfp$value, "1-5", "1 - 5 %")
    dfp$value <-
      stringr::str_replace(dfp$value, "50-75", "50 - 75 %")
    dfp$value <- stringr::str_replace(dfp$value, "<5", "< 5 %")
    dfp$value <- stringr::str_replace(dfp$value, ">50", "> 50 %")
    dfp$value <- stringr::str_replace(dfp$value, ">75", "> 75 %")
    dfp$value <- stringr::str_replace(dfp$value, "\\.", ",")

    unique(dfp$value)
    # Pivotar para minerais nas colunas
    dados_wilder <- dfp %>%
      tidyr::pivot_wider(names_from = Mineral,
                         values_from = value)
    names(dados_wilder)
    #unificar valores de minerais duplicados na base
    ## não tem

    df_cb_tidy <- dados_wilder
    colnames(df_cb_tidy)

    ID <- seq(1, nrow(df_cb_tidy))
    df_cb_tidy <- data.frame(ID, df_cb_tidy)

    ## Prepara coordenadas
    df_cb_tidy$LONGITUDE <-
      as.numeric(gsub(",", ".", df_cb_tidy$LONGITUDE,
                      fixed = TRUE))
    df_cb_tidy$LATITUDE <-
      as.numeric(gsub(",", ".", df_cb_tidy$LATITUDE,
                      fixed = TRUE))

    ## Extrai códigos das folhas das estações (wgs84)

    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- df_cb_tidy[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = df_cb_tidy,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)
    estacoes_folhas_cb <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))
selecionadas

    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_cb))
estacoes_folhas_cb <-  data.frame(BASE, estacoes_folhas_cb)
colnames(estacoes_folhas_cb)[3] <- "PROJETO"


estacoes_folhas_cb <- estacoes_folhas_cb[, c(selecionadas, minerais)]
    colnames(estacoes_folhas_cb)[3] <- "FOLHA"
    colnames(estacoes_folhas_cb)[2] <- "N_LAB"
    colnames(estacoes_folhas_cb)[4:5] <- c("LONG", "LAT")

    write.table(
      estacoes_folhas_cb ,
      "outputs/cb_tidy.csv",
      row.names = FALSE,
      na = "",
      dec = ",",
      sep = ";"
    )
    #Pivoteia e une as base
    lista_pivo[[2]] <-
      tidyr::pivot_longer(
        estacoes_folhas_cb,
        cols = minerais,
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Arrumar dados de Concentrado de Bateia - Análises químicas-----------------
  if (length(res_cb_gq) > 0) {
    tables_cb_gq <- do.call(plyr::rbind.fill, res_cb_gq)
    tables_cb_gq <-
      data.frame(lapply(tables_cb_gq, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_cb_gq,
               paste0("outputs/",
                      "integrada_rigeo_cb_gq.csv"),
               row.names = FALSE)

    tables_cb_gq <- as.data.frame(tables_cb_gq)
    tables_cb_gq <- tables_cb_gq %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)
    ID <- seq(1:nrow(tables_cb_gq))
    tables_cb_gq <- data.frame(ID, tables_cb_gq)

    ## Prepara coordenadas
    tables_cb_gq$LONGITUDE <-
      as.numeric(gsub(",", ".", tables_cb_gq$LONGITUDE,
                      fixed = TRUE))
    tables_cb_gq$LATITUDE <-
      as.numeric(gsub(",", ".", tables_cb_gq$LATITUDE,
                      fixed = TRUE))
    tables_cb_gq[is.na(tables_cb_gq$LATITUDE), "LATITUDE"] <-
      tables_cb_gq[is.na(tables_cb_gq$LATITUDE), "LAT"]
    tables_cb_gq[is.na(tables_cb_gq$LONGITUDE), "LONGITUDE"] <-
      tables_cb_gq[is.na(tables_cb_gq$LONGITUDE), "LONG"]
    ## Extrai códigos das folhas das estações (wgs84)
    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- tables_cb_gq[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = tables_cb_gq,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)
    estacoes_folhas_cb_gq <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))

    elem <-
      dplyr::select(estacoes_folhas_cb_gq, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem, !contains("COMPOS"))
    selec <- estacoes_folhas_cb_gq[, selecionadas]
    estacoes_folhas_cb_gq <- data.frame(selec, elem)

    colnames(estacoes_folhas_cb_gq)[3] <- "FOLHA"
    colnames(estacoes_folhas_cb_gq)[7] <- "PROJETO"
    colnames(estacoes_folhas_cb_gq)[2] <- "N_LAB"
    colnames(estacoes_folhas_cb_gq)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_cb_gq))

    ## Gera dataframe organizado
    estacoes_folhas_cb_gq <-
      data.frame(estacoes_folhas_cb_gq [, 1:9], BASE,
                 estacoes_folhas_cb_gq [, 10:ncol(estacoes_folhas_cb_gq)])

    ## Salva arquivo final do Rigeo
    write.table(
      estacoes_folhas_cb_gq ,
      "outputs/cb_gq_tidy.csv",
      row.names = FALSE,
      na = "",
      sep = ";",
      dec = ".",
      quote = TRUE
    )
    #Pivoteia e une as bases
    lista_pivo[[3]] <-
      tidyr::pivot_longer(
        estacoes_folhas_cb_gq,
        cols = 11:ncol(estacoes_folhas_cb_gq),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Arrumar dados de Rocha - Análises químicas---------------------------------
  if (length(res_r) > 0) {
    tables_r <- do.call(plyr::rbind.fill, res_r)
    tables_r <-
      data.frame(lapply(tables_r, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_r,
               paste0("outputs/",
                      "integrada_rigeo_r.csv"),
               row.names = FALSE)

    tables_r <- as.data.frame(tables_r)

    tables_r <- tables_r %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)
    ID <- seq(1:nrow(tables_r))
    tables_r <- data.frame(ID, tables_r)

    ## Prepara coordenadas
    tables_r$LONGITUDE <-
      as.numeric(gsub(",", ".", tables_r$LONGITUDE,
                      fixed = TRUE))
    tables_r$LATITUDE <-
      as.numeric(gsub(",", ".", tables_r$LATITUDE,
                      fixed = TRUE))
    tables_r[is.na(tables_r$LATITUDE), "LATITUDE"] <-
      tables_r[is.na(tables_r$LATITUDE), "LAT"]
    tables_r[is.na(tables_r$LONGITUDE), "LONGITUDE"] <-
      tables_r[is.na(tables_r$LONGITUDE), "LONG"]
    ## Extrai códigos das folhas das estações (wgs84)
    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- tables_r[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = tables_r,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)

    estacoes_folhas_r <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))
    elem <-
      dplyr::select(estacoes_folhas_r, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem, !contains("COMPOS"))
    selec <- estacoes_folhas_r[, selecionadas]
    estacoes_folhas_r <- data.frame(selec, elem)

    colnames(estacoes_folhas_r)[3] <- "FOLHA"
    colnames(estacoes_folhas_r)[7] <- "PROJETO"
    colnames(estacoes_folhas_r)[2] <- "N_LAB"
    colnames(estacoes_folhas_r)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_r))

    ## Gera dataframe organizado
    estacoes_folhas_r  <-
      data.frame(estacoes_folhas_r [, 1:9], BASE,
                 estacoes_folhas_r [, 10:ncol(estacoes_folhas_r)])

    ## Salva arquivo final do Rigeo
    write.table(
      estacoes_folhas_r ,
      "outputs/r_tidy.csv",
      row.names = FALSE,
      na = "",
      sep = ";",
      dec = ".",
      quote = TRUE
    )
    #Pivoteia e une as base
    lista_pivo[[4]] <-
      tidyr::pivot_longer(
        estacoes_folhas_r,
        cols = 11:ncol(estacoes_folhas_r),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Arrumar dados de Solo - Análises químicas----------------------------------
  if (length(res_l) > 0) {
    tables_l <- do.call(plyr::rbind.fill, res_l)
    tables_l <-
      data.frame(lapply(tables_l, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_l,
               paste0("outputs/",
                      "integrada_rigeo_l.csv"),
               row.names = FALSE)

    tables_l <- as.data.frame(tables_l)
    tables_l <- tables_l %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)
    ID <- seq(1:nrow(tables_l))
    tables_l <- data.frame(ID, tables_l)

    ## Prepara coordenadas
    tables_l$LONGITUDE <-
      as.numeric(gsub(",", ".", tables_l$LONGITUDE,
                      fixed = TRUE))
    tables_l$LATITUDE <-
      as.numeric(gsub(",", ".", tables_l$LATITUDE,
                      fixed = TRUE))
    tables_l[is.na(tables_l$LATITUDE), "LATITUDE"] <-
      tables_l[is.na(tables_l$LATITUDE), "LAT"]
    tables_l[is.na(tables_l$LONGITUDE), "LONGITUDE"] <-
      tables_l[is.na(tables_l$LONGITUDE), "LONG"]
    ## Extrai códigos das folhas das estações (wgs84)
    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- tables_l[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = tables_l,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)

    estacoes_folhas_l <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))
    elem <-
      dplyr::select(estacoes_folhas_l, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem, !contains("COMPOS"))
    selec <- estacoes_folhas_l[, selecionadas]
    estacoes_folhas_l <- data.frame(selec, elem)

    colnames(estacoes_folhas_l)[3] <- "FOLHA"
    colnames(estacoes_folhas_l)[7] <- "PROJETO"
    colnames(estacoes_folhas_l)[2] <- "N_LAB"
    colnames(estacoes_folhas_l)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_l))

    ## Gera dataframe organizado
    estacoes_folhas_l  <-
      data.frame(estacoes_folhas_l [, 1:9], BASE,
                 estacoes_folhas_l [, 10:ncol(estacoes_folhas_l)])

    ## Salva arquivo final do Rigeo
    write.table(
      estacoes_folhas_l ,
      "outputs/l_tidy.csv",
      row.names = FALSE,
      na = "",
      sep = ";",
      dec = ".",
      quote = TRUE
    )
    #Pivoteia e une as bases
    lista_pivo[[5]] <-
      tidyr::pivot_longer(
        estacoes_folhas_l,
        cols = 11:ncol(estacoes_folhas_l),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Arrumar dados de  Minerio - Análises químicas------------------------------
  if (length(res_m) > 0) {
    tables_m <- do.call(plyr::rbind.fill, res_m)
    tables_m <-
      data.frame(lapply(tables_m, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_m,
               paste0("outputs/",
                      "integrada_rigeo_m.csv"),
               row.names = FALSE)

    tables_m <- as.data.frame(tables_m)
colnames(tables_m)
tables_m <- tables_m %>%
  tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)
    ID <- seq(1:nrow(tables_m))
    tables_m <- data.frame(ID, tables_m)

    ## Prepara coordenadas
    tables_m$LONGITUDE <-
      as.numeric(gsub(",", ".", tables_m$LONGITUDE,
                      fixed = TRUE))
    tables_m$LATITUDE <-
      as.numeric(gsub(",", ".", tables_m$LATITUDE,
                      fixed = TRUE))
    tables_m[is.na(tables_m$LATITUDE), "LATITUDE"] <-
      tables_m[is.na(tables_m$LATITUDE), "LAT"]
    tables_m[is.na(tables_m$LONGITUDE), "LONGITUDE"] <-
      tables_m[is.na(tables_m$LONGITUDE), "LONG"]
    ## Extrai códigos das folhas das estações (wgs84)
    crs_SIRGAS2000 <-
      "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    xy <- tables_m[, c("LONGITUDE", "LATITUDE")]
    spdf <- sf::st_as_sf(
      x = tables_m,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
      remove = FALSE
    )
    colnames(spdf)
    spdf <- sf::st_transform(spdf, crs = crs_SIRGAS2000)
    folhas_po <- sf::st_transform(folhas_po, crs = crs_SIRGAS2000)

    estacoes_folhas_m <- as.data.frame(sf::st_join(spdf  , left = TRUE,
                                     folhas_po["layer"]))
    elem <-
      dplyr::select(estacoes_folhas_m, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem, !contains("COMPOS"))
    selec <- estacoes_folhas_m[, selecionadas]
    estacoes_folhas_m <- data.frame(selec, elem)

    colnames(estacoes_folhas_m)[3] <- "FOLHA"
    colnames(estacoes_folhas_m)[7] <- "PROJETO"
    colnames(estacoes_folhas_m)[2] <- "N_LAB"
    colnames(estacoes_folhas_m)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_m))

    ## Gera dataframe organizado
    estacoes_folhas_m  <-
      data.frame(estacoes_folhas_m [, 1:9], BASE,
                 estacoes_folhas_m [, 10:ncol(estacoes_folhas_m)])


    ## Salva arquivo final do Rigeo
    write.table(
      estacoes_folhas_m ,
      "outputs/m_tidy.csv",
      row.names = FALSE,
      na = "",
      sep = ";",
      dec = ".",
      quote = TRUE
    )
    #Pivoteia e une as base

    lista_pivo[[6]] <-
      tidyr::pivot_longer(
        estacoes_folhas_m,
        cols = 11:ncol(estacoes_folhas_m),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Une as bases --------------------------------------------------------------
  unido <- do.call(plyr::rbind.fill, lista_pivo)
  unido <- unido[!is.na(unido$Valor),]

  write.csv2(unido, "outputs/toda_base_integral_rigeo.csv", row.names = FALSE)
}
