#' Recupera dados geoquímicos do Rigeo
#'
#' Prepara os dados disponibilizados pelo SGB-CPRM. Estes dados
#' estão em parte na área de download do GEOSGB e em parte no link dos projetos
#' no RIGEO e compactados na extenção zip. A função abre este arquivos e coloca
#' num diretório temporário e depois salva arquivos csv para cada classe e para
#' todas as classes. É preciso ter o cuidado de só colocar para rodar arquivos
#' semelhantes aos do modelo. Arquivos excel devem xlsx e as máscaras das
#' tabelas de atributos devem ser padronizadas.
#' @param folhas Articulação das folhas. Dica: gerar grid das folhas no QGIS com
#' o plugin DSGTools - Grid Algorithms - Gerar Grid Sistematicamente
#' @param dir_in Diretório onde estão os arqivos zip de prontos do DGM
#' @param dir_out Diretório para gravação das planilhas de dados

#' @return
#' @export
#'
#' @examples
#' #arruma_dados_rigeo()
arruma_dados_rigeo <- function(folhas = "inputs/campo/folhas.shp",
                               dir_in = "inputs/projetos/",
                               dir_out = "outputs/" ) {
# Ler arquivo das áreas--------------------------------------------------------
  folhas_po <- sf::st_read(folhas, quiet = TRUE)
  colnames(folhas_po)[1] <- "layer"

# Definições dos diretórios--------------------------------------------------
  arquivos_rigeo <-
    list.files(dir_in, pattern = "\\.zip$", full.names = TRUE)
  info <-
    tolower(
      c(
        "projeto_amostragem",
        "projeto_publicacao",
        "classe",
        "centro_custo",
        "num_campo",
        "num_Lab",
        "data_visita",
        "Laboratório",
        "abertura",
        "leitura",
        "job"
      )
    )
# Criar variáveis---------------------------------------------------------------
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

# Ler arquivos Zip e gerar listas de dados de cada classe de amostra------------
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
    colnames(campo) <- tolower(colnames(campo))

    x <- unzip(arquivos_rigeo[i], list = TRUE)
    x <- x %>%
      dplyr::filter((stringr::str_detect(Name, 'xlsx')))

    x_org <- x
    x$Name <- iconv(x$Name,  "IBM437",  "UTF-8")
    x <-  x[order(x$Name),]
    from <- sort(list.files(temp, full.names = TRUE, pattern = "xlsx"))
    file.rename(from, paste0(temp, "/", x$Name))
# Filtros-----------------------------------------------------------------------
## Bateia-----------------------------------------------------------------------
    filtered_df_bateia <-
      dplyr::filter(x, grepl("Mineralometria",
                          Name, ignore.case = TRUE), fixed = TRUE)[1, 1]

    filtered_df_bateia <-
      filtered_df_bateia[!is.na(filtered_df_bateia)]

    if (length(filtered_df_bateia) != 0) {
      concentrado_filtro <- filtered_df_bateia
      if (!is.na(filtered_df_bateia)) {
        data_cb <-
          readxl::read_xlsx(
            paste0(temp, "/", concentrado_filtro),
            col_types = "text",
            .name_repair = "minimal"
          )
        colnames(data_cb) <- tolower(colnames(data_cb))
        duplicado <-
          colnames(data_cb)[duplicated(colnames(data_cb))]
        if (length(duplicado) > 0) {
          ind_dup <- grep(duplicado, colnames(data_cb))
        }

        if (length(ind_dup) == 2) {
          df1 <- data_cb[, ind_dup[1]]
          df2 <- data_cb[, ind_dup[2]]
          df <- dplyr::coalesce(df1, df2)
          data_cb <-
            data.frame(data_cb[, c(-ind_dup[1],-ind_dup[2])], df)
          colnames(data_cb)
        }

        observacao <-
          grep("observacao", colnames(data_cb), ignore.case = TRUE)
        observacao2 <-
          grep("observação", colnames(data_cb), ignore.case = TRUE)

        if (length(observacao) > 0) {
          data_cb <- data_cb[,-observacao]
        }
        if (length(observacao2) > 0) {
          data_cb <- data_cb[,-observacao2]
        }

        data_cb_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_cb, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )

        names(data_cb_join) <- toupper(names(data_cb_join))
        names(data_cb_join) <-
          stringr::str_replace(names(data_cb_join), "_PCT", "")

        data_cb_join <- data.frame(data_cb_join)
        m <-
          toupper(colnames(data_cb)[!(colnames(data_cb) %in% info)])
        m <- stringr::str_replace(m, " ", "")
        min[[i]] <-
          data.frame(mineral = stringr::str_replace(m, "_PCT", ""))
        colnames(data_cb_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_cb_join))

        data_cb_join$DATA_VISITA <-
          as.Date(as.character(data_cb_join$DATA_VISITA), "%d/%m/%Y")

        res_cb[[i]] <- data_cb_join
      }
    }
## Sedimento-------------------------------------------------------------------
    filtered_df_sedimento <-
      dplyr::filter(x, grepl("Sedimento de Corrente",
                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_sedimento2 <-
      dplyr::filter(x, grepl("Sedimento_de_Corrente",
                          Name, ignore.case = TRUE))[1, 1]

    filtered_df_sedimento3 <-
      dplyr::filter(x, grepl( "Resultados Analíticos",
                      Name, ignore.case = TRUE))[1, 1]
    filtered_df_sedimento4 <-
      dplyr::filter(x, grepl( "Geoq_Sed",
                      Name, ignore.case = TRUE))[1, 1]
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

        data_sc_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_sc, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )

        names(data_sc_join) <- toupper(names(data_sc_join))
        data_sc_join <- as.data.frame(data_sc_join)
        colnames(data_sc_join) <-
          gsub("SC._PPM", "SC_PPM", colnames(data_sc_join))
  colnames(data_sc_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_sc_join))

        data_sc_join$DATA_VISITA <-
          as.Date(as.character(data_sc_join$DATA_VISITA), "%d/%m/%Y")
        res_sc[[i]] <-  data_sc_join
      }
    }

## Bateia Química--------------------------------------------------------------
    filtered_df_bateia_gq <-
      dplyr::filter(x, grepl("mica Concentrado de Bateia",
                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq2 <-
      dplyr::filter(x, grepl("mica de Concentrado de Bateia",
                       Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq3 <-
      dplyr::filter(x, grepl("mica_Concentrado_de_Bateia",
                       Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq4 <-
      dplyr::filter(x, grepl("mica_de_Concentrado_de_Bateia",
                       Name, ignore.case = TRUE))[1, 1]
    filtered_df_bateia_gq <-
      c(filtered_df_bateia_gq,
        filtered_df_bateia_gq2,
        filtered_df_bateia_gq3,
        filtered_df_bateia_gq4)
    filtered_df_bateia_gq <-
      filtered_df_bateia_gq[!is.na(filtered_df_bateia_gq)]

    if (length(filtered_df_bateia_gq) != 0) {
      concentrado_filtro_gq <- filtered_df_bateia_gq
      if (!is.na(filtered_df_bateia_gq)) {
        data_cb_gq <-
          readxl::read_excel(paste0(temp, "/", concentrado_filtro_gq),
                             col_types = "text")
        colnames(data_cb_gq)[6] <- "num_lab"

        data_cb_gq_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_cb_gq, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )

        data_cb_gq_join <- as.data.frame(data_cb_gq_join)
        names(data_cb_gq_join) <- toupper(names(data_cb_gq_join))
        data_cb_gq_join <- as.data.frame(data_cb_gq_join)
        colnames(data_cb_gq_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_cb_gq_join))

        data_cb_gq_join$DATA_VISITA <-
          as.Date(as.character(data_cb_gq_join$DATA_VISITA), "%d/%m/%Y")

        res_cb_gq[[i]] <-  data_cb_gq_join

      }
    }
## Solo --------------------------------------------------------------------

    filtered_df_solo <-
      dplyr::filter(x, grepl("Geoquimica Solo",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo2 <-
      dplyr::filter(x, grepl("Geoquimica de Solo",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo3 <-
      dplyr::filter(x, grepl("Geoquimica_Solo",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_solo4 <-
      dplyr::filter(x, grepl("Analise_Química_Solo",
                             Name, ignore.case = TRUE))[1, 1]

    filtered_df_solo5 <-
      dplyr::filter(x, grepl("Geoq_Solo",
                             Name, ignore.case = TRUE))[1, 1]

    filtered_df_solo <- c(filtered_df_solo,
                          filtered_df_solo2,
                          filtered_df_solo3,
                          filtered_df_solo4,
                          filtered_df_solo5)
    filtered_df_solo <- filtered_df_solo[!is.na(filtered_df_solo)]


    if (length(filtered_df_solo) != 0) {
      solo_filtro <- filtered_df_solo
      if (!is.na(filtered_df_solo)) {
        data_l <- readxl::read_excel(paste0(temp, "/", solo_filtro),
                                     col_types = "text")
        colnames(data_l) <- tolower(colnames(data_l))
        data_l_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_l, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )
        names(data_l_join) <- toupper(names(data_l_join))
        data_l_join <- as.data.frame(data_l_join)
        colnames(data_l_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_l_join))

        data_l_join$DATA_VISITA <-
          as.Date(as.character(data_l_join$DATA_VISITA), "%d/%m/%Y")


        res_l[[i]] <-  data_l_join
      }
    }

## Rocha ----------------------------------------------------------------------
    filtered_df_rocha <-
      dplyr::filter(x, grepl("mica Rocha",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha2 <-
      dplyr::filter(x, grepl("mica de Rocha",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha3 <-
      dplyr::filter(x, grepl("mica_Rocha",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha4 <-
      dplyr::filter(x, grepl("mica_de_Rocha",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha5 <-
      dplyr::filter(x, grepl("Geoq_Rocha",
                             Name, ignore.case = TRUE))[1, 1]
    filtered_df_rocha <- c(filtered_df_rocha,
                           filtered_df_rocha2,
                           filtered_df_rocha3,
                           filtered_df_rocha4,
                           filtered_df_rocha5)
    filtered_df_rocha <-
      filtered_df_rocha[!is.na(filtered_df_rocha)]

    if (length(filtered_df_rocha) != 0) {
      rocha_filtro <- filtered_df_rocha
      if (!is.na(filtered_df_rocha)) {
        data_r <- readxl::read_excel(paste0(temp, "/", rocha_filtro),
                                     col_types = "text")
        colnames(data_r) <- tolower(colnames(data_r))
        data_r_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_r, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )
        names(data_r_join) <- toupper(names(data_r_join))
        data_r_join <- as.data.frame(data_r_join)
        colnames(data_r_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_r_join))

        data_r_join$DATA_VISITA <-
          as.Date(as.character(data_r_join$DATA_VISITA), "%d/%m/%Y")

        res_r[[i]] <-  data_r_join
      }
    }

## Minério --------------------------------------------------------------------
    filtered_df_minerio <-
      dplyr::filter(x,
                    grepl("mica_Mineral",
                          Name, ignore.case = TRUE))[1, 1]

    filtered_df_minerio2 <-
      dplyr::filter(x,
                    grepl("mica Mineral",
                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_minerio3 <-
      dplyr::filter(x,
                    grepl("mica_de_Mineral",
                          Name, ignore.case = TRUE))[1, 1]
    filtered_df_minerio4 <-
      dplyr::filter(x,
                    grepl("mica de Mineral",
                          Name, ignore.case = TRUE))[1, 1]


    filtered_df_minerio <-
      c(filtered_df_minerio,
        filtered_df_minerio2,
        filtered_df_minerio3,
        filtered_df_minerio4)
    filtered_df_minerio <-
      filtered_df_minerio[!is.na(filtered_df_minerio)]


    if (length(filtered_df_minerio) != 0) {
      minerio_filtro <- filtered_df_minerio
      if (!is.na(filtered_df_minerio)) {
        data_m <- readxl::read_excel(paste0(temp, "/", minerio_filtro),
                                     col_types = "text")
        colnames(data_m) <- tolower(colnames(data_m))
        data_m_join <- dplyr::inner_join(
          campo,
          dplyr::select(data_m, -any_of(names(campo)), num_lab),
          by ="num_lab"
        )
        names(data_m_join) <- toupper(names(data_m_join))
        data_m_join <- as.data.frame(data_m_join)
        colnames(data_m_join) <-
          gsub("DTVISITA", "DATA_VISITA", colnames(data_m_join))

        data_m_join$DATA_VISITA <-
          as.Date(as.character(data_m_join$DATA_VISITA), "%d/%m/%Y")

        res_m[[i]] <-  data_m_join
      }
    }
    print(arquivos_rigeo[i])
    print(i)
  }
# Arruma os dados de cada classe de amostra-------------------------------------
 ## Sedimento de Corrente - Análises químicas-----------------------------------
  if (length(res_sc) > 0) {
    # Tranforma a lista de dados da classe em uma única tabela
    tables_sc <- do.call(plyr::rbind.fill, res_sc)

    # Garante que o campo projeto tenha todos os nomes
    tables_sc[is.na(tables_sc$PROJETO), "PROJETO"] <-
    tables_sc[is.na(tables_sc$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_sc[is.na(tables_sc$PROJETO), "PROJETO"] <-
      tables_sc[is.na(tables_sc$PROJETO), "PROJETO_PUBLICACAO"]
    tables_sc <-
      data.frame(lapply(tables_sc, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    # Grava
    write.csv2(tables_sc, paste0(dir_out, "integrada_rigeo_sc.csv"),
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

    # Recupera os nomes dos elementos da tabela e padroniza tabela
    elem <-
      dplyr::select(estacoes_folhas_sc, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem,!contains("COMPOS"))
    selec <- estacoes_folhas_sc[, selecionadas]
    estacoes_folhas_sc <- data.frame(selec, elem)

    colnames(estacoes_folhas_sc)[3] <- "FOLHA"
    colnames(estacoes_folhas_sc)[7] <- "PROJETO"
    colnames(estacoes_folhas_sc)[6] <- "NCAMP"
    colnames(estacoes_folhas_sc)[2] <- "NLAB"
    # colnames(estacoes_folhas_sc)[4:5] <- c("LONG", "LAT")
    ## Cria campo Base com nome da fonte de dados
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_sc))

    ## Gera dataframe organizado
    estacoes_folhas_sc  <-
      data.frame(estacoes_folhas_sc [, 1:9], BASE,
                 estacoes_folhas_sc [, 10:ncol(estacoes_folhas_sc)])

    ## Salva arquivo final do Rigeo
    write.csv2(
      estacoes_folhas_sc ,
      paste0(dir_out, "sc_tidy.csv"),
      row.names = FALSE,
      na = "",
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
    # Tranforma a lista de dados da classe em uma única tabela
    tables_cb <- do.call(plyr::rbind.fill, res_cb)

    # Garante que o campo projeto tenha todos os nomes
    tables_cb[is.na(tables_cb$PROJETO), "PROJETO"] <-
    tables_cb[is.na(tables_cb$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_cb[is.na(tables_cb$PROJETO), "PROJETO"] <-
      tables_cb[is.na(tables_cb$PROJETO), "PROJETO_PUBLICACAO"]

    # Recupera os nomes dos minerais da tabela
    minerais <- do.call(rbind, min)
    minerais <- sort(unique(minerais$mineral))

    # Padroniza os nomes dos campos
    # Isso pode não ser necessário se a fonte dos dados for padronizada
    ## Nomes dos campos das tabelas
    colnames(tables_cb) <-
      gsub("\u00c1", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00c3", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00c2", "A", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00c9", "E", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00ca", "E", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00cd", "I", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("\u00d3", "O", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("-", "_", colnames(tables_cb), fixed = TRUE)
     colnames(tables_cb) <-
      gsub(".", "_", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <-
      gsub("__", "_", colnames(tables_cb), fixed = TRUE)
    colnames(tables_cb) <- gsub("COL_TAN_", "COL_TAN", colnames(tables_cb),
                                fixed = TRUE)
    colnames(tables_cb) <- gsub("SILLIMANITA", "SILLIMAN", colnames(tables_cb),
                                fixed = TRUE)
    colnames(tables_cb) <- gsub("SILLIMAN", "SILLIMANITA", colnames(tables_cb),
                                fixed = TRUE)
    colnames(tables_cb) <- gsub("N_IDENT_", "N_IDENT", colnames(tables_cb),
                                fixed = TRUE)
    colnames(tables_cb) <- gsub("CENTRO_CUS", "CENTRO_CUSTO",
                                colnames(tables_cb),
                                fixed = TRUE)
    # nomes dos minerais
    minerais <- gsub("\u00c1", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c3", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c2", "A", minerais, fixed = TRUE)
    minerais <- gsub("\u00c9", "E", minerais, fixed = TRUE)
    minerais <- gsub("\u00ca", "E", minerais, fixed = TRUE)
    minerais <- gsub("\u00cd", "I", minerais, fixed = TRUE)
    minerais <- gsub("\u00d3", "O", minerais, fixed = TRUE)
    minerais <- gsub("-", "_", minerais, fixed = TRUE)
    minerais <- gsub(".", "_", minerais, fixed = TRUE)
    minerais <- gsub("_<", "_", minerais, fixed = TRUE)
    minerais <- gsub("__", "_", minerais, fixed = TRUE)
    minerais <- gsub(",", "_", minerais, fixed = TRUE)
    minerais <- gsub("COL_TAN_", "COL_TAN", minerais, fixed = TRUE)
    minerais <- gsub("SILLIMANITA", "SILLIMAN", minerais,
                     fixed = TRUE)
    minerais <- gsub("SILLIMAN", "SILLIMANITA", minerais,
                     fixed = TRUE)
    minerais <- gsub("N_IDENT_", "N_IDENT", minerais,
                     fixed = TRUE)
    minerais <- unique(minerais)

    # Retira campos que não são minerais
    minerais <-
      minerais[!(minerais %in% c("OBSERVACAO", "P_CNC", "P_TOTAL", "P_TOTAL_G"))]
    # minerais <- gsub(" ", "", minerais, fixed = TRUE)
    # # Padroniza e Organiza a tabela de dados da classe
    # tables_cb <-
    #   tables_cb[, c(toupper(intersect(toupper(info), colnames(tables_cb))),
    #                 c("LONGITUDE", "LATITUDE"), minerais)]

    # Cria varável Lab unindo LEITURA e ABERTURA
    tables_cb <- tables_cb %>%
      tidyr::unite("Lab",   LEITURA, ABERTURA, sep = " - ", na.rm = TRUE)

    # colnames(tables_cb) <- gsub(" ", "", colnames(tables_cb), fixed = TRUE)
    # Grava arquivo com todas as amostras da classe
    write.csv2(tables_cb, paste0(dir_out, "integrada_rigeo_cb.csv"),
               row.names = FALSE)

    # Remodela os dados para ficar com campos Mineral e value
    dfp <- tidyr::pivot_longer(
      data = tables_cb,
      cols =  all_of(minerais),
      names_to = "Mineral",
      values_to = "value"
    )

    # Padroniza o campo de valores
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

    # Remodela para dados dos minerais ficar nas colunas
    df_cb_tidy <- dfp %>%
      tidyr::pivot_wider(names_from = Mineral,
                         values_from = value, values_fn = max)

    # Cria uma chave única para cada amostra
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
    # Cria um campo para a fonte dos dados = BASE
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_cb))
    estacoes_folhas_cb <-  data.frame(BASE, estacoes_folhas_cb)

    # Resume tabela para só colunas selecionadas e minerais
    estacoes_folhas_cb <-
      estacoes_folhas_cb[, c(selecionadas, "BASE", minerais)]

    # Padroniza a planilha de dados
    colnames(estacoes_folhas_cb)[3] <- "FOLHA"
    colnames(estacoes_folhas_cb)[7] <- "PROJETO"
    colnames(estacoes_folhas_cb)[6] <- "NCAMP"
    colnames(estacoes_folhas_cb)[2] <- "NLAB"
    # colnames(estacoes_folhas_cb)[4:5] <- c("LONG", "LAT")

    # Grava o arquivo csv com os dados da classe no diretório escolhido
    write.csv2(
      estacoes_folhas_cb ,
      paste0(dir_out, "cb_tidy.csv"),
      row.names = FALSE,
      na = "",
    )
    #Remodela os dados para ter os campos Analito e Valor
    lista_pivo[[2]] <- estacoes_folhas_cb %>%
      tidyr::pivot_longer(
        cols = all_of(minerais),
        names_to = "Analito",
        values_to = "Valor"
      )
  }
  ## Concentrado de Bateia - Análises químicas----------------------------------
  if (length(res_cb_gq) > 0) {
    tables_cb_gq <- do.call(plyr::rbind.fill, res_cb_gq)
    tables_cb_gq[is.na(tables_cb_gq$PROJETO), "PROJETO"] <-
      tables_cb_gq[is.na(tables_cb_gq$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_cb_gq[is.na(tables_cb_gq$PROJETO), "PROJETO"] <-
      tables_cb_gq[is.na(tables_cb_gq$PROJETO), "PROJETO_PUBLICACAO"]

        tables_cb_gq <-
      data.frame(lapply(tables_cb_gq, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_cb_gq,
               paste0(dir_out,
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
    elem <- dplyr::select(elem,!contains("COMPOS"))
    selec <- estacoes_folhas_cb_gq[, selecionadas]
    estacoes_folhas_cb_gq <- data.frame(selec, elem)

    colnames(estacoes_folhas_cb_gq)[3] <- "FOLHA"
    colnames(estacoes_folhas_cb_gq)[7] <- "PROJETO"
    colnames(estacoes_folhas_cb_gq)[6] <- "NCAMP"
    colnames(estacoes_folhas_cb_gq)[2] <- "NLAB"
    # colnames(estacoes_folhas_cb_gq)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_cb_gq))

    ## Gera dataframe organizado
    estacoes_folhas_cb_gq <-
      data.frame(estacoes_folhas_cb_gq [, 1:9], BASE,
                 estacoes_folhas_cb_gq [, 10:ncol(estacoes_folhas_cb_gq)])

    ## Salva arquivo final do Rigeo
    write.csv2(
      estacoes_folhas_cb_gq ,
      paste0(dir_out, "cb_gq_tidy.csv"),
      row.names = FALSE,
      na = "",
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
  ## Rocha - Análises químicas--------------------------------------------------
  if (length(res_r) > 0) {
    tables_r <- do.call(plyr::rbind.fill, res_r)
    tables_r[is.na(tables_r$PROJETO), "PROJETO"] <-
      tables_r[is.na(tables_r$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_r[is.na(tables_r$PROJETO), "PROJETO"] <-
      tables_r[is.na(tables_r$PROJETO), "PROJETO_PUBLICACAO"]

    tables_r <-
      data.frame(lapply(tables_r, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_r,
               paste0(dir_out,
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
    elem <- dplyr::select(elem,!contains("COMPOS"))
    selec <- estacoes_folhas_r[, selecionadas]
    estacoes_folhas_r <- data.frame(selec, elem)

    colnames(estacoes_folhas_r)[3] <- "FOLHA"
    colnames(estacoes_folhas_r)[7] <- "PROJETO"
    colnames(estacoes_folhas_r)[6] <- "NCAMP"
    colnames(estacoes_folhas_r)[2] <- "NLAB"
    # colnames(estacoes_folhas_r)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_r))

    ## Gera dataframe organizado
    estacoes_folhas_r  <-
      data.frame(estacoes_folhas_r [, 1:9], BASE,
                 estacoes_folhas_r [, 10:ncol(estacoes_folhas_r)])

    ## Salva arquivo final do Rigeo
    write.csv2(
      estacoes_folhas_r ,
      paste0(dir_out, "r_tidy.csv"),
      row.names = FALSE,
      na = "",
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
  ## Solo - Análises químicas---------------------------------------------------
  if (length(res_l) > 0) {
    tables_l <- do.call(plyr::rbind.fill, res_l)
    tables_l[is.na(tables_l$PROJETO), "PROJETO"] <-
      tables_l[is.na(tables_l$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_l[is.na(tables_l$PROJETO), "PROJETO"] <-
      tables_l[is.na(tables_l$PROJETO), "PROJETO_PUBLICACAO"]
    tables_l <-
      data.frame(lapply(tables_l, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_l,
               paste0(dir_out,
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
    elem <- dplyr::select(elem,!contains("COMPOS"))
    selec <- estacoes_folhas_l[, selecionadas]
    estacoes_folhas_l <- data.frame(selec, elem)

    colnames(estacoes_folhas_l)[3] <- "FOLHA"
    colnames(estacoes_folhas_l)[7] <- "PROJETO"
    colnames(estacoes_folhas_l)[6] <- "NCAMP"
    colnames(estacoes_folhas_l)[2] <- "NLAB"
    # colnames(estacoes_folhas_l)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_l))

    ## Gera dataframe organizado
    estacoes_folhas_l  <-
      data.frame(estacoes_folhas_l [, 1:9], BASE,
                 estacoes_folhas_l [, 10:ncol(estacoes_folhas_l)])

    ## Salva arquivo final do Rigeo
    write.csv2(
      estacoes_folhas_l ,
      paste0(dir_out, "l_tidy.csv"),
      row.names = FALSE,
      na = "",
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
  ## Minerio - Análises químicas------------------------------------------------
  if (length(res_m) > 0) {
    tables_m <- do.call(plyr::rbind.fill, res_m)
    tables_m[is.na(tables_m$PROJETO), "PROJETO"] <-
      tables_m[is.na(tables_m$PROJETO), "PROJETO_AMOSTRAGEM"]
    tables_m[is.na(tables_m$PROJETO), "PROJETO"] <-
      tables_m[is.na(tables_m$PROJETO), "PROJETO_PUBLICACAO"]
    tables_m <-
      data.frame(lapply(tables_m, function(x)
        gsub(".", ",", x,
             fixed = TRUE)))
    write.csv2(tables_m,
               paste0(dir_out,
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

    estacoes_folhas_m <-
      as.data.frame(sf::st_join(spdf  , left = TRUE,
                                folhas_po["layer"]))
    elem <-
      dplyr::select(estacoes_folhas_m, contains(c("_PPM", "_PCT", "_PPB")))
    elem <- dplyr::select(elem,!contains("COMPOS"))
    selec <- estacoes_folhas_m[, selecionadas]
    estacoes_folhas_m <- data.frame(selec, elem)

    colnames(estacoes_folhas_m)[3] <- "FOLHA"
    colnames(estacoes_folhas_m)[7] <- "PROJETO"
    colnames(estacoes_folhas_m)[6] <- "NCAMP"
    colnames(estacoes_folhas_m)[2] <- "NLAB"
    # colnames(estacoes_folhas_m)[4:5] <- c("LONG", "LAT")
    ## Cria Base
    BASE <- rep("SGB-CPRM - Rigeo", nrow(estacoes_folhas_m))

    ## Gera dataframe organizado
    estacoes_folhas_m  <-
      data.frame(estacoes_folhas_m [, 1:9], BASE,
                 estacoes_folhas_m [, 10:ncol(estacoes_folhas_m)])

    ## Salva arquivo final do Rigeo
    write.csv2(
      estacoes_folhas_m ,
      paste0(dir_out, "m_tidy.csv"),
      row.names = FALSE,
      na = "",
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
  # Une e grava todas bases da lista -------------------------------------------
  unido <- do.call(plyr::rbind.fill, lista_pivo)
  unido <- unido[!is.na(unido$Valor), ]
  # Grava a base integrada com todas classes e projetos
  write.csv2(unido, paste0(dir_out, "toda_base_integral_rigeo.csv"),
             row.names = FALSE)
}
