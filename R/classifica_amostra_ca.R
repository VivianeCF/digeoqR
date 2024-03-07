
#' Classificação dos dados geoquímicos para cartas de anomalias
#'
#' A ferramenta permite transformar os dados do Rigeo e classificar as amostra
#' pelo método Tucker Inner Fence (TIF) valores acima de 75% são considerados
#' anômalos. Usar a mesma máscara do arquivo de saída da função
#' arruma_dados_rigeo do sedimento de corrente.
#'
#' @param file Arquivo dos dados analiticos brutos
#'
#' @return
#' @export
#'
#' @examples
classifica_amostra_ca <- function(dir_out, file = "sc_tidy.csv"){

  df_new <- read.csv2(file)
  colnames(df_new)[c(2,4:5)] <- c("NLAB", "LONGITUDE", "LATITUDE")
  # colnames(df_new) <- stringr::str_to_title(colnames(df_new))
  df_new <- df_new %>% dplyr::select(-ID)

  # Rotina para classicar os dados----------------------------------------------
  ## Entra as condições do processamento ---------------------------------------

  # Atribui os valores do Lab (método analítico)
  lab <- c("EE", "AA")

  # Função que cria uma tabela vazia para receber dados da classificação
  create_empty_table <- function(num_rows, num_cols) {
    frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
    return(frame)
  }

  # Prepara os dataframes de dados analíticos
  df_list <- transforma_dados_05ld(df_new)
  df_brutos_orig <- df_list[[1]]
  df_brutos_orig <- df_brutos_orig[!is.na(df_brutos_orig$FOLHA),]
  df_brutos <- df_list[[2]]
  df_brutos <- df_brutos[!is.na(df_brutos$FOLHA),]
  data_transf <- df_list[[3]]
  data_transf <- data_transf[!is.na(data_transf$FOLHA),]

  ## Gera planilhas com resultados da classificação-----------------------------
  # para cada método analítico
  for (l in 1:length(lab)) {
    # Define os diretórios da saída
    path <- paste0(dir_out,"Folhas/",  lab[l], "/")
    path2 <- paste0(dir_out,"Classificadas/",  lab[l], "/")
    path3 <- paste0(dir_out, "Dados_areas/",  lab[l], "/")

    # Recorta a base analítica pelo método analítico simplificado (Lab)
    data_brutos <- df_brutos[df_brutos$Lab == lab[l] ,]
    data_brutos <- data_brutos[!is.na(data_brutos$FOLHA),]
    data <- data_transf[data_transf$Lab == lab[l],]
    data <- data[!is.na(data$FOLHA),]

    # Controle para tratar só folhas com mais de uma amostra
    # só tratar dados > 0
    if (nrow(data_brutos) != 0) {
      elementos <-
        c("Au", "Cu", "Pb", "Zn") # nomes dos elementos sem unidade
      unidades <-
        c("ppb", "ppm", "ppm", "ppm") # nomes das unidades de medida

      # Concatena elementos e unidades
      nomes_elementos <- paste0(elementos, "_", unidades)

      # Salva dados brutos e transformados na pasta Dados_area por método
      write.csv2(data, paste0(path3, "dados_transf.csv"), row.names = FALSE)
      write.csv2(data_brutos,
                 paste0(path3, "dados_brutos.csv"),
                 row.names = FALSE)

      # Cria lista das folhas e dos nomes das variáveis de campo
      folhas <- unique(data$FOLHA)
      folhas <- folhas[folhas != ""]
      folhas <- folhas[!is.na(folhas)]
      nomes_campo <- colnames(data[, 1:10])

      # Cria planilhas e shapes com anomalias de cada folha
      n_analises <- 0

      # Rotina para classificar dados por folha e elementos selecionados
      for (j in seq(folhas)) {
        dig <-
          c(2, 2, 2, 2) # número de dígitos dos valores analíticos dos elementos

        # Extrai dados transformados das folhas e elementos
        data_analise <- data[data$FOLHA == folhas[j],
                             c(nomes_campo, toupper(nomes_elementos))]

        # Retira da base linhas sem dados analíticos para os elementos
        data_analise <- data_analise %>%
          dplyr::filter(dplyr::if_any((ncol(
            data_analise
          ) - 3):ncol(data_analise),
          complete.cases))

        # Extrai data frame com dados de campo e amostral
        data_campo <- data_analise[, 1:10]

        # Recorta base analítica da folha e elementos
        data_analise <- data_analise[, toupper(nomes_elementos)]

        # Arruma os nomes simplificado dos analitos
        colnames(data_analise) <- elementos

        # Extrai dados brutos das folhas e elementos
        data_orig <- data_brutos[data_brutos$FOLHA == folhas[j],
                                 c(nomes_campo, toupper(nomes_elementos))]

        # Retira da base original linhas sem dados analíticos
        # para os elementos

        data_orig <- data_orig %>%
          dplyr::filter(dplyr::if_any((ncol(data_orig) - 3):ncol(data_orig),
                                      complete.cases))

        # Recorta base analítica da folha e elementos
        data_orig <- data_orig[, toupper(nomes_elementos)]

        # Conta o número total de amostras que vão para classificação (N)
        n_analises[j] <- nrow(data_analise)

        # Cria vetor com dados de N para os elementos
        n_tot <- rep(nrow(data_orig), length(elementos))

        # Conta o número de amostras com análises validas por elemento
        n_val1 <- sapply(data_orig, function(y)
          sum(is.na(y))) # NA
        n_val2 <- sapply(data_orig, function(x)
          sum(stringr::str_detect(x, '<'))) # <LD
        n_val3 <- sapply(data_orig, function(x)
          sum(stringr::str_detect(x, "ND"))) # ND
        n_val <-
          n_tot - colSums(rbind(n_val1, n_val2, n_val3), na.rm = TRUE)

        # Testa se existem mais de 5 amostras analisadas
        # e processa a classificação
        if (nrow(data_analise) >= 5) {
          lim <- 0
          pref <- 0

          # define tabela com dados de classificação
          classifica <- create_empty_table(nrow(data_analise), 4)
          tab <- create_empty_table(6, 4)

          # Cria tabela de classificação para os elementos
          for (i in 1:4) {
            if (n_val[i] > 0) {
              tab[i] <- round(10 ^ summary(log10(data_analise[, i]))[1:6],
                              dig[i])
            } else{
              tab[i] <- c("", "", "", "", "", "")
            }

            # Controle de único valor válido
            if (length(unique(data_analise[, i])) == 1) {
              lim[i] <- NA
              pref[i] <- ""
              unidades[i]
            }
            else {
              # Controle para 2 valores válidos
              if (length(unique(data_analise[, i])) == 2) {
                lim[i] <-  min(data_analise[, i], na.rm = TRUE)
                pref[i] <- "> "
                unidades[i]
              }

              # Controle para os demais números de valores válidos
              if (length(unique(data_analise[, i])) > 2) {
                lim[i] <- 10 ^ (boxplot.stats(log10(data_analise[, i]))$stats[4])
                pref[i] <- "> "
                unidades[i]
              }
            }

            # Define vetor  dos limites de classes (lim) como numérico
            lim[i] <- as.numeric(lim[i])
            classifica[i] <-
            rgr::cutter(data_analise[, i], c(-Inf, round(lim[i], dig[i]), Inf))
          }

          # Define uma chave ID para a tabela
          ID = seq(1:nrow(data_analise))

          # Calcula o número de coluna dos dados recortados
          n <- ncol(data_analise)
          # Arredonda os limites (não ssei se precisa isso denovo)
          lim[1] <- round(lim[1], 1)
          lim[2:4] <- round(lim[2:4], 1)

          #  Cria vetor de rótulos dos destaques
          rotulo <-
            paste0(elementos,
                   " " ,
                   pref,
                   lim,
                   " ",
                   unidades,
                   " (máximo ",
                   t(tab[6, ]),
                   " ",
                   unidades,
                   " )")

          #  Define nomes das colunas da tabela de classificação
          colnames(classifica) <- colnames(data_analise)
          colnames(tab) <- colnames(data_analise)
          parametros <-
            c("N total",
              "N válid.",
              "Mín.",
              "1o. Qu.",
              "Mediana",
              " Média",
              "3o. Qu.",
              " Máx.")

          # Gera a tabela com parâmetros estatísticos
          tab <- rbind(n_tot, n_val, tab)
          LAB <- rep(lab[l], 8)
          FOLHA <- rep(unique(data_campo$FOLHA), 8)
          tab <- cbind(FOLHA, LAB, parametros, tab)
          tab[, 4:7] <-
            apply(
              tab[, 4:7] ,
              2,
              gsub,
              patt = ".",
              replace = ",",
              fixed = TRUE
            )

          # Transforma os dados da tabela para 1 e 2
          classifica <- classifica - 1
          classifica01 <- signif(classifica, 1)
          LAB <- rep(lab[l], length(elementos))
          FOLHA <- rep(unique(data_campo$FOLHA), length(elementos))

          # Cria tabela com legenda da classificação
          limiares_elementos <-
            data.frame(cbind(FOLHA, LAB, elementos, lim, rotulo))

          # Dá nomes as colunas da legenda
          colnames(limiares_elementos) <-
            c("Folha",
              "Laboratório",
              "Elemento",
              "Limiar",
              "Legenda")

          limiares_elementos <-
            limiares_elementos[!is.na(limiares_elementos$Limiar), ]
          limiares_elementos[, 4:5] <-
            apply(
              limiares_elementos[, 4:5],
              2,
              gsub,
              patt = ".",
              replace = ",",
              fixed = TRUE
            )

          # Substitui o valor 2 (>75%)  pelo nome da coluna
          w <- which(classifica == 2 , arr.ind = TRUE)
          classifica[w] <- paste0(names(classifica)[w[, "col"]], " ")

          # Substitui 1 por vazio
          classifica[classifica == 1] <- ""
          classifica_el <- classifica

          # Função para concatenar strings
          col_concat <- function(data, sep = "") {
            if (!(any(class(data) %in% c("matrix", "data.frame"))))
              stop("\"data\" must be a data.frame (or matrix)", call. = FALSE)
            apply(data, 1, paste0, collapse = sep)
          }
          attr(col_concat, "call") <- "col_concat"

          # Cria campo destaque Elementos anomalos concatenados (Destaques)
          Destaques <- col_concat(classifica, sep = "")

          # Elimina dados com valor "NA"
          Destaques <- gsub("NA", "", Destaques)

          # Define nome das colunas dos elementos classificados
          colnames(classifica_el) <-
            paste0("dest ", colnames(data_analise))

          # Define tabelas de classificação com chave, campo,
          # elementos e destaques
          classifica <-
            cbind(ID, data_campo, classifica_el, Destaques)

          # Define tabelas de classificação com chave, campo e destaques
          classifica2 <- cbind(ID, data_campo, Destaques)

          # Cria planilhas com todas as amostras e só com as que tem destaques
          classifica_destaques <-
            subset(classifica2, classifica$Destaques != "")
          classifica_destaques_ordem <-
            classifica_destaques[order(classifica_destaques$Destaques),
                                 c(2:ncol(classifica_destaques))]

          # Atribui o sistema de coordenadas geográficas da projeção
          # 4674: SIRGAS2000
          r <-  4674

          # Cria shapes com todas as amostras e só com as que tem destaques
          # por folha

           plot_locations <- sf::st_as_sf(
            classifica_destaques_ordem,
            coords = c("LONGITUDE", "LATITUDE"),
            crs = r
          )

          plot_locations_todos <-
            sf::st_as_sf(classifica,
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = r)
          colnames(data_orig) <-
            c("Au_ppb", "Cu_ppm", "Pb_ppm", "Zn_ppm")

          # Salva planilhas csv das classificações por folha
          write.table(
            classifica,
            paste0(
              path,
              "destaque_todas_LAB_",
              lab[l],
              "_FOLHA_",
              folhas[j],
              ".csv"
            ),
            sep = ";",
            dec = "," ,
            row.names = FALSE,
            fileEncoding = "latin1",
            na = ""
          )
          write.table(
            classifica_destaques_ordem,
            paste0(
              path,
              "destaque_LAB_",
              lab[l],
              "_FOLHA_",
              folhas[j],
              ".csv"
            ),
            sep = ";",
            dec = "," ,
            row.names = FALSE,
            fileEncoding = "latin1" ,
            na = ""
          )
          write.table(
            limiares_elementos,
            paste0(
              path,
              "limiares_elementos_LAB_",
              lab[l],
              "_FOLHA_",
              folhas[j],
              ".csv"
            ),
            sep = ";",
            dec = "," ,
            row.names = FALSE,
            fileEncoding = "latin1" ,
            na = ""
          )
          write.table(
            tab,
            paste0(
              path,
              "sumario_estatistico_LAB_",
              lab[l],
              "_FOLHA_",
              folhas[j],
              ".csv"
            ),
            sep = ";",
            dec = "," ,
            row.names = FALSE,
            fileEncoding = "latin1",
            na = ""
          )

          write.table(
            cbind(data_campo , data_orig),
            paste0(
              path,
              "dados_analiticos_LAB_",
              lab[l],
              "_FOLHA_",
              folhas[j],
              ".csv"
            ),
            sep = ";",
            dec = "," ,
            row.names = FALSE ,
            fileEncoding = "latin1",
            na = ""
          )

        } else
          next
      }
      ## Salva os resultados da classificação para cada método analítico---------
      ## Lista nomes de arquivos para ser combinado
      filenames <- paste0(path, list.files(
        path = path,
        pattern = glob2rx("destaque_todas*.csv")
      ))

      ## Combina arquivos listados do diretório
      dataset <- do.call("rbind", lapply(
        filenames,
        FUN = function(files) {
          read.csv2(files, fileEncoding = "latin1")
        }
      ))

      # Salva planilha csv das classificações
      write.table(
        dataset,
        paste0(path2,  "destaque_todas_LAB_", lab[l], ".csv"),
        sep = ";",
        dec = "," ,
        row.names = FALSE,
        na = ""
      )

      ## Lista nomes de arquivos para ser combinado
      filenames <- paste0(path, list.files(
        path = path,
        pattern = glob2rx("destaque_LAB*.csv")
      ))
      ## Combina arquivos listados do diretório
      dataset <- do.call("rbind", lapply(
        filenames,
        FUN = function(files) {
          read.csv2(files)
        }
      ))

      # Salva planilha csv das classificações
      write.csv2(
        dataset,
        paste0(path2,  "destaque_LAB_", lab[l], ".csv"),
        row.names = FALSE,
        na = ""
      )

      ## Lista nomes de arquivos para ser combinado
      filenames <- paste0(path, list.files(
        path = path,
        pattern = glob2rx("limiares_elementos_LAB*.csv")
      ))

      ## Combina arquivos listados do diretório
      dataset <- do.call(plyr::"rbind.fill", lapply(
        filenames,
        FUN = function(files)
        {
          read.csv2(files, fileEncoding = "latin1")
        }
      ))

      dataset <- dplyr::filter(dataset,!is.na(Limiar))

      # Salva planilha csv das classificações
      write.csv2(
        dataset,
        paste0(path2, "legenda_LAB_", lab[l], ".csv"),
        row.names = FALSE,
        na = ""
      )

      ## Lista nomes de arquivos para ser combinado
      filenames <- paste0(path, list.files(
        path = path,
        pattern = glob2rx("sumario_estatistico_LAB*.csv")
      ))

      ## Combina arquivos listados do diretório
      dataset <- do.call(plyr::"rbind.fill", lapply(
        filenames,
        FUN = function(files)
        {
          read.csv2(files,
                    sep = ";", dec = ",")
        }
      ))

      # Salva planilha csv das classificações
      write.csv2(
        dataset,
        paste0(path2, "sumario_LAB_", lab[l], ".csv"),
        row.names = FALSE,
        na = ""
      )

      ## Lista nomes de arquivos para ser combinado
      filenames <- paste0(path, list.files(
        path = path,
        pattern = glob2rx("dados_analiticos*.csv")
      ))

      ## Combina arquivos listados do diretório
      dataset <- do.call("rbind", lapply(
        filenames,
        FUN = function(files)
        {
          read.csv2(files,
                    sep = ";", dec = ",")
        }
      ))

      # Salva planilha csv das classificações
      write.table(
        dataset,
        paste0(path2, "dados_analiticos_LAB_", lab[l], ".csv"),
        sep = ";",
        dec = "," ,
        row.names = FALSE,
        na = ""
      )

      #Gerar shapes
      ##Destaques
      destaques <-
        read.csv2(paste0(path2, "destaque_LAB_", lab[l], ".csv"))
      destaques_st <-
        sf::st_as_sf(destaques,
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs = r)
      sf::st_write(
        destaques_st,
        paste0(path2, "destaque_LAB_", lab[l], ".shp"),
        driver = "ESRI Shapefile",
        delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
      )

      ## Destaques todas
      destaques_todas <-
        read.csv2(paste0(path2, "destaque_todas_LAB_",
                         lab[l], ".csv"))
      destaques_todas_st <-
        sf::st_as_sf(destaques_todas,
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs = r)

      sf::st_write(
        destaques_todas_st,
        paste0(path2, "destaque_todas_LAB_", lab[l],
               ".shp"),
        driver = "ESRI Shapefile",
        delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
      )

      dados_analiticos <-
        read.csv2(paste0(path2, "dados_analiticos_LAB_",
                         lab[l], ".csv"),
                  fileEncoding = "latin1")
      dados_analiticos_st <-
        sf::st_as_sf(
          dados_analiticos,
          coords = c("LONGITUDE", "LATITUDE"),
          crs = r
        )
      sf::st_write(
        dados_analiticos_st,
        paste0(path2, "dados_analiticos_LAB_", lab[l],
               ".shp"),
        driver = "ESRI Shapefile",
        delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
      )

    }

  }

## Salva os dados de entrada do processamento-----------------------------------
  df_brutos <- df_brutos[df_brutos$NLAB %in% data_transf$NLAB, ]
  data_transf <- data_transf[data_transf$FOLHA != "", ]

  data_originais_st <-
    sf::st_as_sf(df_brutos,
                 coords = c("LONGITUDE", "LATITUDE"),
                 crs = r)

  data_originais_st <-
    data_originais_st[data_originais_st$FOLHA != "", ]
  sf::st_write(
    data_originais_st,
    paste0(dir_out, "Dados_areas/TODAS/dados_arrumados_brutos.shp"),
    driver = "ESRI Shapefile",
    delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
  )

  data_transf_st <-
    sf::st_as_sf(data_transf,
                 coords = c("LONGITUDE", "LATITUDE"),
                 crs = r)

  sf::st_write(
    data_transf_st,
    paste0(dir_out, "Dados_areas/TODAS/dados_arrumados_transf.shp"),
    driver = "ESRI Shapefile",
    delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
  )

  write.csv2(df_brutos,
             paste0(dir_out, "Dados_areas/TODAS/dados_arrumados_brutos.csv"),
             row.names = F)

  write.csv2(
    data_transf,
    paste0(dir_out, "Dados_areas/TODAS/dados_arrumados_transf.csv"),
    row.names = F
  )

  # Dados brutos originais
  # Filtra só linhas com amostras
  df_brutos_orig <- df_brutos_orig[df_brutos_orig$NLAB %in% data_transf$NLAB, ]
   write.csv2(
    df_brutos_orig,
    paste0(dir_out, "Dados_areas/TODAS/dados_brutos_originais.csv"),
    row.names = F
  )
   # Cria dados espaciais
   df_brutos_orig_st <-
     sf::st_as_sf(df_brutos_orig,
                  coords = c("LONGITUDE", "LATITUDE"),
                  crs = r)

    # Salva dados espaciais
     sf::st_write(
     df_brutos_orig_st,
     paste0(dir_out, "Dados_areas/TODAS/dados_brutos_orig.shp"),
     driver = "ESRI Shapefile",
     delete_layer = TRUE, layer_options = "ENCODING=UTF-8"
   )


  # Unifica os resultados dos dois laboratórios -------------------------------
   ## Dados Analíticos
   path1 <- paste0(dir_out, "Dados_areas/AA/")
   path2 <- paste0(dir_out,"Dados_areas/EE/")

   ## Dados Classificados
   path3 <- paste0(dir_out, "Classificadas/AA/")
   path4 <- paste0(dir_out,"Classificadas/EE/")

   # Dados destaque
   dest1 <- read.csv2(paste0(path3, "destaque_LAB_AA.csv"),
                      colClasses = "character")
   dest2 <- read.csv2(paste0(path4, "destaque_LAB_EE.csv"),
                      colClasses = "character")
   dest = dplyr::bind_rows(dest1, dest2)
   write.csv2(dest,paste0(dir_out,"Processadas/destaque.csv"), row.names = F)

   # Dados destaque todos
   destt1 <- read.csv2(paste0(path3, "destaque_todas_LAB_AA.csv"),
                       colClasses = "character")
   destt2 <- read.csv2(paste0(path4, "destaque_todas_LAB_EE.csv"),
                       colClasses = "character")
   destt = dplyr::bind_rows(dest1, dest2)
   write.csv2(dest,paste0(dir_out,"Processadas/destaque_todas.csv"), row.names = F)

   # Legenda
   leg1 <- read.csv2(paste0(path3, "legenda_LAB_AA.csv"),
                     colClasses = "character")
   leg2 <- read.csv2(paste0(path4, "legenda_LAB_EE.csv"),
                     colClasses = "character")
   leg = dplyr::bind_rows(leg1, leg2)

   write.csv2(leg,paste0(dir_out,"Processadas/legenda.csv"), row.names = F)

   # Sumario
   sum1 <- read.csv2(paste0(path3, "sumario_LAB_AA.csv"),
                     colClasses = "character")
   sum2 <- read.csv2(paste0(path4, "sumario_LAB_EE.csv"),
                     colClasses = "character")
   sum = dplyr::bind_rows(sum1, sum2)
   write.csv2(sum,paste0(dir_out,"Processadas/sumario.csv"))

   ## Shapes destaques
   path5 <- paste0(dir_out,"Classificadas/AA")
   path6 <- paste0(dir_out,"Classificadas/EE")
   destsp1 <- sf::st_read(path5, "destaque_LAB_AA" )
   destsp2 <- sf::st_read(path6, "destaque_LAB_EE")
   destsp <- do.call(rbind, list(destsp1,destsp2))
   sf::st_write(destsp,paste0(dir_out,"Processadas/destaque.shp"),
                append=FALSE, layer_options = "ENCODING=UTF-8" )

   ## Shapes destaques todas
   desttsp1 <- sf::st_read(path5, "destaque_todas_LAB_AA" )
   desttsp2 <- sf::st_read(path6, "destaque_todas_LAB_EE")
   desttsp <- do.call(rbind, list(desttsp1,desttsp2))
   sf::st_write(desttsp,paste0(dir_out,"Processadas/destaque_todas.shp"),
                append=FALSE, layer_options = "ENCODING=UTF-8" )
}
