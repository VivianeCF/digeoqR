
#' Title
#'
#' @param file Arquivo dos dados analiticos brutos
#'
#' @return
#' @export
#'
#' @examples
classifica_amostra <- function(file = "outputs/sc_tidy.csv"){
  df_new <- read.csv2(file)
df_new$LONG <- as.numeric(df_new$LONG )
df_new$LAT <- as.numeric(df_new$LAT )
  #
colnames(df_new) <- stringr::str_to_title(colnames(df_new))
df_new <- df_new %>% dplyr::select(-Id)
#
#
# ### Rotina para classicar os dados----------------------------------------------
# # ## Entrar as condições do processamento
lab <- c("EE", "AA")
#
# ## Função que cria uma tabela vazia para receber dados da classificação
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}
#
#
# # Prepara os dataframes de dados analíticos
df_list <- transforma_dados_05ld(df_new)
data_transf <- df_list[[1]]
df_brutos <- df_list[[3]]
#
for(l in 1:length(lab)) {
# Define os diretórios da saída
  path <- paste0("outputs/Folhas/",  lab[l], "/")
  path2 <- paste0("outputs/Classificadas/",  lab[l], "/")
  path3 <- paste0("outputs/Dados_areas/",  lab[l], "/" )

#  # Recorta a base analítica pelo método analítico simplificado (Lab)
  data_brutos <- df_brutos[df_brutos$Lab == lab[l] , ]
  data <- data_transf[data_transf$Lab == lab[l], ]
#
#   ## Controle lab
  if(nrow(data_brutos) != 0){ # só tratar dados > 0



    elementos <-  c("Au", "Cu", "Pb", "Zn") # nomes dos elementos sem unidade
    unidades <- c("ppb", "ppm", "ppm", "ppm") # nomes das unidades de medida

    # concatena elementos e unidades
    nomes_elementos <- paste0(elementos, "_", unidades)

# Salva dados brutos e transformados na pasta Dados_area por método
    write.csv2(data, paste0(path3,"dados_transf.csv"), row.names = FALSE )
    write.csv2(data_brutos, paste0(path3,"dados_brutos.csv"), row.names = FALSE )

# Cria lista das folhas e dos nomes das variáveis de campo
    folhas <- unique(data$Folha)
    folhas <- folhas[folhas != ""]
    nomes_campo <- colnames(data[,1:10])

  # Cria planilhas e shapes com anomalias de cada folha
    n_analises <- 0

  # Rotina para classificar dados por folha e elementos selecionados
    for (j in seq(folhas)) {
      dig <- c(2,2,2,2) # número de dígitos dos valores analíticos dos elementos

      # Extrai dados transformados das folhas e elementos
      data_analise <- data[data$Folha == folhas[j],
                           c(nomes_campo, nomes_elementos)]

      # Retira da base linhas sem dados analíticos para os elementos selecionados
        data_analise <- data_analise %>%
        dplyr::filter(dplyr::if_any((ncol(data_analise)-3):ncol(data_analise),
                                    complete.cases))

      # Extrai data frame com dados de campo e amostral
      data_campo <- data_analise[, 1:10]

      # Recorta base analítica da folha e elementos de interesse
      data_analise <- data_analise[, nomes_elementos]

      # Arruma os nomes simplificado dos analitos
      colnames(data_analise) <- elementos

      # Extrai dados brutos das folhas e elementos
      data_orig <- data_brutos[data_brutos$Folha == folhas[j],
                                 c(nomes_campo, nomes_elementos)]

      # Substitui valores vazios por NA

      # Retira da base original linhas sem dados analíticos
      # para os elementos selecionados

      data_orig <- data_orig %>%
        dplyr::filter(dplyr::if_any((ncol(data_orig)-3):ncol(data_orig),
                                    complete.cases))

      # Recorta base analítica da folha e elementos de interesse
      data_orig <- data_orig[, nomes_elementos]

      # Conta o número total de amostras que vão para classificação (N)
      n_analises[j] <- nrow(data_analise)
      # Cria vetor com dados de N para os elementos
      n_tot <- rep(nrow(data_orig), length(elementos))
      # Conta o número de amostras com análises validas por elemento
      n_val1 <-sapply(data_orig, function(y) sum(is.na(y))) # NA
      n_val2 <- sapply(data_orig, function(x) sum(stringr::str_detect(x, '<'))) # <LD
      n_val3 <- sapply(data_orig, function(x) sum(stringr::str_detect(x, 'ND'))) # ND
      n_val <- n_tot-colSums(rbind(n_val1,n_val2,n_val3), na.rm=TRUE)

      if(nrow(data_analise) >= 5){

        lim <- 0
        pref <- 0

        # define tabela com dados de classificação
        classifica <- create_empty_table(nrow(data_analise), 4)
        tab <- create_empty_table(6, 4)
        # Cria tabela de classificação para os elementos
        for(i in 1:4){
          if(n_val[i] > 0){
            tab[i] <- round(10^summary(log10(data_analise[,i]))[1:6], dig[i] )
          } else{
            tab[i] <- c("","","","", "", "")
            }
          # Controle de único valor válido
          if (length(unique(data_analise[,i])) == 1) {
            lim[i] <- NA
            pref[i] <- ""
            unidades[i]}
          else {
            # Controle para 2 valores válidos
            if(length(unique(data_analise[,i])) < 3) {
              lim[i] <-  min(data_analise[,i], na.rm = TRUE)
              pref[i] <- "> "
              unidades[i]}
            # Controle para os demais números de valores válidos
            if(length(unique(data_analise[,i])) >= 3 ) {
              lim[i] <- 10^(boxplot.stats(log10(data_analise[,i]))$stats[4])
              pref[i] <- "> "
              unidades[i]}
          }

                    # Define vetor  dos limites de classes (lim) como numérico
          lim[i] <- as.numeric(lim[i])
          classifica[i] <- rgr::cutter(data_analise[,i], c(-Inf, round(lim[i], dig[i]), Inf))
        }

        # Define uma chave ID para a tabela
        ID = seq(1:nrow(data_analise))

        # Calcula o número de coluna dos dados recortados
        n <- ncol(data_analise)
        # Arredonda os limites (não ssei se precisa isso denovo)
        lim[1] <- round(lim[1], 1)
        lim[2:4] <- round(lim[2:4], 1)
        #  Cria vetor de rótulos dos destaques


        rotulo <- paste0(elementos, " " , pref, lim, " ", unidades, " (máximo ",t(tab[6,]),  " ", unidades, " )")

        #  Define nomes das colunas da tabela de classificação
        colnames(classifica) <- colnames(data_analise)
        colnames(tab) <- colnames(data_analise)
        parametros <- c( "N total","N válid.","Mín.", "1o. Qu.",  "Mediana",   " Média", "3o. Qu.",   " Máx." )
        # tab <- tab[colSums(!is.na(tab)) > 0]

        # tab <- tab[c(TRUE, lapply(tab[-1], var, na.rm = TRUE) != 0)]
        # tab <- round(tab, dig)
        tab <- rbind(n_tot, n_val, tab)
        LAB <- rep(lab[l], 8)

        FOLHA <- rep(unique(data_campo$Folha),8)
        tab <- cbind(FOLHA,LAB,parametros, tab)
        tab[,4:7] <- apply(tab[,4:7] , 2, gsub, patt=".", replace=",", fixed=TRUE)

        #tab <-tab[,colSums(tab == 0) == 0]
        # Transforma os dados da tabela para 1 e 2
        classifica <- classifica - 1
        classifica01 <- signif(classifica, 1)
        LAB <- rep(lab[l], length(elementos))
        FOLHA <- rep(unique(data_campo$Folha),length(elementos))
        # Cria tabela com resultados da classificação organizados
        limiares_elementos <- data.frame(cbind(FOLHA, LAB, elementos,lim, rotulo))
         # Dá nomes as colunas da classifixação
        colnames(limiares_elementos) <- c("Folha", "Laboratório", "Elemento", "Limiar", "Legenda")

        limiares_elementos <- limiares_elementos[!is.na(limiares_elementos$Limiar),]
        limiares_elementos[,4:5] <- apply(limiares_elementos[,4:5], 2, gsub, patt=".", replace=",", fixed=TRUE)


        # Elimina linhas vazias
        # limiares_elementos <- limiares_elementos[!(rowSums(is.na(limiares_elementos))),]
        # Substitui o valor 2 (>75%)  pelo nome da coluna

        w <- which(classifica == 2 ,arr.ind=TRUE)
        classifica[w] <- paste0(names(classifica)[w[,"col"]]," ")
        # Substitui 1 por vazio
        classifica[classifica == 1] <- ""
        classifica_el <- classifica
        # Função para concatenar strings
        col_concat <- function(data, sep=""){
          if(!(any(class(data) %in% c("matrix", "data.frame"))))
            stop("\"data\" must be a data.frame (or matrix)", call.=FALSE)
          apply(data, 1, paste0, collapse=sep)
        }
        attr(col_concat, "call") <- "col_concat"
        # Cria campo destaque Elementos anomalos concatenados (Destaques)
        Destaques <- col_concat(classifica, sep = "")
        # Elimina dados com valor "NA"
        Destaques <- gsub("NA", "", Destaques)
        # Define nome das colunas dos elementos classificados
        colnames(classifica_el) <- paste0("dest ", colnames(data_analise))
        # Define tabelas de classificação com chave, campo, elementos e destaques
        classifica <- cbind(ID, data_campo, classifica_el, Destaques )
        # Define tabelas de classificação com chave, campo e destaques
        classifica2 <- cbind(ID, data_campo, Destaques )
        # Cria planilhas com todas as amostras e só com as que tem destaques
        classifica_destaques <- subset(classifica2, classifica$Destaques != "")
        classifica_destaques_ordem <- classifica_destaques[order(classifica_destaques$Destaques), c(2:ncol(classifica_destaques))]
        # Atribui o sistema de coordenadas geográficas da projeção
        # Opção 1: WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS:
        # r <- 4326
        # Opção 2: SIRGAS2000:
        r <-  4674

        # Cria shapes com todas as amostras e só com as que tem destaques por folha
        plot_locations <- sf::st_as_sf(classifica_destaques_ordem,
                                   coords = c("Long", "Lat"), crs = r)
        plot_locations_todos <- sf::st_as_sf(classifica, coords = c("Long", "Lat"), crs = r)
        colnames(data_orig) <- c("Au_ppb", "Cu_ppm", "Pb_ppm", "Zn_ppm")

        # Salva planilhas csv das classificações por folha
        write.table(classifica, paste0(path,  "destaque_todas_LAB_", lab[l], "_FOLHA_",
                                       folhas[j],".csv"), sep=";", dec="," , row.names = FALSE, fileEncoding = "latin1", na = "")
        write.table(classifica_destaques_ordem, paste0(path, "destaque_LAB_", lab[l],
                                                       "_FOLHA_",folhas[j],".csv"), sep=";",
                    dec="," , row.names = FALSE, fileEncoding = "latin1" , na = "")
        write.table(limiares_elementos, paste0(path, "limiares_elementos_LAB_", lab[l],
                                               "_FOLHA_", folhas[j], ".csv"), sep = ";",
                    dec = "," , row.names = FALSE, fileEncoding = "latin1" , na = "")
        write.table(tab, paste0(path, "sumario_estatistico_LAB_", lab[l], "_FOLHA_",
                                folhas[j], ".csv"), sep=";", dec="," , row.names = FALSE, fileEncoding = "latin1", na = "" )

        write.table(cbind(data_campo ,data_orig), paste0(path, "dados_analiticos_LAB_",
                                                        lab[l], "_FOLHA_", folhas[j],".csv"),
                    sep = ";", dec = "," , row.names = FALSE , fileEncoding = "latin1", na = "")

        # Salva planilha com limites de classe (75%)
        #setwd(cwd)
      }else
        next
    }
    r <-  4674
    ## List filenames to be merged.
    filenames <- paste0(path,list.files(path = path,
                                        pattern = glob2rx("destaque_todas*.csv")))
    ## Merge listed files from the path above
    dataset <- do.call("rbind", lapply(filenames,
                                       FUN = function(files){ read.csv2(files, fileEncoding = "latin1")}))
    # Salva planilha csv das classificações por folha
    write.table(dataset, paste0( path2,  "destaque_todas_LAB_", lab[l],".csv"),
                sep=";", dec="," , row.names = FALSE, na = "" )

    ## List filenames to be merged.
    filenames <- paste0(path,list.files(path = path,
                                        pattern = glob2rx("destaque_LAB*.csv")))
    ## Merge listed files from the path above
    dataset <- do.call("rbind",lapply(filenames,
                                      FUN = function(files){ read.csv2(files)}))
    # Salva planilha csv das classificações por folha
    write.csv2(dataset, paste0( path2,  "destaque_LAB_", lab[l], ".csv"),
               row.names = FALSE, na = "" )

    ## List filenames to be merged.
    filenames <- paste0(path, list.files(path = path,
                                         pattern = glob2rx("limiares_elementos_LAB*.csv")))

    ## Merge listed files from the path above
       dataset <- do.call(plyr::"rbind.fill",lapply(filenames,
                                           FUN = function(files)
                                           { read.csv2(files, fileEncoding = "latin1")}))

       dataset <- dplyr::filter(dataset, !is.na(Limiar))

    # Salva planilha csv das classificações por folha
    write.csv2(dataset, paste0( path2, "legenda_LAB_", lab[l], ".csv"),
               row.names = FALSE, na = "")


    ## List filenames to be merged.
    filenames <- paste0(path, list.files(path = path,
                                         pattern = glob2rx("sumario_estatistico_LAB*.csv")))

    ## Merge listed files from the path above
    dataset <- do.call(plyr::"rbind.fill",lapply(filenames,
                                           FUN = function(files)
                                           { read.csv2(files,
                                                       sep = ";", dec = ",")}))

    # Salva planilha csv das classificações por folha
    write.csv2(dataset, paste0( path2, "sumario_LAB_",lab[l], ".csv"),
               row.names = FALSE,  na = "")


    ## List filenames to be merged.
    filenames <- paste0(path,list.files(path = path,
                                        pattern = glob2rx("dados_analiticos*.csv")))

    ## Merge listed files from the path above
    dataset <- do.call("rbind",lapply(filenames,
                                      FUN=function(files)
                                      { read.csv2(files,
                                                  sep = ";", dec = ",")}))

    # Salva planilha csv das classificações por folha
    write.table(dataset, paste0( path2, "dados_analiticos_LAB_", lab[l], ".csv"),
                sep = ";", dec = "," , row.names = FALSE, na = "")

    #Gerar shapes
    ##Destaques
    destaques <- read.csv2( paste0( path2, "destaque_LAB_", lab[l], ".csv"))
    destaques_st <-  sf::st_as_sf(destaques, coords = c("Long", "Lat"), crs = r)
    sf::st_write(destaques_st, paste0( path2, "destaque_LAB_", lab[l], ".shp"),
             driver="ESRI Shapefile", delete_layer = TRUE)


    ##Destaques todas
    destaques_todas <- read.csv2( paste0( path2, "destaque_todas_LAB_",
                                          lab[l], ".csv"))
    destaques_todas_st <-  sf::st_as_sf(destaques_todas, coords = c("Long", "Lat"),
                                    crs = r)

     sf::st_write(destaques_todas_st, paste0( path2, "destaque_todas_LAB_", lab[l],
                                         ".shp"), driver="ESRI Shapefile",
             delete_layer = TRUE)

    dados_analiticos <- read.csv2( paste0( path2, "dados_analiticos_LAB_",
                                           lab[l], ".csv"), fileEncoding = "latin1")
    dados_analiticos_st <-  sf::st_as_sf(dados_analiticos, coords = c("Long", "Lat"),
                                     crs = r)
    sf::st_write(dados_analiticos_st, paste0( path2, "dados_analiticos_LAB_", lab[l],
                                          ".shp"), driver="ESRI Shapefile",
             delete_layer = TRUE)


  }

}

colnames(df_brutos)
# df <- data.frame(df[, c(1:5, 69:72, 79)], df[,c(6:68,73:78)])
df_brutos <- df_brutos[df_brutos$N_lab %in% data_transf$N_lab,]
data_originais_st <-  sf::st_as_sf(df_brutos, coords = c("Long", "Lat"),
                               crs = r)
data_originais_st <- data_originais_st[data_originais_st$Folha != "",]
sf::st_write(data_originais_st, "outputs/Dados_areas/TODAS/dados_arrumados_brutos.shp", driver="ESRI Shapefile",
         delete_layer = TRUE )

data_transf_st <-  sf::st_as_sf(data_transf, coords = c("Long", "Lat"),
                            crs = r)
data_transf_st <- data_transf_st[data_transf_st$Folha != "",]

sf::st_write(data_transf_st, "outputs/Dados_areas/TODAS/dados_arrumados_transf.shp", driver="ESRI Shapefile",
         delete_layer = TRUE)
write.csv2(df_brutos, "outputs/Dados_areas/TODAS/dados_arrumados_brutos.csv", row.names = F)

write.csv2(data_transf, "outputs/Dados_areas/TODAS/dados_arrumados_transf.csv", row.names = F)

 }
