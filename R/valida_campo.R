# Valida dados de campo
# Recupera a base de dados
#' Title
#'
#' @param path_ctr diretório dos pontos de controle
#' @param path_base diretório da base de campo
#' @param base nome do gdb
#' @param layer_base feição das estações
#' @param amostras tabela com dados das amostras
#' @param threshold limiar em metros das distâcias entre estações
#' @param buffer raio de busca em relação ao ponto controle
#'
#' @return lista com as validações
#' @export
#'
#' @examples
valida_campo <- function(path_ctr, path_base, base, layer_base,
                         amostras, threshold, buffer ){
out <- list()
  # Fonte de dados
sf <- sf::st_read(paste0(path_base, base), layer = layer_base, quiet = TRUE)
df <- as.data.frame(sf)
df <- df[!is.na(df$nu_ponto),]

# Tem estações faltantes numa sequência
df_miss <- df %>% dplyr::mutate(vol=1) %>% dplyr::group_by(id_coletor) %>%
  tidyr::complete(nu_ponto = min(nu_ponto):max(nu_ponto), fill = list(vol = 0))

## Mostre as estações faltantes
faltantes <- df_miss[df_miss$vol == 0, c("nu_ponto", "id_coletor")]
if(nrow(faltantes) > 0){
  out[[1]] <- faltantes
  }else{
    faltantes <- "Todas validadas"
    }

# 1. Avaliação das Estações
# Ver se tem estações duplicadas
estacao_dup <- df[duplicated(df$nm_estacao),c("globalid", "nm_estacao")]
est_repetida <- df[duplicated(df$nm_estacao),"nm_estacao"]
estacao_dup <- df[df$nm_estacao %in% est_repetida, c("nm_estacao", "uniquerowid") ]

if(nrow(estacao_dup) > 0){
  out[[2]] <- estacao_dup
}else{
  out[[2]] <- "Todas validadas"
}

# Pela distancia
# filtra estações pela distância entre elas
dist_points <-  sf::st_distance(sf)
dist_points <- apply(dist_points, 2, as.numeric)
colnames(dist_points) <- sf$nm_estacao
rownames(dist_points) <- sf$nm_estacao
res_dist <- which(dist_points<=threshold & dist_points>0, arr.ind = T)
res_dist[,"row"]
estacao1 <- as.data.frame(sf[res_dist[,"col"],"nm_estacao"])[, "nm_estacao"]
estacao2 <- as.data.frame(sf[res_dist[,"row"],"nm_estacao"])[, "nm_estacao"]
res_dist <- unique(data.frame(estacao1, estacao2))

if(nrow(res_dist) > 0){
  out[[3]] <- res_dist
}else{
  out[[3]] <- "Todas validadas"
}


# Estações sem registros
id_estacao_na <- as.data.frame(sf[is.na(sf$nm_estacao),])
id_estacao_na <- id_estacao_na[, "globalid"]

if(length(id_estacao_na) > 0){
  out[[4]] <- id_estacao_na
}else{
  out[[4]] <- "Todas validadas"
}


# Avaliação das amostras
am <- sf::st_read(paste0(path_base, base), layer = amostras, quiet = TRUE)
am <- as.data.frame(am)

am <- am[!is.na(am$cd_numero_campo),]

am_repetida <- am[duplicated(am$cd_numero_campo),"cd_numero_campo"]
amostras_dup <- am[am$cd_numero_campo %in% am_repetida, c("cd_numero_campo", "parentrowid") ]
if(nrow(amostras_dup) > 0){
  out[[5]] <- amostras_dup
}else{
  out[[5]] <- "Todas validadas"
}


# id_estacao_dup <- am[duplicated(am$cd_numero_campo),"parentrowid"]
# estacoes_dup <- df[df$uniquerowid %in% id_estacao_dup,"nm_estacao"]
# out[[6]] <- estacoes_dup

## Conta numero de amostras por estações
unida <- dplyr::full_join(df, am, by = dplyr::join_by(uniquerowid == parentrowid))
contagem_amostras <- unida %>% dplyr::group_by(nm_estacao) %>% dplyr::count()
out[[6]] <- contagem_amostras[!is.na(contagem_amostras$nm_estacao),]

## Amostras sem estação
am_sem_estacao <- unida[is.na(unida$nm_estacao), "cd_numero_campo"]

if(length(am_sem_estacao) > 0){
  out[[7]] <- am_sem_estacao
}else{
  out[[7]] <- "Todas validadas"
}
## Conta o número de amostras duplicadas
am_repetidas <- am %>% dplyr::group_by(cd_numero_campo) %>% dplyr::count()

if(nrow(am_repetidas) > 0){
  out[[8]] <- am_repetidas[am_repetidas$n >1,]
}else{
  out[[8]] <- "Todas validadas"
}
## Pontos de controle
# Importa feição de pontos
sf_ctr <- sf::st_read(paste0(path_ctr, base_ctr), quiet = TRUE)

# Verifica se as estações estão próximas do controle

# Gera buffer nos pontos controle

sf_buffer <- sf::st_buffer(sf[!is.na(sf$nm_estacao),], dist = buffer)
p <- sf_buffer %>% sf::st_intersects(sf[!is.na(sf$nm_estacao),] )
filtro <- which(lengths(p) == 0)
sel <- list()
if(length(filtro) > 0){
  for(i in 1:length(filtro)){
  sel[[i]] <- sf$nm_estacao[p[[filtro[i]]]]
  res_dist <- as.data.frame(do.call(rbind, sel))
  res_dist <- unique(res_dist)
  rownames(res_dist) <- paste("Conjunto", 1:nrow(res_dist))
  colnames(res_dist) <- paste("nm_estacao", 1:ncol(res_dist))
}
} else{
  res_dist <- "Todas validadas"
    }



out[[9]] <- res_dist

# Calcula a distancia entre survey123 e ponto controle
pt_survey <- sf[, "nm_estacao"]
pt_ctr <- sf_ctr[, "N_CAMPO"]
pt_ctr$N_CAMPO <- gsub("-B-", "-", pt_ctr$N_CAMPO)
pt_ctr$N_CAMPO <- gsub("-S-", "-", pt_ctr$N_CAMPO)
pt_ctr$N_CAMPO <- gsub("D", "", pt_ctr$N_CAMPO)
pt_ctr <- unique(pt_ctr)
colnames(pt_ctr)[1] <- "nm_estacao"
pt_ctr$layer <- 1
pt_survey$layer <- 2
pt_survey$geometry <- pt_survey$Shape
merge_estacao <- dplyr::bind_rows(pt_survey, pt_ctr)

dist_ctr <- merge_estacao %>%
  arrange(nm_estacao, desc(layer)) %>%
  dplyr::group_by(nm_estacao) %>%
  dplyr::mutate(
    study_coord = geometry[1],
    dist = sf::st_distance(geometry, study_coord, by_element = T)
  )

# Extrai estações com distâcia maior do que o limiar
dist_ctr$dist <- as.numeric(dist_ctr$dist)
res_dist_ctr <- as.data.frame(dist_ctr[dist_ctr$dist > threshold, "nm_estacao"])
res_dist_ctr <- res_dist_ctr$nm_estacao[!is.na(res_dist_ctr$nm_estacao)]
if(length(res_dist_ctr) > 0){
  out[[10]] <- res_dist_ctr
}else{
  out[[10]] <- "Todas validadas"
}



# Amostras duplicatas sem registro
estacoes_sel <- contagem_amostras[contagem_amostras$n>1,"nm_estacao"]
am_dup <- unida[unida$nu_ncampo_duplicata == 1 & is.na(unida$nm_amostra_extra) |
                  unida$nm_estacao %in% estacoes_sel , "cd_numero_campo" ]
if(length(am_dup) > 0){
  out[[11]] <- am_dup
}else{
  out[[11]] <- "Todas validadas"
}

# Duplitatas de campo com erro no nome
dupli_erro_nome <- unida[ unida$nm_amostra_extra != "A" &  !is.na(unida$nm_amostra_extra),
                         "cd_numero_campo"]
if(length(dupli_erro_nome) > 0){
  out[[12]] <- dupli_erro_nome
}else{
  out[[12]] <- "Todas validadas"
}

# Sem anotar que é duplicata na base (nu_ncampo_duplicata = 0)
sem_anotar_dup <- unida[!is.na(unida$nm_amostra_extra) &
                          unida$nu_ncampo_duplicata == 0,"cd_numero_campo"]

if(length(sem_anotar_dup) > 0){
  out[[13]] <- sem_anotar_dup
}else{
  out[[13]] <- "Todas validadas"
}


names(out) <- c("Estações ausentes", "Estações duplicadas",
                "Estações muito próxima da vizinha", "Estações sem registros",
                "Amostras duplicadas",
                "Contagem de amostras por estações","Amostras sem estação",  "Número de campo repetido",
                "Estações dentro do buffer do ponto de controle",
                "Estações com distância entre ponto controle e survey123 maior do que o limiar",
                "Amostras duplicatas de campo sem registro", "Nome da duplicata com erro", "Amostras sem registro da duplicata")
return(out)
}
