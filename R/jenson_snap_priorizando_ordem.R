#' @title Jenson Snap Pour Points com Prioridade de Ordem
#' @description Move os pontos de entrada para a drenagem mais próxima, priorizando a ordem de Strahler.
#'
#' @param input_points_path Caminho para o arquivo SHP dos pontos de entrada (estações).
#' @param strahler_raster_path Caminho para o arquivo TIF com as ordens de Strahler.
#' @param output_snap_path Caminho para o arquivo de saída dos pontos ajustados.
#' @param snap_dist_max Distância máxima de encaixe (em metros).
#' @param wbt_wd Diretório de trabalho do Whitebox.
#' @param decrescente Logico TRUE para ordem decrecente e FALSE para ordem crescente.#'
#' @return Retorna o caminho para o arquivo de saída dos pontos ajustados.
#'
jenson_snap_priorizando_ordem <- function(input_points_path,
                                          strahler_raster_path,
                                          output_snap_path,
                                          snap_dist_max,
                                          wbt_wd, decrescente) {

  # Certifica-se de que o diretório de trabalho está definido
  wbt_wd(wd = wbt_wd)

  # Carrega os pontos e o raster de Strahler
  pontos <- st_read(input_points_path)
  strahler_raster <- raster(strahler_raster_path)

  # Obtém as ordens de Strahler únicas e as ordena de forma crescente
  # A lógica agora prioriza a menor ordem, mudando 'decreasing' para FALSE.
  ordens <- sort(unique(values(strahler_raster)), decreasing = decrescente)

  # Cria um objeto para armazenar os pontos não encaixados
  pontos_nao_encaixados <- pontos

  # Cria uma lista para armazenar os pontos já encaixados
  pontos_encaixados_list <- list()

  # Itera sobre as ordens de Strahler
  for (ordem in ordens) {

    # Ignora valores NA
    if (is.na(ordem)) {
      next
    }

    # Adiciona a condição para verificar se ainda há pontos para encaixar
    if (nrow(pontos_nao_encaixados) > 0) {

      cat(sprintf("Tentando encaixar %d pontos na ordem de Strahler %d...\n",
                  nrow(pontos_nao_encaixados), ordem))

      # Filtra o raster de Strahler para a ordem atual
      strahler_filtrado <- strahler_raster
      strahler_filtrado[strahler_filtrado != ordem] <- NA

      # Salva o raster filtrado temporariamente
      temp_strahler_path <- file.path(paste0(wbt_wd, "strahler_ordem_", ordem, ".tif"))
      writeRaster(strahler_filtrado, temp_strahler_path, overwrite = TRUE)

      # Cria um nome de arquivo temporário para os pontos ajustados nesta iteração
      temp_output_path <- file.path(paste0(wbt_wd, "snap_temp_ordem_", ordem, ".shp"))

      # Salva os pontos não encaixados em um arquivo temporário
      temp_points_path <- file.path(paste0(wbt_wd, "pontos_nao_encaixados_temp_", ordem, ".shp"))
      st_write(pontos_nao_encaixados, temp_points_path, delete_layer = TRUE, quiet = TRUE)

      # Executa o Jenson Snap Pour Points com o raster filtrado
      wbt_jenson_snap_pour_points(
        pour_pts = temp_points_path,
        streams = temp_strahler_path,
        output = temp_output_path,
        snap_dist = snap_dist_max
      )

      # Carrega os pontos ajustados
      pontos_ajustados <- st_read(temp_output_path)

      # Calcula as distâncias, mas somente se os pontos tiverem sido movidos
      if (nrow(pontos_ajustados) > 0) {

        # Garante que os IDs dos pontos sejam consistentes para a comparação
        pontos_ajustados$ID <- pontos_nao_encaixados$ID

        # Calcula as distâncias entre os pontos originais e os ajustados
        distancias <- st_distance(pontos_nao_encaixados, pontos_ajustados, by_element = TRUE)

        # Seleciona os pontos que foram realmente movidos
        pontos_encaixados_agora <- pontos_ajustados[which(as.numeric(distancias) > 0.0), ]

        # Se houver pontos encaixados, armazena-os e remove-os da lista de não-encaixados
        if (nrow(pontos_encaixados_agora) > 0) {
          pontos_encaixados_list[[as.character(ordem)]] <- pontos_encaixados_agora

          # Obtém os IDs dos pontos que foram encaixados
          ids_encaixados <- pontos_encaixados_agora$ID

          # Remove os pontos encaixados da lista de "pontos_nao_encaixados"
          pontos_nao_encaixados <- pontos_nao_encaixados[!pontos_nao_encaixados$ID %in% ids_encaixados, ]
        }
      }

      # Remove todos os arquivos temporários criados na iteração
      temp_points_base_name <- paste0("pontos_nao_encaixados_temp_", ordem)
      temp_output_base_name <- paste0("snap_temp_ordem_", ordem)

      files_to_remove <- list.files(path = wbt_wd,
                                    pattern = paste0("^(", temp_points_base_name, "|", temp_output_base_name, "|strahler_ordem_", ordem, ")\\..*"),
                                    full.names = TRUE)

      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      }
    }
  }

  # Junta todos os pontos encaixados
  if (length(pontos_encaixados_list) > 0) {
    pontos_finais <- do.call(rbind, pontos_encaixados_list)
  } else {
    cat("Nenhum ponto foi encaixado.")
    return(NULL)
  }

  # Adiciona os pontos não encaixados (se houver) ao resultado final
  if (nrow(pontos_nao_encaixados) > 0) {
    cat(sprintf("%d pontos não puderam ser encaixados em nenhuma drenagem.\n", nrow(pontos_nao_encaixados)))
    pontos_finais <- rbind(pontos_finais, pontos_nao_encaixados)
  }

  # Salva o arquivo final
  st_write(pontos_finais, output_snap_path, delete_layer = TRUE)

  cat(sprintf("Processo concluído. O arquivo de saída está em: %s\n", output_snap_path))

}
