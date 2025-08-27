#' Gera bases para a modelagem das bacias e estações
#'
#' Converte as bases de entrada em um alista de dados para serem carregadas nas
#' funções modela_bacias e gera estações.
#'
#' @param dir_in
#' @param limite
#' @param limite_srtm
#' @param area_urbana
#' @param rios
#' @param massa_dagua
#' @param areas_dificil_acesso
#'
#' @return
#' @export
#'
#' @examples
gera_bases_model <- function(dir_in = "inputs/campo/",
                             limite = "carta_100M.shp",
                             limite_srtm = "area_srtm.shp",
                             area_urbana = "area_urbana.shp",
                             rios = "rios_ibge.shp",
                             massa_dagua = "massa_dagua.shp",
                             areas_dificil_acesso = c("pantanal.shp", "area_protecao.shp", "terra_indigena")
                             )
{
  sf::sf_use_s2(FALSE)
  out <- list()
  lista_areas <- list()
  out[[1]] <- sf::read_sf(paste0(dir_in, limite_srtm))
  out[[2]] <- sf::read_sf(paste0(dir_in, limite))
  out[[3]] <- sf::read_sf(paste0(dir_in, area_urbana))
  out[[4]] <- sf::read_sf(paste0(dir_in, rios))
  out[[5]] <- sf::read_sf(paste0(dir_in, massa_dagua))

  for(i in 1:length(area_dificil_acesso)){
  lista_areas[[i]] <- sf::read_sf(paste0(dir_in, areas_dificil_acesso[i]))
  }

  out[[6]] <- do.call(sf::st_union, c(lista_areas, model = "open"))

  names(out) <-
    c("limite da área srtm", "limite da área folha", "área urbana",
      "rios", "massa de água", "area de dificil acesso")
   sf::sf_use_s2(TRUE)
  return(out)
}
