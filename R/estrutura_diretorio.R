
#' Cria diretórios e subdiretórios para o processamento de dados
#'
#' @return Cria a estrutura dos diretórios onde devem ser lidos os arquivos de
#'   entrada e salvos os arquivos de saída
#' @export
#'
#' @examples
#'
estrutura_diretorio <- function() {
  # Diretório de entrada de dados
  dir.create(paste0('inputs'))
  dir.create(paste0('outputs'))
  # Dados mineralométricos
  dir.create(paste0('inputs/mineral'))
  dir.create(paste0('inputs/mineral/B'))
  dir.create(paste0('inputs/mineral/L'))
  dir.create(paste0('inputs/mineral/R'))
  dir.create(paste0('inputs/mineral/S'))

  # Dados Químicos
  dir.create(paste0('inputs/quimica'))
  dir.create(paste0('inputs/quimica/B'))
  dir.create(paste0('inputs/quimica/L'))
  dir.create(paste0('inputs/quimica/R'))
  dir.create(paste0('inputs/quimica/S'))

  # Dados de Campo
  dir.create(paste0('inputs/campo'))

  # Dados de Projetos do Rgeo
  dir.create(paste0('inputs/projetos'))

  # Dados de Imagens (SRTM)
  dir.create(paste0('inputs/imagens'))

  # Dados Diversos
  dir.create(paste0('inputs/diversos'))
}
