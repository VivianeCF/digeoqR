
#' Cria diretórios e subdiretórios para o processamento de dados
#'
#' @param path Caminho para o diretório do projeto.
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
  dir.create(paste0(path, 'inputs/quimica'))
  dir.create(paste0(path, 'inputs/quimica/B'))
  dir.create(paste0(path, 'inputs/quimica/L'))
  dir.create(paste0(path, 'inputs/quimica/R'))
  dir.create(paste0(path, 'inputs/quimica/S'))

  # Dados de Campo
  dir.create(paste0(path, 'inputs/campo'))

  # Dados de Projetos do Rgeo
  dir.create(paste0(path, 'inputs/projetos'))

  # Dados de Imagens (SRTM)
  dir.create(paste0(path, 'inputs/imagens'))

  # Dados Diversos
  dir.create(paste0(path, 'inputs/diversos'))
}
