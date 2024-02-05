
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
  dir.create('inputs')
  dir.create('outputs')
  # Dados mineralométricos
  dir.create('inputs/mineral')
  dir.create('inputs/mineral/B')
  dir.create('inputs/mineral/L')
  dir.create('inputs/mineral/R')
  dir.create('inputs/mineral/S')

  # Dados Químicos
  dir.create('inputs/quimica')
  dir.create('inputs/quimica/B')
  dir.create('inputs/quimica/L')
  dir.create('inputs/quimica/R')
  dir.create('inputs/quimica/S')

  # Dados de Campo
  dir.create('inputs/campo')

  # Dados de Projetos do Rgeo
  dir.create('inputs/projetos')

  # Dados de Imagens (SRTM)
  dir.create('inputs/imagens')

  # Dados Diversos
  dir.create('inputs/diversos')
}
