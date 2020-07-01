#verifica (e instala) pacotes requeridos
show('Testando se pacotes necessários estão isntalados...')
if("feather" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote feather...')
  install.packages("feather")
}
if("tcltk" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote tcltk...')
  install.packages("tcltk")
}
#carrega pacotes requeridos
show('Carregando pacote feather...')
library(feather)
show('Carregando pacote tcltk...')
library(tcltk)

#remove variáveis antigas
show('Removendo lixo...')
rm(list = ls())

#carrega configuracoes, se existir
#show('Verificando se existem configurações...')
#if(file.exists("config.feather")){
#  show('Carregando configurações...')
#  configuracao <- read_feather("config.feather")
#}else{
#  show('Criando configurações padrão...')
#  configuracao = data.frame(origem='.', destino='.', stringsAsFactors = FALSE)
#}

#pega o local dos arquivos
show('Selecionando origem dos dados...')
#origem <- choose.dir(default = configuracao$origem, caption = "Selecione o diretório dos TXT")
#setwd(configuracao$origem)
#origem <- tclvalue(tkchooseDirectory())
origem <- "C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-05/pm/MES05"
if(origem == ""){
  show('Você não selecionou um diretório de origem!')
  quit()
}

#pega o diretório onde salvar os arquivos
show('Selecionando destino dos dados...')
#destino <- tclvalue(tkchooseDirectory())
destino <- "C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-05/pm"
if(destino == ""){
  show('Você não selecionou um diretório de destino!')
  quit()
}

#muda o diretório de trabalho para onde estão os txt
show(paste('Mudando o diretório de trabalho para', '...', origem, sep = " "))
setwd(origem)

#processa os arquivos
processa_txt <- function(arquivo, larguras, colunas, tipos){
  nlinhas <- length(
    readLines(arquivo)
  )
  show(paste('Processando', arquivo, '...', sep = ' '))
  dados <- read.fwf(
    arquivo,
    widths = larguras,
    col.names = colunas,
    strip.white = TRUE,
    skip = 1,
    stringsAsFactors = FALSE,
    colClasses = tipos,
    nrows = nlinhas-2
  )
  return(dados)
}

#definição dos arquivos
empenho <- processa_txt(
  "EMPENHO.TXT",
  c(2,2,2,3,4,3,5,15,4,4,13,2,2,4,13,1,10,165,03,2,1,20,20,4,400,3,2,1,1,14,4),
  c(
    "orgao",
    "uniorcam",
    "funcao",
    "subfuncao",
    "programa",
    "obsoleto1",
    "projativ",
    "rubrica",
    "recurso_vinculado",
    "contrapartida_recurso_vinculado",
    "numero_empenho",
    "dia",
    "mes",
    "ano",
    "valor",
    "sinal",
    "credor",
    "obsoleto2",
    "caracteristica_peculiar_despesa",
    "obsoleto3",
    "registro_preco",
    "obsoleto4",
    "numero_licitacao",
    "ano_licitacao",
    "historico",
    "forma_contratacao",
    "base_legal",
    "identificador_funcionario",
    "licitacao_compartilhada",
    "cnpj_gerenciador",
    "complemento_recurso_vinculado"
  ),
  c(
    "orgao"="integer",
    "uniorcam"="integer",
    "funcao"="integer",
    "subfuncao"="integer",
    "programa"="integer",
    "obsoleto1"="integer",
    "projativ"="integer",
    "rubrica"="character",
    "recurso_vinculado"="integer",
    "contrapartida_recurso_vinculado"="integer",
    "numero_empenho"="character",
    "dia"="integer",
    "mes"="integer",
    "ano"="integer",
    "valor"="integer",
    "sinal"="character",
    "credor"="integer",
    "obsoleto2"="character",
    "caracteristica_peculiar_despesa"="integer",
    "obsoleto3"="character",
    "registro_preco"="character",
    "obsoleto4"="character",
    "numero_licitacao"="integer",
    "ano_licitacao"="integer",
    "historico"="character",
    "forma_contratacao"="character",
    "base_legal"="integer",
    "identificador_funcionario"="character",
    "licitacao_compartilhada"="character",
    "cnpj_gerenciador"="character",
    "complemento_recurso_vinculado"="integer"
  )
)

#transformações
empenho$valor = empenho$valor /100

#muda o diretório para salvar os arquivos
show(paste('Alterando diretório de trabalho para', destino, '...', sep = ' '))
setwd(destino)

#salva os arquivos
show('Salvando arquivos...')
system.time({
  write_feather(empenho, "empenho.feather")
})
show('Fim!')