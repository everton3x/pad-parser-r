#verifica (e instala) pacotes requeridos
show('Testando se pacotes necessários estão instalados...')
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
  c(4,2,3,4,3,5,15,4,4,13,8,13,1,10,165,03,2,1,20,20,4,400,3,2,1,1,14,4),
  c(
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
    "data",
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
    "uniorcam"="character",
    "funcao"="integer",
    "subfuncao"="integer",
    "programa"="integer",
    "obsoleto1"="integer",
    "projativ"="integer",
    "rubrica"="character",
    "recurso_vinculado"="integer",
    "contrapartida_recurso_vinculado"="integer",
    "numero_empenho"="character",
    "data"="character",
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
empenho$valor = empenho$valor /100
empenho$data = as.Date(empenho$data, format = "%d%m%Y")
#empenho

liquidac <- processa_txt(
  "LIQUIDAC.TXT",
  c(13,20,8,13,1,165,30,400,1,20,20,4,1,9,3,1),
  c(
    "numero_empenho",
    "numero_liquidacao",
    "data",
    "valor",
    "sinal",
    "obsoleto1",
    "operacao",
    "historico",
    "existe_contrato",
    "numero_contrato_tce",
    "numero_contrato",
    "ano_contrato",
    "existe_nota_fiscal",
    "numero_nota_fiscal",
    "serie_nota_fiscal",
    "tipo_contrato"
  ),
  c(
    "numero_empenho"="character",
    "numero_liquidacao"="integer",
    "data"="character",
    "valor"="integer",
    "sinal"="character",
    "obsoleto1"="character",
    "operacao"="character",
    "historico"="character",
    "existe_contrato"="character",
    "numero_contrato_tce"="integer",
    "numero_contrato"="character",
    "ano_contrato"="integer",
    "existe_nota_fiscal"="character",
    "numero_nota_fiscal"="integer",
    "serie_nota_fiscal"="character",
    "tipo_contrato"="character"
  )
)
liquidac$valor = liquidac$valor /100
liquidac$data = as.Date(liquidac$data, format = "%d%m%Y")
#liquidac

pagament <- processa_txt(
  "PAGAMENT.TXT",
  c(13,20,8,13,1,120,30,20,4,20,4,400,20),
  c(
    "numero_empenho",
    "numero_pagamento",
    "data",
    "valor",
    "sinal",
    "obsoleto1",
    "operacao",
    "conta_contabil_debito",
    "uniorcam_debito",
    "conta_contabil_credito",
    "uniorcam_credito",
    "historico",
    "numero_liquidacao"
  ),
  c(
    "numero_empenho"="character",
    "numero_pagamento"="integer",
    "data"="character",
    "valor"="integer",
    "sinal"="character",
    "obsoleto1"="character",
    "operacao"="character",
    "conta_contabil_debito"="character",
    "uniorcam_debito"="character",
    "conta_contabil_credito"="character",
    "uniorcam_credito"="character",
    "historico"="character",
    "numero_liquidacao"="integer"
  )
)
pagament$valor = pagament$valor /100
pagament$data = as.Date(pagament$data, format = "%d%m%Y")
#pagament

#muda o diretório para salvar os arquivos
show(paste('Alterando diretório de trabalho para', destino, '...', sep = ' '))
setwd(destino)

#salva os arquivos
show('Salvando arquivos...')
system.time({
  write_feather(empenho, "empenho.feather")
  write_feather(liquidac, "liquidac.feather")
  write_feather(pagament, "pagament.feather")
})
show('Fim!')