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
if("bit64" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote bit64...')
  install.packages("bit64")
}

#carrega pacotes requeridos
show('Carregando pacote feather...')
library(feather)
show('Carregando pacote tcltk...')
library(tcltk)
show('Carregando pacote bit64...')
library(bit64)

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
    "valor"="integer64",
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
    "valor"="integer64",
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
    "valor"="integer64",
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

bal_rec <- processa_txt(
  "BAL_REC.TXT",
  c(20,4,13,13,4,170,1,2,3,13,4),
  c(
    "receita",
    "uniorcam",
    "orcado",
    "arrecadado",
    "recurso_vinculado",
    "especificacao",
    "tipo_nivel",
    "nivel",
    "caracteristica_peculiar_receita",
    "previsao_atualizada",
    "complemento_recurso_vinculado"
  ),
  c(
    "receita"="character",
    "uniorcam"="character",
    "orcado"="integer64",
    "arrecadado"="integer64",
    "recurso_vinculado"="integer",
    "especificacao"="character",
    "tipo_nivel"="character",
    "nivel"="integer",
    "caracteristica_peculiar_receita"="integer",
    "previsao_atualizada"="integer64",
    "complemento_recurso_vinculado"="integer"
  )
)
bal_rec$orcado = bal_rec$orcado /100
bal_rec$arrecadado = bal_rec$arrecadado /100
bal_rec$previsao_atualizada = bal_rec$previsao_atualizada /100
#bal_rec

receita <- processa_txt(
  "RECEITA.TXT",
  c(20,4,13,13,13,13,13,13,13,13,13,13,13,13,12,12,12,12,12,12,3,4,4),
  c(
    "receita",
    "uniorcam",
    "arrecadado_janeiro",
    "arrecadado_fevereiro",
    "arrecadado_marco",
    "arrecadado_abril",
    "arrecadado_maio",
    "arrecadado_junho",
    "arrecadado_julho",
    "arrecadado_agosto",
    "arrecadado_setembro",
    "arrecadado_outubro",
    "arrecadado_novembro",
    "arrecadado_dezembro",
    "meta_1bim",
    "meta_2bim",
    "meta_3bim",
    "meta_4bim",
    "meta_5bim",
    "meta_6bim",
    "caracteristica_peculiar_receita",
    "recurso_vinculado",
    "complemento_recurso_vinculado"
  ),
  c(
    "receita"="character",
    "uniorcam"="character",
    "arrecadado_janeiro"="integer64",
    "arrecadado_fevereiro"="integer64",
    "arrecadado_marco"="integer64",
    "arrecadado_abril"="integer64",
    "arrecadado_maio"="integer64",
    "arrecadado_junho"="integer64",
    "arrecadado_julho"="integer64",
    "arrecadado_agosto"="integer64",
    "arrecadado_setembro"="integer64",
    "arrecadado_outubro"="integer64",
    "arrecadado_novembro"="integer64",
    "arrecadado_dezembro"="integer64",
    "meta_1bim"="integer64",
    "meta_2bim"="integer64",
    "meta_3bim"="integer64",
    "meta_4bim"="integer64",
    "meta_5bim"="integer64",
    "meta_6bim"="integer64",
    "caracteristica_peculiar_receita"="integer",
    "recurso_vinculado"="integer",
    "complemento_recurso_vinculado"="integer"
  )
)
receita$arrecadado_janeiro = receita$arrecadado_janeiro /100
receita$arrecadado_fevereiro = receita$arrecadado_fevereiro /100
receita$arrecadado_marco = receita$arrecadado_marco /100
receita$arrecadado_abril = receita$arrecadado_abril /100
receita$arrecadado_maio = receita$arrecadado_maio /100
receita$arrecadado_junho = receita$arrecadado_junho /100
receita$arrecadado_julho = receita$arrecadado_julho /100
receita$arrecadado_agosto = receita$arrecadado_agosto /100
receita$arrecadado_setembro = receita$arrecadado_setembro /100
receita$arrecadado_outubro = receita$arrecadado_outubro /100
receita$arrecadado_novembro = receita$arrecadado_novembro /100
receita$arrecadado_dezembro = receita$arrecadado_dezembro /100
receita$meta_1bim = receita$meta_1bim /100
receita$meta_2bim = receita$meta_2bim /100
receita$meta_3bim = receita$meta_3bim /100
receita$meta_4bim = receita$meta_4bim /100
receita$meta_5bim = receita$meta_5bim /100
receita$meta_6bim = receita$meta_6bim /100
#receita

bal_desp <- processa_txt(
  "BAL_DESP.TXT",
  c(4,2,3,4,3,5,6,4,13,13,13,13,13,13,13,13,13,13,13,13,13,13,4),
  c(
    "uniorcam",
    "funcao",
    "subfuncao",
    "programa",
    "obsoleto1",
    "projativ",
    "elemento",
    "recurso_vinculado",
    "dotacao_inicial",
    "atualizacao_monetaria",
    "credito_suplementar",
    "credito_especial",
    "credito_extraordinario",
    "reducao_dotacao",
    "suplementacao_recurso_vinculado",
    "reducao_recurso_vinculado",
    "empenhado",
    "liquidado",
    "pago",
    "limitado",
    "recomposto",
    "previsao_ate_termino",
    "complemento_recurso_vinculado"
  ),
  c(
    "uniorcam"="character",
    "funcao"="integer",
    "subfuncao"="integer",
    "programa"="integer",
    "obsoleto1"="character",
    "projativ"="integer",
    "elemento"="character",
    "recurso_vinculado"="integer",
    "dotacao_inicial"="integer64",
    "atualizacao_monetaria"="integer64",
    "credito_suplementar"="integer64",
    "credito_especial"="integer64",
    "credito_extraordinario"="integer64",
    "reducao_dotacao"="integer64",
    "suplementacao_recurso_vinculado"="integer64",
    "reducao_recurso_vinculado"="integer64",
    "empenhado"="integer64",
    "liquidado"="integer64",
    "pago"="integer64",
    "limitado"="integer64",
    "recomposto"="integer64",
    "previsao_ate_termino"="integer64",
    "complemento_recurso_vinculado"="integer"
  )
)
bal_desp$dotacao_inicial = bal_desp$dotacao_inicial /100
bal_desp$atualizacao_monetaria = bal_desp$atualizacao_monetaria /100
bal_desp$credito_suplementar = bal_desp$credito_suplementar /100
bal_desp$credito_especial = bal_desp$credito_especial /100
bal_desp$credito_extraordinario = bal_desp$credito_extraordinario /100
bal_desp$reducao_dotacao = bal_desp$reducao_dotacao /100
bal_desp$suplementacao_recurso_vinculado = bal_desp$suplementacao_recurso_vinculado /100
bal_desp$reducao_recurso_vinculado = bal_desp$reducao_recurso_vinculado /100
bal_desp$empenhado = bal_desp$empenhado /100
bal_desp$liquidado = bal_desp$liquidado /100
bal_desp$liquidado = bal_desp$pago /100
bal_desp$limitado = bal_desp$limitado /100
bal_desp$recomposto = bal_desp$recomposto /100
bal_desp$previsao_ate_termino = bal_desp$previsao_ate_termino /100
#bal_desp

tce_4111 <- processa_txt(
  "tce_4111.TXT",
  c(20,4,4,12,12,13,8,17,1,12,150,1,1,1,4,4),
  c(
    "conta_contabil",
    "uniorcam",
    "reservado1",
    "numero_lancamento",
    "numero_lote",
    "numero_documento",
    "data",
    "valor",
    "tipo_lancamento",
    "numero_arquivamento",
    "historico",
    "tipo_documento",
    "natureza_informacao",
    "indicador_superavit",
    "recurso_vinculado",
    "complemento_recurso_vinculado"
  ),
  c(
    "conta_contabil"="character",
    "uniorcam"="character",
    "reservado1"="character",
    "numero_lancamento"="character",
    "numero_lote"="character",
    "numero_documento"="character",
    "data"="character",
    "valor"="integer64",
    "tipo_lancamento"="character",
    "numero_arquivamento"="character",
    "historico"="character",
    "tipo_documento"="character",
    "natureza_informacao"="character",
    "indicador_superavit"="character",
    "recurso_vinculado"="integer",
    "complemento_recurso_vinculado"="integer"
  )
)
tce_4111$data = as.Date(tce_4111$data, format = "%d%m%Y")
tce_4111$valor = tce_4111$valor / 100
#tce_4111

bal_ver <- processa_txt(
  "BAL_VER.TXT",
  c(20,4,13,13,13,13,13,13,148,1,2,1,1,1,1,4,4),
  c(
    "conta_contabil",
    "uniorcam",
    "saldo_anterior_debito",
    "saldo_anterior_credito",
    "movimento_debito",
    "movimento_credito",
    "saldo_atual_debito",
    "saldo_atual_credito",
    "especificacao",
    "tipo_nivel",
    "nivel",
    "obsoleto1",
    "escrituracao",
    "natureza_informacao",
    "indicador_superavit",
    "recurso_vinculado",
    "complemento_recurso_vinculado"
  ),
  c(
    "conta_contabil"="character",
    "uniorcam"="character",
    "saldo_anterior_debito"="integer64",
    "saldo_anterior_credito"="integer64",
    "movimento_debito"="integer64",
    "movimento_credito"="integer64",
    "saldo_atual_debito"="integer64",
    "saldo_atual_credito"="integer64",
    "especificacao"="character",
    "tipo_nivel"="character",
    "nivel"="integer",
    "obsoleto1"="character",
    "escrituracao"="character",
    "natureza_informacao"="character",
    "indicador_superavit"="character",
    "recurso_vinculado"="integer",
    "complemento_recurso_vinculado"="integer"
  )
)
bal_ver$saldo_anterior_debito = bal_ver$saldo_anterior_debito / 100
bal_ver$saldo_anterior_credito = bal_ver$saldo_anterior_credito / 100
bal_ver$movimento_debito = bal_ver$movimento_debito / 100
bal_ver$movimento_credito = bal_ver$movimento_credito / 100
bal_ver$saldo_atual_debito = bal_ver$saldo_atual_debito / 100
bal_ver$saldo_atual_credito = bal_ver$saldo_atual_credito / 100
#bal_ver

if(file.exists("BVER_ENC.TXT")){
  bver_enc <- processa_txt(
    "BVER_ENC.TXT",
    c(20,4,13,13,13,13,13,13,148,1,2,1,1,1,1,4,4),
    c(
      "conta_contabil",
      "uniorcam",
      "saldo_anterior_debito",
      "saldo_anterior_credito",
      "movimento_debito",
      "movimento_credito",
      "saldo_atual_debito",
      "saldo_atual_credito",
      "especificacao",
      "tipo_nivel",
      "nivel",
      "obsoleto1",
      "escrituracao",
      "natureza_informacao",
      "indicador_superavit",
      "recurso_vinculado",
      "complemento_recurso_vinculado"
    ),
    c(
      "conta_contabil"="character",
      "uniorcam"="character",
      "saldo_anterior_debito"="integer64",
      "saldo_anterior_credito"="integer64",
      "movimento_debito"="integer64",
      "movimento_credito"="integer64",
      "saldo_atual_debito"="integer64",
      "saldo_atual_credito"="integer64",
      "especificacao"="character",
      "tipo_nivel"="character",
      "nivel"="integer",
      "obsoleto"="character",
      "escrituracao"="character",
      "natureza_informacao"="character",
      "indicador_superavit"="character",
      "recurso_vinculado"="integer",
      "complemento_recurso_vinculado"="integer"
    )
  )
  bver_enc$saldo_anterior_debito = bver_enc$saldo_anterior_debito / 100
  bver_enc$saldo_anterior_credito = bver_enc$saldo_anterior_credito / 100
  bver_enc$movimento_debito = bver_enc$movimento_debito / 100
  bver_enc$movimento_credito = bver_enc$movimento_credito / 100
  bver_enc$saldo_atual_debito = bver_enc$saldo_atual_debito / 100
  bver_enc$saldo_atual_credito = bver_enc$saldo_atual_credito / 100
}#bver_enc

#muda o diretório para salvar os arquivos
show(paste('Alterando diretório de trabalho para', destino, '...', sep = ' '))
setwd(destino)

#salva os arquivos
show('Salvando arquivos...')
system.time({
  write_feather(empenho, "empenho.feather")
  write_feather(liquidac, "liquidac.feather")
  write_feather(pagament, "pagament.feather")
  write_feather(bal_rec, "bal_rec.feather")
  write_feather(receita, "receita.feather")
  write_feather(bal_desp, "bal_desp.feather")
  write_feather(tce_4111, "tce_4111.feather")
  write_feather(bal_ver, "bal_ver.feather")
  if(file.exists("BVER_ENC.TXT")){
    write_feather(bver_enc, "bver_enc.feather")
  }
})
show('Fim!')