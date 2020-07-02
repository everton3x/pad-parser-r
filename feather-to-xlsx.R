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
if("xlsx" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote xlsx...')
  install.packages("xlsx")
}

#carrega pacotes requeridos
show('Carregando pacote feather...')
library(feather)
show('Carregando pacote tcltk...')
library(tcltk)
show('Carregando pacote xlsx...')
library(xlsx)

#remove variáveis antigas
show('Removendo lixo...')
rm(list = ls())

#pega o local dos arquivos
show('Selecionando origem dos dados...')
origem <- tk_choose.files(multi = TRUE, filters = matrix(c(
  "feather files", ".feather"
), 1,2, byrow = TRUE))

if(length(origem) == 0){
  show('Você não selecionou arquivos de origem!')
  quit()
}


#pega o diretório onde salvar os arquivos
show('Selecionando destino dos dados...')
destino <- tclvalue(tkgetSaveFile(filetypes = "{{MS Excel} {.xlsx}}"))

if(destino == ""){
  show('Você não selecionou um arquivo de destino!')
  quit()
}

vetor = unlist(strsplit(basename(destino), split = "[.]"))
if(vetor[2] != "xlsx"){
  show(paste("O arquivo de destino precisa ter a extensão xlsx e não", vetor[2], sep = " "))
  quit()
}

#faz o loop nos arquivos de origem
control = FALSE
for (arquivo_origem in origem) {
  show(paste("Convertendo",arquivo_origem, sep = " "))
  
  vetor = unlist(strsplit(basename(arquivo_origem), split = "[.]"))
  planilha = vetor[1]
    
  dados = read_feather(arquivo_origem)
  
  write.xlsx(dados, destino, sheetName = planilha, append = control)
  control = TRUE
  
}

show("Fim!")