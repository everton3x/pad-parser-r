options(java.parameters = "- Xmx2048m")
#verifica (e instala) pacotes requeridos
show('Testando se pacotes necess�rios est�o instalados...')
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

#remove vari�veis antigas
show('Removendo lixo...')
rm(list = ls())

#pega o local dos arquivos
show('Selecionando origem dos dados...')
origem <- tk_choose.files(multi = TRUE, filters = matrix(c(
  "feather files", ".feather"
), 1,2, byrow = TRUE))

if(length(origem) == 0){
  show('Voc� n�o selecionou arquivos de origem!')
  quit()
}


#pega o diret�rio onde salvar os arquivos
show('Selecionando destino dos dados...')
destino <- tclvalue(tkgetSaveFile(filetypes = "{{MS Excel} {.xlsx}}"))

if(destino == ""){
  show('Voc� n�o selecionou um arquivo de destino!')
  quit()
}

vetor = unlist(strsplit(basename(destino), split = "[.]"))
if(vetor[2] != "xlsx"){
  show(paste("O arquivo de destino precisa ter a extens�o xlsx e n�o", vetor[2], sep = " "))
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