library(data.table)
library(stringi)
library(stringr)
library(caret)
library(randomForest)
library(RSNNS)
library(doParallel)
library(h2o)
library(lubridate)
library(fields)
library(caTools)
library(tcltk)
library(PRROC)
library(pROC)
library(rvest)
library(e1071)
library(h2oEnsemble)
library(yaml)

# Choose git repository
# setwd(tclvalue(tkchooseDirectory()))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Sources preprocessing functions
source('../general/pre_processing.R')
source('./prep_datav03.R')
source('./modelo2_tempo_duracao.R')
source('./modelo3_proc_improc.R')
source('./modelo4_acordo_nacordo.R')
source('./modelo5_valor_predito.R')
source('./modelo6_valor_predito_acordo.R')
source('./predict.R')

#############################################################
# Columns
#############################################################
variables = yaml.load_file("./confFiles/parameters.yml")

trabalhista = fread("./databases/trabalhista.csv",sep="|", na.strings = c("Não Informado","Sem Resultado",""))
trabalhista[trabalhista == "Não Informado"] = NA

prep_data(trabalhista)

