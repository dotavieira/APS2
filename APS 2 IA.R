library(tidyverse)
library(caTools)
library(class)
library(ggplot2)
library(plotly)

options(digits = 8)

#Coleta de dados
hf_records = read.csv2(file = 'heart_failure_clinical_records_dataset.csv', dec = ',')
hf_records$age = as.numeric(hf_records$age)
hf_records$platelets = readr::parse_double(hf_records$platelets)
hf_records$serum_creatinine = readr::parse_double(hf_records$serum_creatinine)


#Para descobrir a porcentagem de mortos durante o tratamento
table(hf_records$DEATH_EVENT)/nrow(hf_records)

#Descobrindo se há dados faltantes
any(is.na(hf_records))

#No algotítmo KNN, é interessante ter os dados todos na mesma escala, visto que
#o algorítmo é baseado em distância

#Verificando a escala
var(hf_records[,1])
var(hf_records[,2])

if(var(hf_records[,1]) != var(hf_records[,2])){
  #Colocando os dados na mesma escala
  hf_records[,1:13] = scale(hf_records[,1:13])
}

#Verificando a escala
var(hf_records[,1])
var(hf_records[,2])

hf_records$DEATH_EVENT[hf_records$DEATH_EVENT > 1] = 'Sim'
hf_records$DEATH_EVENT[hf_records$DEATH_EVENT < 0] = 'Não'

#Separando as partes para teste e para treinamento
set.seed(1)
#divisao = sample.split(hf_records$age, SplitRatio = 0.5)
divisao = sample.split(hf_records$time, SplitRatio = 0.5)
hf_records_treino = subset(hf_records, divisao == TRUE) 
hf_records_teste  = subset(hf_records, divisao == FALSE)

#Knn_K2= knn(hf_records_treino[,-13], hf_records_teste[,-13],
#            hf_records_treino$DEATH_EVENT, k=2, prob=TRUE)

#Knn_K3= knn(hf_records_treino[,-13], hf_records_teste[,-13],
#            hf_records_treino$DEATH_EVENT, k=3, prob=TRUE)

#table(hf_records_teste$DEATH_EVENT, Knn_K2)
#table(hf_records_teste$DEATH_EVENT, Knn_K3)


#sum(Knn_K2==hf_records_teste$DEATH_EVENT)/length(hf_records_teste$DEATH_EVENT)*100
#sum(Knn_K3==hf_records_teste$DEATH_EVENT)/length(hf_records_teste$DEATH_EVENT)*100


Knn_Testes = list()
acuracia = numeric()

for(k in 1:20){
  
  Knn_Testes[[k]] = knn(hf_records_treino[,-13], hf_records_teste[,-13], hf_records_treino$DEATH_EVENT, k, prob=TRUE)
  acuracia[k] = sum(Knn_Testes[[k]]==hf_records_teste$DEATH_EVENT)/length(hf_records_teste$DEATH_EVENT)*100
  
}

plot(acuracia, type="b", col="blue", cex=1, pch=1,
     xlab="k", ylab="Acuracia",
     main="Acuracia para cada k")

# Linha vertical vermelha marcando o maximo
abline(v=which(acuracia==max(acuracia)), col="red", lwd=1.5)

# Linha horizontal cinza marcando o maximo
abline(h=max(acuracia), col="grey", lty=2)

# Linha horizontal cinza para marcar o minimo
abline(h=min(acuracia), col="grey", lty=2)


Knn_Kmax= knn(hf_records_treino[,-13], hf_records_teste[,-13],
              hf_records_treino$DEATH_EVENT, k = (which(acuracia == max(acuracia))), prob=TRUE)

table(hf_records_teste$DEATH_EVENT, Knn_Kmax)
#sum(Knn_Kmax==hf_records_teste$DEATH_EVENT)/length(hf_records_teste$DEATH_EVENT)*100
