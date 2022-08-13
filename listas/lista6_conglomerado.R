# Você foi contratado(a) para estimar o volume de madeira da mata ciliar de um trecho de um rio. Para reduzir os
# deslocamentos você optou por utilizar a amostragem em conglomerados em um único estágio. Processe o inventário
# florestal (α = 10%) utilizando os resultados das medições por “cluster” apresentados na planilha de dados em anexo
# (“clusters.csv”). (100%)
# Campos da planilha de dados:
# 
# • ncluster: Número de clusters cabíveis na área amostrada;
# • cluster : Número do cluster selecionado pela ACS;
# • nparcs : Número de parcelas no cluster;
# • areaparc: Área da parcela [m2];
# • volume : Volume comercial com casca [m3/cluster]


#Leitura dos dados
dados <- read.csv2("./listas/clusters.csv", h = TRUE) 

library(plyr) 
dados <- rename(dados, replace = c('nparcs' = 'mi')) 
dados <- rename(dados, replace = c('volume'= 'yi')) 
names(dados)

#Informações gerais

#número de clusters medidos em campo
(n <- length(dados$cluster)) 
#número de clusters cabíveis na área
(N <- dados$ncluster[1]) 
#número de elementos M não é conhecido
sig <- 0.10

#Como o número M não é conhecido...
#total populacional
(Y <- N/n * sum(dados$yi))

#variância da população
(ymed <- mean(dados$yi))
#m^6
(s2 <- (N^2) * ((N - n)/ N) * (1/n) * sum((dados$yi - ymed)^2)/ (n - 2)) 

#erro padrão populacional
(s <- sqrt(s2)) 

#erro do inventário
(erro <- qt(1 - sig/2, n - 1)*s) 

#erro em porcentagem
(erroperc <- erro/Y*100)

#Intervalo de confiança
paste('IC (m³):', round(Y-erro), '<=total estimado pela ACE<=', round(Y+erro), 'para n.s.=', sig * 100,'%')

