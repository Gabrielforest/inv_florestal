dados <- read.csv2('conglomerado.csv')
View(dados)

dados$mi <- dados$nsubunid
dados$yi <- dados$vol

#Definições
#N = Número de clusters cabíveis na população
#n = Número de clusters amostrados pela ACS
#M = Número de elementos cabíveis na população
#mi= Número de elementos no iésimo cluster
#mm= Tamanho médio dos clusters que compõem a amostra
#Mm= Tamanho médio dos clusters que compõem a população

N <- 506
M <- 2706
n <- nrow(dados)

#a) Estimador da população total quando o número
#de elementos (M) é conhecido
Mm <- M/N

# média de y por subunidade:
ym <- sum(dados$yi) / sum(dados$mi)

#a.1) Total populacional
#(m3/povoamento)
Y <- M * ym

#a.2) Variância da população

s2Mym <- (N^2) * ((N - n)/N) * (1/n) * sum((dados$yi - dados$mi * ym)^2) / (n - 1)

#a.3) Erro padrão da população
sMym <- sqrt(s2Mym)

#a.4) Erro do inventário

#a.4.1) Erro absoluto (m^3/população) para alpha = 5%
erroabs <- qt(0.975, n - 1) * sMym

#a.4.2) Erro em porcentagem
erroperc <- erroabs/Y * 100

#a.5)Intervalo de confiança
#li: limite inferior do IC
li <- Y - erroabs
#ls: limite superior do IC
ls <- Y + erroabs

cat(paste0("IC: ", round(li), " <= tau <= ", round(ls), " m³"))

#b) Estimador da população total quando o número
#de elementos (M) NÃO é conhecido

#b.1) Total populacional
#(m3/povoamento);
Y <- (N/n) * sum(dados$yi)

#b.2) Variância da população
ymed <- mean(dados$yi)

s2Nym <- (N^2) * ((N - n) / N) * (1/n)* sum((dados$yi - ymed)^2) / (n - 2)

#b.3) Erro padrão da população
sNym <- sqrt(s2Nym)

#b.4) Erro do inventário

#b.4.1) Erro absoluto
erroabs <- qt(0.975, n - 1) * sNym

#b.4.2) Erro em porcentagem
erroperc <- erroabs/Y*100

#b.5)Intervalo de confiança
#li: limite inferior do IC
li <- Y - erroabs
#ls: limite superior do IC
ls <- Y + erroabs

cat(paste0("IC: ", round(li), " <= tau <= ", round(ls), " m³"))



