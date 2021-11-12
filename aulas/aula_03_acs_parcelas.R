#AMOSTRAGEM CASUAL SIMPLES

dados <- read.csv2('parcelas.csv');
View(dados)
names(dados)

#Variável de interesse
y <- dados$vcomcc

#Média
(ymed <- mean(y))

#Variância
(yvar <- var(y))

#Desvio padrão
(ydesv <- sd(y))
#ou
(ydesv <- sqrt(yvar))

#Tamanho da amostra
(n <- length(y))

#Área da parcela
(areaparc <- mean(dados$areaparc))

#Área da fazenda
talhao <- subset(x = dados, 
                 subset = !duplicated(talhao), 
                 select = c('talhao','area'))
View(talhao)
(areafaz <- sum(talhao$area))

#ou
(areafaz <- sum(talhao$area[!duplicated(dados$talhao)]))

#Intensidade amostral
(ia <- areafaz/n)

#Número de parcelas cabíveis
(N <- areafaz*10000/areaparc)

#Variância da media
(yvarmed <- (yvar/n) * (1-n/N))

#Erro padrão da média
(ydesvmed <- sqrt(yvarmed))

#Erro do inventário
(erroinv <- qt(0.975,n-1) * ydesvmed)

#Erro do inventário em porcentagem

(erroinvperc <- erroinv/ymed *100)

#Média por ha
(yha <- ymed * (10000/areaparc))
#Erro inventário por ha
(erroinvha <- erroinv * (10000/areaparc))

#Total populacional
(ytot <- yha * areafaz)
#Erro inventário para a população
(erroinvtot <- erroinvha * areafaz)

#Intervalo de confiança para a população
cat(paste(round(ytot-erroinvtot,0),'<= total <=', round(ytot+erroinvtot,0),'m³\n'));








