#AMOSTRAGEM CASUAL SIMPLES - HIPSOMETRIA GENÉRICA

arv <- read.csv2('fustes.csv')
View(arv)
names(arv)


#Hipsometria genérica ---------------------------------------------------


#arv[is.na(arv)]<-0;

modelo <- 'log(ht)~I(1/dap) + I(log(hd))'
selarv <- subset(arv, !is.na(ht) & dap > 0)
ajhipso <- lm(modelo, selarv)

sumario <- summary(ajhipso)

y <- arv$ht[!is.na(arv$ht)]
y <- selarv$ht
D(expression(log(y)),'y'); #Derivada
dy <- 1/y

(medgeo<-exp(mean(log(dy))));

#sumario$sigma = Erro padrão residual

# Índice de Furnival em metros 
(IF <- 1/medgeo * sumario$sigma
# Índice de furnival em porcentagem
(IFperc <- IF/mean(y) * 100)


#Estimando as alturas totais
(bs <- as.vector(coef(ajhipso)))

arv$htest <- exp(bs[1] + bs[2] / arv$dap + bs[3] * log(arv$hd))
#OU
arv$htest <- with(arv, exp(bs[1] + bs[2] / dap + bs[3] * log(hd)))
#OU
arv$htest <- as.vector(exp(predict(ajhipso, newdata = arv)))

#arv$htest[arv$dap==0|is.na(dap)]<-0

arv$htre <- arv$ht
ii <- is.na(arv$ht) | arv$ht == 0
# pega as posições que é verdadeiro (na ou igual a 0) e faz a atribuição:
arv$htre[ii] <- arv$htest[ii]

arv$vicc <- with(arv, 6.436159e-05 * dap^1.852143 * htre^9.530665e-01)

names(arv)

library(dplyr)
parc <- 
  arv %>% 
  group_by(fazenda, 
           talhao, 
           areatal, 
           parcela, 
           areaparc) %>% 
  summarise(vol = sum(vicc))

# OU
# group vol by fazenda, talhao, areatal, parcela, areaparc
parc1 <- aggregate(list(vol = arv$vicc),
                 list(fazenda = arv$fazenda,
                      talhao = arv$talhao,
                      areatal = arv$areatal,
                      parcela = arv$parcela,
                      areaparc = arv$areaparc
                 ),sum)
View(parc)

#Variável de interesse
y <- parc$vol

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
(areaparc <- mean(parc$areaparc))

#Área da fazenda
talhao <- subset(parc,!duplicated(talhao), c('talhao','areatal'))
View(talhao)
(areafaz <- sum(talhao$areatal))

#Intensidade amostral
(ia <- areafaz/n);

#Número de parcelas cabíveis
(N <- areafaz * 10000/areaparc)

#Variância da media
(yvarmed <- (yvar/n)*(1-n/N))

#Erro padrão da média
(ydesvmed <- sqrt (yvarmed))

#Erro do inventário
(erroinv <- qt(0.975,n-1) * ydesvmed)

#Erro do inventário em porcentagem

(erroinvperc<-erroinv/ymed *100)

#Média por ha
(yha <- ymed*(10000/areaparc))
#Erro inventário por ha
(erroinvha <- erroinv*(10000/areaparc))

#Total populacional
(ytot <- yha * areafaz)
#Erro inventário para a população
(erroinvtot <- erroinvha * areafaz);

#Intervalo de confiança para a população

cat(paste(round(ytot - erroinvtot, 0),'<= total <=',
          round(ytot + erroinvtot, 0),'m³\n'))



