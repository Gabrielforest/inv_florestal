##Amostragem casual estratificada
dados <- read.csv2("./trabalho_final/hipso_parcela.csv")
names(dados)


#variável de interesse chamada de vary
dados$vary <- dados$vprod_6 

#conhecendo possíveis estratificações
#3 materiais
matgen <- subset(dados, !duplicated(matgen), c("matgen")) 

calc <- subset(dados, !duplicated(codplantio), c("codplantio", "areaplantio", "matgen"))


mat1 <- subset(calc, matgen == 26)
areamat1 <- sum(mat1$areaplantio)

mat2 <- subset(calc, matgen == 35)
areamat2 <- sum(mat2$areaplantio)

mat3 <- subset(calc, matgen == 53)
areamat3 <- sum(mat3$areaplantio)

areamattot <- c(areamat1, areamat2, areamat3)
areatot <- sum(areamattot)

dados$areaest <- NA
dados$areaest[dados$matgen == 26] <- areamat1
dados$areaest[dados$matgen == 35] <- areamat2
dados$areaest[dados$matgen == 53] <- areamat3

#escolhendo estratificação
dados$estrato = NULL
dados$estrato = dados$matgen


dados$vary <- dados$vprod_6; 

names(dados)
#VOLUME DE INDIVíDUO SOMADO P/ OBTER VOLUME POR PARCELA
dados2 <- with(dados, aggregate(list(vary = vary),
                             list(areaest = areaest, areaparc = areaparc, estrato = estrato, parcela = parcela),sum));

#alpha=5%
sig <- 0.05 

#Médias por estrato
estrato <- with(dados2,
                aggregate(
                  list(areaest = areaest, 
                       areaparc = areaparc,
                       ym = vary),
                  list(estrato = estrato),
                  mean
                ))

#Número de parcelas por estrato
calc <- with(dados2,
             aggregate(
               list(anj = parcela),
               list(estrato = estrato),
               length
             ))

estrato <- merge(estrato, calc)

#Variância e desvio padrão por estrato
calc <- with(dados2,
             aggregate(
               list(s2y = vary),
               list(estrato = estrato),
               var
             ))

calc$sy <- sqrt(calc$s2y)

estrato <- merge(estrato, calc)

#Número de parcelas cabíveis por estrato
estrato$pnj <- with(estrato, areaest * 10000/areaparc)

#Nomeando a população
estrato$populacao <- "Fazenda CH"

#Área, número de amostras e número de amostras cabíveis na
#população

populacao <- with(estrato,
                  aggregate(
                    list(area = areaest,
                         an = anj,
                         pn = pnj),
                    list(populacao = populacao),
                    sum
                  ))

estrato <- merge(populacao, estrato)

#Peso de cada estrato
estrato$pwj <- with(estrato, anj/an)

#Cálculo da média estratificada

calc <- with(estrato,
             aggregate(
               list(ymstr = pwj * ym),
               list(populacao = populacao),
               sum
             ))

populacao <- merge(populacao, calc)

#Variância da média estratificada
calc <- with(estrato,
             aggregate(
               list(calc1 = (pwj^2) * s2y/anj, 
                    calc2 = (pwj * s2y) / pn),
               list(populacao = populacao),
               sum
             ))

calc$s2ystr <- calc$calc1 - calc$calc2
calc$calc1 <- NULL
calc$calc2 <- NULL

populacao <- merge(populacao, calc)

#Cálculo do grau de liberdade efetivo
estrato$calcgl <- with(estrato, pnj * (pnj - anj)/ anj)

calc <- with(estrato,
             aggregate(
               list(calc1 = calcgl * s2y,
                    calc2 = (calcgl * s2y)^2 / (anj - 1)),
               list(populacao = populacao),
               sum
             ))

calc$gle <- with(calc, calc1^2 / calc2)
calc$calc1 <- NULL
calc$calc2 <- NULL

populacao <- merge(populacao, calc)
populacao$systr <- sqrt(populacao$s2ystr)

#Erro na unidade
populacao$errounid <- with(populacao, qt(1 - sig/2, gle) * systr)

#Erro percentual
populacao$erroperc <- with(populacao, errounid/ymstr * 100)

#Total e erro por hectare
populacao$ytstr <- with(populacao, ymstr * pn)
populacao$errototal <- with(populacao, errounid * pn)

#Total e erro por hectare
populacao$ymha <- with(populacao, ytstr/area)
populacao$erroha <- with(populacao, errototal/area)

View(populacao)
write.csv2(populacao,"./trabalho_final/ACE_calculada.csv", row.names = F)





#Volume/ha
dados$vprod_6_ha <- dados$vprod_6 * 10000/dados$areaparc
View(dados)

#para calcular o volume medio m³/ha 
vmed_ha <- mean(dados$vprod_6_ha)

#Variável de interesse
y <- dados$vprod_6

#Média (m?/parcela)
(ym <- mean(y))

#Tamanho da amostra
(n <- length(y))

#Área parcela
#Não compensa trabalhar com parcelas com tamanhos desiguais
(areaparc <- mean(dados$areaparc))


#Área da fazenda  
talhao <- subset(dados,!duplicated(codplantio), c("codplantio","areaplantio")); #Subset - Pegar apenas uma informa??o por parcela
View(talhao)
(areafaz <- sum(talhao$area))

#Média por ha (m³/ha)
(yha <- ym*(10000/areaparc))

#Total populacional (m³)
(ytot <- yha * areafaz)

#Erro inventário por ha (m³/ha)
(erroinvha <- populacao$errounid * (10000/areaparc))

#Intervalo de Confiança m³/parcela
cat(paste(round(ym - populacao$errounid, 2), " a ", round(ym + populacao$errounid, 2),"m³/ha com 95% de confiança", sep = ""))

#Intervalo de confiança em m³/ha
cat(paste(round(yha - erroinvha, 2)," a ",round(yha + erroinvha, 2),"m³/ha com 95% de confiança", sep = ""))

#Intervalo de confiança em m³/povoamento
cat(paste(round((ytot - ytot * populacao$erroperc/100), 2),
          " a ",
          round((ytot + ytot * populacao$erroperc/100), 2),
          "m³/fazenda com 95% de confiança", sep = ""))



