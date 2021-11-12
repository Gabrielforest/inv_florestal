
# Para realizar o inventário pré-corte de um povoamento de 28ha de Eucalyptus grandis foi realizada a contagem de
# todas as árvores no talhão, o que totalizou 40535 indivíduos. Os resultados por parcela podem ser observados na
# tabela 1. (100%)
# 
# Tabela 1: Resultados por parcela do inventário florestal
# 
# Estrato     Área (ha) Total árvores Parcela Área parcela(m2)  N.Árv./parc  Volume (m3/parc)
# A             16         18755        1          363             45           4,59
#                                       2          363             44           4,83
#                                       3          363             42           4,83
#                                       4          363             44           4,33
#                                       5          363             42           4,28 
#                                       
# B             12         21780 
#                                       6          363             64           7,89
#                                       7          363             68           8,01
#                                       8          363             63           7,38
#                                       9          363             62           7,19
#                                       10         363             68           7,11
#                                       
# Encontre o erro amostral % e o intervalo de confiança para o volume total do povoamento (α = 7%) 
# utilizando os seguintes estimadores:
# (a) Estimador tradicional da amostragem casual simples.
# (b) Estimador de razão na amostragem casual simples.
# (c) Estimador tradicional da amostragem casual estratificada.

areafaz <- 28

total_arvores <- 18755 + 21780

dados <- data.frame(estrato = c(rep("A", 5), rep("B", 5)),
                    area = c(rep(16, 5), rep(12, 5)),
                    parcela = c(1:10),
                    area_parcela = 363,
                    n_arv_parc = c(45, 44, 42, 44, 42,
                                   64, 68, 63, 62, 68),
                    volume = c(4.59, 4.83, 4.83, 4.33, 4.28,
                               7.89, 8.01, 7.38, 7.19, 7.11)
)

areaparc <- dados$area_parcela[1]

# variável de interesse
y <- dados$volume

N <- areafaz*10000/areaparc


# a) Amostragem Casual Simples --------------------------------------------

estatisticas_acs <- as.data.frame(cmrinvflor::estats_acs(vy = y, nt = N, sig = 7))

estatisticas_acs$eperc

# média por hectare (m3/ha) 
yha <- estatisticas_acs$ymed * 10000 / areaparc

# Erro do inventário por hectare (m3/ha)
erroinvha <- estatisticas_acs$eunid * 10000 / areaparc 

# total populacional (m3/fazenda: total) #volume comercial
ytot <- yha * areafaz;

#Erro do inventário para a populaçao (m3/fazenda) 
erroinvpop <- erroinvha * areafaz

#Intervalo de confiança para a população (m3)
cat(paste(round(ytot-erroinvpop,0), " m3", "<= total populacional <=", 
          round(ytot+erroinvpop,0), " m3", sep=''))


# b) Estimador de Razão ACS -----------------------------------------------

# variável auxiliar = número de árvores por parcela:
x <- dados$n_arv_parc
  
# gráfico:
plot(x, y,
     xlim = c(0, max(x)), 
     ylim = c(0, max(y)),
     xlab = "área da parcela (ha)",
     ylab = "volume (m3)",
     pch = "*",
     col = "red")

ajlin <- lm(y ~ x)
lines(x, predict(ajlin))
summary(ajlin)

# b0 não foi significativo, reta crescente, e passa pela origem,
# portanto utilizarei a acs estimador de razão.

#Média estimada da variável auxiliar (x)
xme <- mean(x)

#Média populacional da variável auxiliar
xm <- total_arvores/N

#Média estimada da variável de interesse (y)
yme <- mean(y)

n <-length(y)

#Estimador de razão
R <- yme/xme 

#varr=Variância entre valores observados e os estimados
#     pelo estimador de razão
varr <- (sum( y^2 ) - 2 * R * sum(y * x) + R^2 * sum( x^2))/(n - 1)

# Cálculos com o estimador de razão ---------------------------------------

#Média por parcela (m3/parc)
ymraz <- R * xm

#Variância da média estimada
varmedia <- (1 - n / N)* (1 / n) * varr

#Erro padrão da média
erro_pad_media <- sqrt(varmedia)

#Erro do inventário (m3/parc) - alpha de 7%:
erro_inv <- qt(1 - (0.07/2), n - 1) * erro_pad_media

#Erro do inventário (%)
erro_inv_perc <- erro_inv/ ymraz * 100

#Total populacional (m3)
ytraz <- ymraz * N 

ext_ha <- 10000/dados$area_parc[1]

erroinvtot <- erro_inv * ext_ha * areafaz

#Intervalo de confiança para a população
cat(paste(
  round(ytraz - erroinvtot, 0),'<= total <=', 
  round(ytraz + erroinvtot, 0),'m³\n')
)


# c) Estimador ACE --------------------------------------------------------


#duplicando valores
dados$vary <- dados$volume 

#alpha = 7%
sig <- 0.07

#Médias por estrato
estrato <- with(dados,
                aggregate(
                  list(areaest = area, 
                       areaparc = area_parcela,
                       ym = vary),
                  list(estrato = estrato),
                  mean
                ))

#Número de parcelas por estrato
calc <- with(dados,
             aggregate(
               list(anj = area_parcela),
               list(estrato = estrato),
               length
))

estrato <- merge(estrato, calc)

#Variância e desvio padrão por estrato
calc <- with(dados,
             aggregate(
               list(s2y = vary),
               list(estrato = estrato),
               var
))

calc$sy <- sqrt(calc$s2y)

estrato <- merge(estrato, calc)

#Número de parcelas cabíveis por estrato
estrato$pnj <- with(estrato, areaest*10000/areaparc)

#Nomeando a população
estrato$populacao <- 1

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
estrato$pwj <- with(estrato, pnj/pn)

#Cálculo da média estratificada
calc <- with(estrato,
             aggregate(
               list(ymstr = pwj*ym),
               list(populacao = populacao),
               sum
             ))

populacao <- merge(populacao, calc)

#Variância da média estratificada
calc <- with(estrato,
             aggregate(
               list(calc1 = (pwj^2)*s2y/anj, 
                    calc2 = (pwj*s2y)/pn),
               list(populacao = populacao),
               sum
             ))

calc$s2ystr<- calc$calc1 - calc$calc2
calc$calc1 <- NULL
calc$calc2 <- NULL

populacao <- merge(populacao, calc)

#Cálculo do grau de liberdade efetivo
estrato$calcgl <- with(estrato, pnj * (pnj - anj)/anj)

calc <- with(estrato,
             aggregate(
               list(calc1 = calcgl * s2y, 
                    calc2 = (calcgl*s2y)^2 / (anj - 1)),
               list(populacao = populacao),
               sum
             ))

calc$gle <- with(calc, calc1^2/calc2)
calc$calc1 <-NULL
calc$calc2 <-NULL

populacao <- merge(populacao, calc)
populacao$systr <- sqrt(populacao$s2ystr)

populacao$errounid <- with(populacao, qt(1-sig/2,gle)*systr)
populacao$erroperc <- with(populacao, errounid/ymstr*100)
populacao$erroperc

total <- with(populacao, ymstr * pn)
etotal <- with(populacao, errounid * pn)

##Intervalo de confiança da população
litot <- total - etotal
lstot <- total + etotal

print(paste('IC:',round(litot),'<=T<=',round(lstot),'m³',sep=''))


estrato <- subset(dados,
                  !duplicated(estrato),
                  select = c("estrato", "area"))
amostra <- dados[, c('estrato', 'parcela', 'area_parcela', 'vary')]

ace <- as.data.frame(cmrinvflor::estats_ace(estrato, amostra, sig=sig*100, fc_dim=1/10000))
totace <- with(ace, ymstr*np)
errototace <- with(ace, eunid*np)
litotace <- totace-errototace
lstotace <- totace+errototace

print(paste('IC:',round(litotace),'<=T<=',round(lstotace),'m³',sep=''));










