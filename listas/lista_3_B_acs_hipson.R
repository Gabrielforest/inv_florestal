# A planilha de dados “fustes.csv” contém os dados de campo de um inventário florestal realizado no Sul do Estado
# de São Paulo em um povoamento de Eucalyptus spp. A planilha “cubagem.csv” contém os resultados por árvore da
# cubagem rigorosa realizada neste mesmo povoamento. Diante de uma gama de modelos volumétricos e hipsométricos
# disponíveis na literatura, para o processamento deste inventário, foram pré-selecionados os seguintes modelos:

# (a) Modelo hipsométrico genérico (Um único ajuste para todo o povoamento):
#   
#   ln (ht) = β0 + β1 × ln (hd) + β2 × (1/dap) (1)

# (b) Modelo volumétrico:
#   
#   ln (vcom) = β0 + β1 × ln (dap) + β2 × ln (ht) (2)

# Sabendo-se que o plano amostral adotado foi a amostragem casual simples e que a variável de interesse é o volume
# comercial sem casca (vcomsc), processe o inventário florestal e responda os itens abaixo. Considere α = 1%. (100%)

# (a) Hipsometria:
# β0
# β1
# β2
# Ind. Furnival[m]
# Ind. Furnival[%]
# R2
# aj [%]

# (b) Volumetria:
# β0
# β1
# β2
# Ind. Furnival[m3              ]
# Ind. Furnival[%]
# R2
# aj [%]

# (c) Estatísticas por parcela:
# Média [m3/parc]
# Erro do inventário em m3/parc.
# Erro do inventário em %
# Limite inferior do intervalo de confiança [m3/parc]
# Limite superior do intervalo de confiança [m3/parc]

# (d) Estatísticas por hectare:
# Média [m3/ha]
# Limite inferior do intervalo de confiança [m3/ha]
# Limite superior do intervalo de confiança [m3/ha]

# (e) Estatísticas do povoamento:
# Total [m3]
# Limite inferior do intervalo de confiança [m3]
# Limite superior do intervalo de confiança [m3]



# Lendo os dados ----------------------------------------------------------

arv <- read.csv2("listas/lista3_B_fustes.csv")
cub <- read.csv2("listas/cubagem_lista3_B.csv")


# Criando coluna de DAP ---------------------------------------------------

arv$dap <- arv$cap/pi 


# Modelo hipsométrico -----------------------------------------------------

#ln (ht) = β0 + β1 × ln (hd) + β2 × (1/dap)

modelo <- 'log(ht) ~  I(log(hdom)) + I(1/dap)'

# árvores mensuradas:
selarv <- subset(arv, !is.na(ht) & dap > 0 & ht > 0)

# ajuste do modelo:
ajhipso <- lm(modelo, selarv)

# resumo:
sumario <- summary(ajhipso)
coef(sumario)

#Indice de Furnival
y <- arv$ht[!is.na(arv$ht) & arv$ht > 0]

D(expression(log(y)), 'y')

dy <- 1/y

medgeo <- exp(mean(log(dy)))

# Indice de Furnival [m]
IF <- 1/medgeo * sumario$sigma

# Indice de furnival em porcentagem
IF_perc <- IF/mean(y) * 100 

# coeficiente de determinação
R2adj <- sumario$adj.r.squared 

# coeficiente de determinação (%)
R2adjp <- round(R2adj * 100, 2)



# Criando coluna de dados estimados de altura e real ----------------------

# Estimando as alturas totais através do modelo ajustado:
arv$htest <- exp(predict(ajhipso, newdata = arv))

# coluna de dados reais medidos
arv$htre <- arv$ht

# vetor lógico de árvores mensuradas ou não
ii <- is.na(arv$ht) | arv$ht == 0

# preenchendo a coluna onde o vetor lógico é TRUE (não há árvores medidas):
arv$htre[ii] <- arv$htest[ii]



# Modelo volumétrico ------------------------------------------------------

# ln (vcom) = β0 + β1 × ln (dap) + β2 × ln (ht) 

modelovol <- "log(vcomsc) ~ I(log(dap)) + I(log(ht))"

# árvores cubadas:
selcub <- subset(cub, !is.na(ht) & dap > 0 & ht > 0)

# ajuste do modelo:
ajvol <- lm(modelovol, selcub)

# resumo:
volume <- summary(ajvol)
coef(volume)

# Furnival
x <- cub$vcomsc
D(expression(log(x)),'x')
dx <- 1/x

medgeov <- exp(mean(log(dx)))

# Indice de Furnival [m]
furnival <- 1/medgeov * volume$sigma

# Indice de furnival em porcentagem
furnivalperc <- furnival/mean(x) * 100

# R2 ajustado:
r2 <- volume$adj.r.squared

# coeficiente de determinação (%)
R2adjp_vol <- round(r2 * 100, 2)

# Estimando os volumes
bv <- as.vector(coef(ajvol))
arv$vcomscest <- exp(bv[1] + bv[2] * log(arv$dap) + bv[3] * log(arv$htre)) # estimativa pelo modelo volumétrico
#arv$vicc<-with(arv,6.436159e-05*dap^1.852143*htre^9.530665e-01), # estimativa pela formula do volume



# Estatísticas por parcela ------------------------------------------------

# agrupando os dados de volume por parcela:
parc <- arv %>% 
  group_by(
    talhao,
    area,
    parcela,
    areaparc) %>% 
  summarize(vol = sum(vcomscest))


#Variável de interesse
z <- parc$vol

#Média m³/parcela
zmed <- mean(z)

#Variância m6/parc 
zvar <- var(z)

#Desvio padrão
zdesv <- sd(z)
#ou
zdesv <- sqrt(zvar)

#Tamanho da amostra
n <- length(z)

#Área da parcela
areaparc <- mean(parc$areaparc)

#Área da fazenda
talhao <- subset(parc, !duplicated(talhao), c("talhao", "area"))
areafaz <- sum(talhao$area)

#Intensidade amostral
ia <- areafaz/n

#Número de parcelas cabíveis
N <- areafaz*10000/areaparc

# assim sai os resultados:
as.data.frame(cmrinvflor::estats_acs(vy = z, nt = N, sig = 1))

#OU:

#Variância da media
zvarmed <- (zvar/n) * (1-n/N)

#Erro padrão da média
zdesvmed <- sqrt(zvarmed)

#Erro do inventário
erroinv <- qt(0.995,n-1) * zdesvmed

#Erro do inventário em porcentagem

erroinvperc <- erroinv/zmed *100

#Intervalo de confiança por ha
cat(paste(round(zmed - erroinv, 2), '<= total <=',
          round(zmed + erroinv, 2), 'm³/ha\n'));



# Estatísticas por hectare ------------------------------------------------


#Média por ha
zha <- zmed*(10000/areaparc)

#Erro inventário por ha
erroinvha <- erroinv * (10000/areaparc)

#Intervalo de confiança por ha
cat(paste(round(zha - erroinvha, 2),'<= total <=',
          round(zha + erroinvha, 2),'m³/ha\n'));



# Estatísticas populacionais ----------------------------------------------


#Total populacional
ztot <- zha * areafaz
#Erro inventário para a população
erroinvtot <- erroinvha * areafaz

#Intervalo de confiança para a população
cat(paste(round(ztot - erroinvtot, 2), '<= total <=',
          round(ztot + erroinvtot, 2), 'm³\n'))






