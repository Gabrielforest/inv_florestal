### Gabriel de Freitas Pereira

# lista 2

# A planilha de dados em anexo (“parcelas.csv”) contém os resultados por parcela, já extrapolados para hectare, de um
# inventário florestal realizado em um povoamento de Eucalyptus sp. Como o povoamento é heterogêneo, divida a área
# em 02 estratos, de forma que, os coeficientes de variação dos estratos sejam inferiores ao coeficiente de variação de
# toda a base. Explique qual foi o critério de estratificação e calcule as seguintes estatísticas para a variável volume
# comercial sem casca (vcomsc): (100%)


# População - média [m3/ha]
# População - desvio padrão [m3/ha]
# População - coeficiente de variação [%]
# População - erro padrão da média [m3/ha]
# População - erro do inventário em m3. Considere alpha = 5%
# População - erro do inventário em %
# População - limite inferior do intervalo de confiança [m3/ha]
# População - limite superior do intervalo de confiança [m3/ha]



# lendo os dados ----------------------------------------------------------


dados <- read.csv2("listas/lista2_parcelas.csv")

# variável de interesse
y <- dados$vcomsc

#Área da parcela
areaparc <- mean(dados$areaparc)

# area da fazenda
areafaz <- sum(dados$area [!duplicated(dados$talhao)] )

#Número de parcelas cabíveis
N <- areafaz * 10000 / areaparc

# Amostragem casual simples -----------------------------------------------

estats_pop <- as.data.frame(cmrinvflor::estats_acs(vy = y, nt = N, sig = 1))



# dividindo em estratos por idade: --------------------------------------



# estrato 1:

dados_estrato1 <- subset(x = dados,
                           idade > 5)

# variável de interesse
y1 <- dados_estrato1$vcomsc

#Área da parcela
areaparc1 <- mean(dados_estrato1$areaparc)

# area da fazenda
areafaz1 <- sum(dados_estrato1$area [!duplicated(dados_estrato1$talhao)] )

#Número de parcelas cabíveis
N1 <- areafaz1 * 10000 / areaparc1

estats_estrato1 <- as.data.frame(cmrinvflor::estats_acs(vy = y1, nt = N1, sig = 1))




# estrato 2:

dados_estrato2 <- subset(x = dados,
                         idade <= 5)

# variável de interesse
y2 <- dados_estrato2$vcomsc

#Área da parcela
areaparc2 <- mean(dados_estrato2$areaparc)

# area da fazenda
areafaz2 <- sum(dados_estrato2$area [!duplicated(dados_estrato2$talhao)] )

#Número de parcelas cabíveis
N2 <- areafaz2 * 10000 / areaparc2

estats_estrato2 <- as.data.frame(cmrinvflor::estats_acs(vy = y2, nt = N2, sig = 1))




# ymed = Média
# na = Número de amostras
# s2dy = variância
# sdy = desvio padrão
# cv = coeficiente de variação em porcentagem
# sdymed = erro padrão da média
# eunid = erro na unidade da variável de interesse
# eperc = erro em porcentagem
# li = limite inferior
# ls = limite superior
# sa = suficiência amostral para o erro máximo pré estabelecido


# Estimativa da intensidade amostral 
cmrinvflor::nacs(vy = y, eperc = 5, sig = 5)
