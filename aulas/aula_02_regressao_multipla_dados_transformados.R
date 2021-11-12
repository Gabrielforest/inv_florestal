#AJUSTE E SELEÇÃO DE MODELOS LINEARES MÚLTIPLOS COM VARIÁVEL DEPENDENTE TRANSFORMADA


# Carregando os Dados -----------------------------------------------------

dados <- read.csv2('cubagem.csv')

# Modelo de Schumacher & Hall (logarítmico)  ln(vicc) = b0 + b1 * ln(dap)+ b2 * ln(ht)
# modelo centrado pra média é o que utilizaremos em Inventário 100pre:
modelo <- 'log(vicc) ~ log(dap) + log(ht)'

### Ajuste de modelos lineares múltiplos
ajsch <- lm(formula = modelo, data = dados)

# h0 os betas são iguais a 0, nesse caso os betas são diferentes de zero (significativos)
sumario <- summary(ajsch)

anova(ajsch)


## Índice de Furnival 
## Para o cálculo do IF deve-se calcular a inversa da média geométrica da derivada da 
## variável dependente e, em seguida,
## multiplicar pelo erro padrão residual obtido no ajuste com a variável transformada

y <- dados$vicc

# derivada de y em função de y
D(expression(log(y)), 'y')
dy <- 1/y



# Média geométrica
medgeo <- exp(mean(log(dy), na.rm = T))

#OU

# Média geométrica
medgeo <- prod(dy)^(1/length(dy))

#OU

library(psych)
medgeo <- geometric.mean(dy,na.rm=TRUE)

# obs. nós iremos utilizar a média geométrica (o primeiro método demonstrado acima)


# escala convertida de lnm^3 para m^3
furnival <- 1/medgeo*sumario$sigma

furnival_perc <- furnival/mean(y) * 100



##Fator de correção da discrepância logarítmica de Meyer para as estimativas das variáveis dependentes
fc <- exp(0.5 * sumario$sigma^2)
viccest <- exp(predict(ajsch))* fc


##Análise gráfica dos resíduos

par(mfrow=c(1,2))

residuos <- dados$vicc-viccest

#vest<-exp(predict(ajsch));
#residuos<-dados$vicc-vest;

# gráfico com correção: (os gráficos são iguais sem essa correção)
plot(viccest, 
     residuos,
     pch = '*', 
     col = 'blue',
     xlab = 'vicc estimado [m³]',
     ylab = 'erro[m³]'
     )

# eixo para facilitar visualização:
abline(h = 0, lty = 2)

# gráfico sem correção:
plot(viccest,
     rstudent(ajsch),
     pch = '*', 
     col = 'blue',
     xlab = 'vicc estimado [m³]',
     ylab = 'erro studentizado'
     )

abline(h = 0, lty = 2)

(tinf <- qt(0.025, nrow(dados)-1))
(tsup <- qt(0.975, nrow(dados)-1))

abline(h = tinf, lty = 3, col = 'red')
abline(h = tsup, lty = 3, col = 'red')

# indicar os pontos estranhos (clicar nos pontos): 
vetor_indice <- identify(exp(predict(ajsch)), rstudent(ajsch))

# apagando os dados selcionados:
dados <- dados[- vetor_indice, ]


# escala de todos os gráficos deve ser a mesma
