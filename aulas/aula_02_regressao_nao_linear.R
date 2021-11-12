#AJUSTE E SELEÇÃO DE MODELOS NÃO LINEARES

dados <- read.csv2('cubagem.csv')
names(dados)


#Modelo de Schumacher & Hall
modelo <- 'vicc ~ b0 * dap^b1 * ht^b2'

# ajuste não linear (nls)
(ajnlin <- 
    nls(
      modelo,dados,
      start = list( b0 = pi/40000 * 0.45,
                    b1 = 2,
                    b2 = 1)
    )
)

coef(ajnlin)

(sumario <- summary(ajnlin))

#Erro padrão residual:
(syx <- sumario$sigma)

#Syx percentual
(syxperc <- syx/mean(dados$vicc) * 100)

cat(paste('Erro padrão residual: ', round(syxperc, 2), '%', sep = ''))

plot(dados$dap, 
     dados$vicc,
     xlab = 'dap(cm)',
     ylab = 'vicc(m³)',
     pch = '*',
     col = 'green')

points(dados$dap,
       predict(ajnlin),
       pch = '*',
       col = 'red')

legend('topleft',
       legend = c('vicc_obs','vicc_est'),
       pch = c('*', '*'),
       col = c('green','red'),
       bty = 'n',
       text.col = c('green','red')
       )

#Calculando os valores preditos
(dados$vicc_est <- predict(ajnlin))
#OU
(bs <-  as.vector(coef(ajnlin)))
dados$vicc_est <- with(dados, bs[1] * dap^bs[2] * ht^bs[3])
#*****

#Calculando os residuos
dados$res <- residuals(ajnlin)
#OU
dados$res <- dados$vicc-dados$vicc_est
#*****


par(mfrow=c(2,2))

with(dados,
     plot(vicc_est, res, pch = '*',
          xlab = 'Volume estimado(m³)',
          ylab = 'Resíduos (m³)',
          col = 'red')
     )

abline(h=0)

library(fBasics);
qqnormPlot(dados$res);
#qqnorm(dados$res);

dados$res_padronizado <- dados$res/sumario$sigma;

with(dados,plot(vicc_est,
                res_padronizado,
                pch = '*',
                xlab = 'Volume estimado(m³)',
                ylab = 'Resíduos padronizados',
                col = 'red',ylim=c(-2.5,2.5)
                )
     )

abline(h=0);

#Inversa da distribuição de t
(tinf <- qt(0.025,nrow(dados)-1))

#t(alpha=5 e [n-1] gl)  
(tsup <- qt(0.975,nrow(dados)-1)) 

abline(h = tinf, lty = 2)
abline(h = tsup, lty = 2)

#Inversa da distribuição de t
(tinf <- qt(0.005, nrow(dados)-1))
#t(alpha=5 e [n-1] gl)  
(tsup <- qt(0.995, nrow(dados)-1)) 

abline(h = tinf, lty = 2, col = 'blue')
abline(h = tsup, lty = 2, col = 'blue')


hist(dados$res, 
     freq = F,
     main = '',
     ylab = 'densidade',
     xlab = 'resíduos(m³)',
     col = 'red')

# inserindo a curva normal:
mres <- mean(dados$res)
sdres <- sd(dados$res)
x <- dados$res
curve(dnorm(x,mres,sdres), col = 'darkgreen',add = T)

###Opções para remoção de outliers

dados$res_padronizado <- dados$res/sumario$sigma

View(
  subset(
    dados, 
    res_padronizado < tinf | res_padronizado > tsup)
  )

# dados <- subset(dados,
#                 arv!=13 & arv!=34)


with(dados,
     plot(
       vicc_est,res_padronizado,
       pch = '*',
       xlab = 'Volume estimado(m³)',
       ylab = 'Resíduos padronizados',
       col = 'red',ylim=c(-2.5,2.5)
       )
     )

abline(h = 0)

vetor_indice <- identify(dados$vicc_est, dados$res_padronizado)

dados <- dados[- vetor_indice, ]


