
# AJUSTE E SELEÇÃO DE MODELOS LINEARES SEM TRANSFORMAÇÃO DA VARIÁVEL --------

dados <- read.csv2('cubagem.csv')
View(dados)
names(dados)

# volume individual com casca = vicc
# volume individual sem casca = visc
# ht = altura total
# dap = diametro altura do peito
# arv = árvore

#Correlacao
cor(dados)

cor(dados[
  , 
  c("dap", "vicc", "ht")
  ]
)

# Ou chamar uma correlação direta através de:
cor(dados$dap, dados$vicc)


# Modelo de Spurr - modelo linear simples. Usado para estimar volumes com B0 e B1:
# I = variável independente

modelo <- "vicc ~ I(dap^2 * ht)"

# atribuindo a fórmula do modelo aos dados
ajlin <- lm(modelo, dados)

# extraindo os coeficientes de um modelo:
coef(ajlin)

sumario <- summary(ajlin)



# Quanto os valores obs variam dos estimados ------------------------------

# erro padrão residual (m^3)
syx <- sumario$sigma

# erro padrão residual em porcentagem (%)
syxp <- syx/mean(dados$vicc)*100



#Quanto as variações de uma variável explica as variações da outra variável ----

# coeficiente de determinação
R2adj <- sumario$adj.r.squared 

# coeficiente de determinação (%)
R2adjp <- round(R2adj * 100, 2)



# Análise de um modelo completo  ------------------------------------------

# análise de variância (só usado com modelos lineares)
anova(ajlin)



# Calculando os valores preditos ------------------------------------------

dados$vicc_est <- predict(ajlin)

#OU

# bs são os betas:
bs <- as.vector(coef(ajlin))
dados$vicc_est <- with(dados, bs[1] + bs[2] * dap^2 * ht)



# Calculando os resíduos --------------------------------------------------

dados$res <- residuals(ajlin)

#OU

dados$res <- dados$vicc - dados$vicc_est




# Gráficos ----------------------------------------------------------------


plot(dados$dap, dados$vicc, xlab = 'dap(cm)', ylab='vicc(m³)', pch = '*',col = 'green')

points(dados$dap, dados$vicc_est, pch = '*', col = 'red')

legend('topleft', legend = c('vicc_obs','vicc_est'),
       pch = c('*','*'), col = c('green','red'), bty='n',
       text.col = c('green','red'))


#Dividir janela gráfica

par(mfrow=c(2,2)); 

with(dados,plot(vicc_est, res,pch='*',
                xlab = 'Volume estimado(m³)',
                ylab = 'Resíduos (m³)',
                col = 'red'));
abline(h=0);

library(fBasics)

qqnormPlot(dados$res)
#qqnorm(dados$res);

# Normalidade: Anderson-Darling
# nortest::ad.test(dados$res)$p.value;

with(dados,plot(vicc_est,rstudent(ajlin),pch='*',
                xlab='Volume estimado(m³)',
                ylab='Resíduos studentizados',
                col='red',ylim=c(-2.5,2.5)));

abline(h=0);

#Inversa da distribuição de t
tinf <- qt(0.025, nrow(dados) - 1)

#t(alpha=5 e [n-1] gl)  
tsup <- qt(0.975, nrow(dados) - 1)

abline(h=tinf,lty = 2);
abline(h=tsup,lty = 2);

#Inversa da distribuição de t
tinf <- qt(0.005, nrow(dados) - 1)

#t(alpha=5 e [n-1] gl)
tsup <- qt(0.995, nrow(dados)- 1)  


abline(h = tinf, lty = 2, col = 'blue')
abline(h = tsup, lty = 2, col = 'blue')

hist(dados$res, probability = TRUE,
     main = '', ylab = 'densidade', xlab = 'resíduos(m³)',
     col = 'red');

mres <- mean(dados$res);
sdres <- sd(dados$res);
x <- dados$res;
curve(dnorm(x,mres,sdres), col = 'darkgreen', add = T);

###Opções para remoção de outliers

dados$rstudent <- rstudent(ajlin)

View(dados)
View(subset(dados, rstudent < tinf | rstudent > tsup))

#dados<-subset(dados,arv!=13 & arv!=34);
dados_sem_outlier <- subset(dados, rstudent >= tinf & rstudent <= tsup)

identify(dados$vicc_est, rstudent(ajlin))
