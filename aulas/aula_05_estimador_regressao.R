#ACS - ESTIMADOR DE REGRESSÃO

parc <- read.csv2('dados_raz_reg.csv')
View(parc)
names(parc)

comp_frag <- mean(parc$larg_frag)
larg_parc <- mean(parc$larg_parc)
area_frag <- mean(parc$areafrag)

#Número de parcelas cabíveis
nt <- comp_frag/larg_parc)

x <- parc$areaparc/10000)
y <- parc$vtcc

x11();
plot(x, y,
     xlim = c(0,max(x)), 
     ylim = c(0,max(y)),
     xlab = 'área da parcela(ha)',
     ylab = 'Vtcc (m³)',
     pch = '*', 
     col = 'darkblue');

aj <- lm(y ~ x)
lines(x, predict(aj))
summary(aj)

#Média estimada de x
xme <- (mean(x)))

#Média populacional da variável auxiliar
(xm <- area_frag/nt)

#Média estimada de y
(yme <- mean(y))

#Número de amostras
(n <- length(y))

(b <- sum((y - yme) * (x - xme))/ sum((x - xme)^2))
#OU
(b <- as.vector(coef(lm(y ~ x))[2]))

#varyx: Porção da variância de y que não pode ser explicada pela relação
#linear com x
varyx <- sum((y- yme - b * (x - xme))^2) / (n-2)

#Média da parcela
(ymreg <- yme + b * (xm - xme))

#Variância da média estimada
(varmedia <- (1 - n/nt) * (1/n) * varyx)

#Erro padrão da média
(erro_pad_media <- sqrt(varmedia))

#Erro do inventário (m³/parcela)
(erro_inv <- qt(0.975, n - 1) * erro_pad_media) 

#Erro do inventário (m³/parcela)
(erro_invperc <- round(erro_inv/ ymreg * 100,2)) 

#m³/população
(ytreg <- ymreg * nt)

#m³/ha
(yhareg <- ytreg/area_frag)


# intervalo de confiança para a população
cat(paste( round(ytreg - erro_inv * nt, 0), "<= total <=",
           round(yteg + erro_inv * nt, 0), "m³\n"))

# intervalo de confiança da parcela, # precisa corrigir essa daqui:
cat(paste(round(ymreg - erro_inv, 2), "<= média populacional <=",
          round(ymreg - erro_inv, 2), "m³/parcela"), "\n")




