#ACS - ESTIMADOR DE RAZÃO PARA O TOTAL POPULACIONAL

#Estimador de razão para o total da população
parc <- read.csv2('dados_raz_reg.csv')
View(parc)
comp_frag <- parc$compfrag[1]
larg_parc <- parc$larg_parc[1]
area_frag <- parc$areafrag[1]

#Número de parcelas cabíveis
(nt <- comp_frag/larg_parc) 

# pra ter uma realção metro cúbico por ha, área em ha:
x <- parc$areaparc/10000
y <- parc$vtcc

plot(x, y,
     xlab = "área da parcela(ha)",
     ylab = "vtcc",
     pch = "*",
     col = "red")

aj <- lm(y ~ x)
lines(x, predict(aj))

#média estimada de x
xme <- mean(x)

#Média populacional da variável auxiliar
xm <- area_frag/nt

#média estimada de y
yme <- mean(y) 

#Estimador de razão
R <- yme/xme 
R 

#Número de amostras
n <- length(y)
n 

#varr=Variância entre valores observados e os estimados
#     pelo estimador de razão
varr <- (sum(y^2) - 2 * R * sum(y * x) + R^2 * sum(x^2))/ (n-1)
varr

#Total(m³) 
(ytraz <- R*area_frag)

#Variância do total
varrtotal <- (1 - (n/nt)) * (1/n) * (area_frag^2) * (1/xm^2) * varr

#Erro padrão do total
(erro_pad_total <- sqrt(varrtotal))

#Erro do inventário (m³)
(erro_inv <- qt(0.975,n-1) * erro_pad_total)

#Erro do inventário(%)
(erro_inv_perc <- erro_inv/ytraz * 100)
