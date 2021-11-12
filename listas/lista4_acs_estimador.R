#   Para realizar o inventário em um povoamento de 1a rotação de 201ha de Eucalyptus urophylla foram lançadas 
# aleatoriamente “n” parcelas de 400m2

#. Toda a madeira foi entregue para uma unidade de produção de carvão, totalizando
#35463m3 de madeira. Concluída a colheita o povoamento foi conduzido e, após 6 anos, as mesmas “n” parcelas foram
#remedidas. Considerando α = 1% e utilizando a base de dados “parcelas.csv”, a qual possui os resultados por parcela
#do volume comercial com casca das medições realizadas nas duas rotações, responda os itens abaixo: (100%)

# (a) O volume comercial com casca estimado para a fazenda na primeira rotação.
# (b) O erro percentual do inventário na primeira rotação (precisão).
# (c) O intervalo de confiança do volume comercial com casca estimado para a fazenda na primeira rotação.
# (d) Diferença percentual entre volume entregue e volume comercial com casca estimado pelo inventário na primeira
# rotação (acurácia).
# (e) O volume comercial com casca estimado para a fazenda na segunda rotação.
# (f) O erro percentual do inventário na segunda rotação.
# (g) O intervalo de confiança do volume comercial com casca estimado para a fazenda na segunda rotação.



# dados -------------------------------------------------------------------

parc <- read.csv2("listas/lista4_parcelas.csv")


# adicionanado colunas e variáveis ----------------------------------------

# área da parcela em m^2
parc$area_parc <- 400

area_parc <- 400


# área do povoamento primeira rotação em ha
parc$area_faz <- 201

area_faz <- 201

# volume total
vtcc <- 35463


# estatísticas a 1% de sign. primeira rotação -----------------------------


#Variável de interesse
y <- parc$vcomcc1

#Número de parcelas cabiveis na população
N <- (area_faz * 10000) / area_parc

# tabela de estatísticas 
estatisticas_acs <- as.data.frame(cmrinvflor::estats_acs(vy = y, sig = 1, nt = N))

#Tamanho da amostra (quantas parcelas) 
n <- length(y)

# var y
yvar <- var(y)

#Variancia da média 
yvarmed <- (yvar/n) * (1 - n/N)

# média por hectare (m3/ha) 
yha <- estatisticas_acs$ymed * 10000 / area_parc

# Erro do inventário por hectare (m3/ha)
erroinvha <- estatisticas_acs$eunid * 10000 / area_parc 

# total populacional (m3/fazenda: total) #volume comercial
ytot <- yha * area_faz;


#Erro do inventário para a populaçao (m3/fazenda) 
erroinvpop <- erroinvha * area_faz


#diferença percentual entre volume entregue e volume estimado pelo inventário 
acuracia <- ((vtcc-ytot)/vtcc) * 100

#Intervalo de confiança para a população (m3)
cat(paste(round(ytot-erroinvpop,0), " m3", "<= total populacional <=", 
          round(ytot+erroinvpop,0), " m3", sep=''))





# Segunda rotação ---------------------------------------------------------



x <- parc$vcomcc1
y <- parc$vcomcc2


# gráfico:
plot(x, y,
     xlim = c(0,max(x)), 
     ylim = c(0,max(y)),
     xlab = "área da parcela (ha)",
     ylab = "volume (m3)",
     pch = "*",
     col = "red")

ajlin <- lm(y ~ x)
lines(x, predict(ajlin))
summary(ajlin)

# b0 não foi significativo, e apesar de crescente, a reta passa pela origem,
# portanto utilizarei a acs estimador de razão.


#Média estimada da variável auxiliar (x)
xme <- mean(x)

#Média populacional da variável auxiliar
xm <- vtcc/N

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

#Erro do inventário (m3/parc)
erro_inv <- qt(0.995, n - 1) * erro_pad_media

#Erro do inventário (%)
erro_inv_perc <- erro_inv/ ymraz * 100

#Total populacional (m3)
ytraz <- ymraz * N 

ext_ha <- 10000/parc$area_parc[1]

erroinvtot <- erro_inv * ext_ha * parc$area_faz[1] 

#Intervalo de confiança para a população
cat(paste(
        round(ytraz - erroinvtot, 0),'<= total <=', 
        round(ytraz + erroinvtot, 0),'m³\n')
    )

