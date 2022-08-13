# modelo hipso ------------------------------------------------------------

# I(log(ht)) ~ I(log(dap))


# a) Quantas observações foram utilizadas?

# Df do modelo + Df dos resíduos + 1
# 1            +   36818         + 1

# b) Qual é o modelo e a equação ajustada?

## modelo - ht = b0 + b1(log(dap))
## equação ajustada - ht = 1.7012 + 0.5488 *(log(dap))

# c) O modelo é linear simples ou múltiplo? Justifique.

# simples, pois só tem 1 variável independente

# d) Para qual nível de significância em % o 
# ajuste foi significativo?

# Pr(> F) = 2.2e-16
# O ajuste é significativo,  95% de nível de sig.
# pois 2.2e-16 é menor do que 0.05

# e) para DAP = 18cm, qual altura estimada
ht <- 1.7012 + 0.5488 * (log(18))

## na P2 a letra f) será:
# Quanto as variações da variável dependente
# são explicadas pelas variações da variável
# independente
# resposta é a mesma da de baixo, só um jeito novo de perguntar

# f) calcule o coef de determinação. O que significa ?
sum_sq <- 512.78
sum_sq_res <- 183.03
sqtot <- sum_sq + sum_sq_res
r2 <- sum_sq/sqtot
r2_perc <- round(r2 * 100, 2)

# 73.70% das variações da variável dependente
# são explicadas pelas variações da variável
# independente

## na P2 a letra g) será:
# Quanto em média os valores observados variam em relação aos estimados?
# resposta é a mesma da de baixo, só um jeito novo de perguntar

# g) Calcule o erro padrão residual na unidade da variável 
# dependente. O que este valor significa?
mean_sq_res <- 0
e_res <- exp(sqrt(mean_sq_res))

# quanto em média os valores observados variam em relação
# aos estimados

  
# h) Sabendo-se que a média é de __________ ,
# qual é o erro padrão residual em porcentagem?
x <- 0
e_res_porc <- 100 * (e_res/x) 

# i) Porque é importante fazer a análise dos resíduos? 
# Qual é o comportamento esperado dos resíduos? 
# (Se você achar necessário, faça um esboço do gráfico
# para facilitar sua explicação.)

# Para analisar se os resíduos são tendenciosos ou não.
# O comportamento esperado é de que os resíduos sigam a 
# distribuição normal, ou seja, estejam 
# distribuídos em torno do 0 em faixa. 
# Essa análise é importante para 
# avaliarmos se existem dados estranhos e decidirmos
# qual ação será tomada em relação a isso.

# j) Caso os resíduos não possuam distribuição normal 
# qual(is) procedimento(s) você adotaria?

# Transformar variável dependente e ou testar outros 
# modelos.




# tii Zé ------------------------------------------------------------------

# O preguiçoso do Totonho não veio buscar 
# os 8 eucalipto de CAP igual a 50cm e altura 
# total igual a 29m.
# Quanto de CO2 as plantas comeram?

# considere 1 tonelada retira do meio ambiente
# 1.83 toneladas de CO2

cap <- 50
h <- 29
ff <- 0.45
eucalipto <- 8
ton_co2 <- 1.83

dap <- cap/pi

v <- pi * dap^2/ 40000 * h * ff

ton_pop <- v * 0.5 * eucalipto

ton_co2_consumido <- ton_pop * ton_co2





# Questão de intervalo de confiança ---------------------------------------

# Foi efetuado um inventário de Pinus em uma área de 115ha
# Foram mensuradas 20 parcelas de 20x30m distribuídas
# aleatoriamente. Após o processamento, foi estimado o 
# seguinte IC 14.11 <= m <= 20.33 m³/ parcela
# (α = 5%). Responda:

areafaz <- 115
n <- 20
areaparc <- 20 * 30
li <- 14.11
ls <- 20.33

# a) erro do inventário %:

# número de parcelas cabíveis:
N <- round(areafaz * 10000/ areaparc)
ymed <- (li + ls)/ 2
erro <- ymed - li
# erro do inventário em porcentagem
erro_porc <- (erro / ymed) * 100

# b) Volume estimado para população
vol_estimado <- N * ymed

# c) IC para população, interprete o IC:
erro_populacao <- (erro_porc/100) * vol_estimado 

paste0(round(vol_estimado - erro_populacao, 0), 
       " <= ", round(vol_estimado, 2), " <= ",
       round(vol_estimado + erro_populacao, 0))

# Eu tenho 95% de confiança que o total populacional 
# encontra-se entre os limites inferior e superior 
# do intervalo de confiança, com 5% de chance de ser 
# iludido e o total populacional estar fora desse IC


# d) Considerando que o erro foi superior ao admissível
# o que deve ser feito para diminuí-lo?

# o mais barato seria avaliar se é possível estratificar a população em estratos
# mais homogêneos ou aumentar a intensidade amostral, pois quando eu aumento 
# a quantidade de amostras a tendência é reduzir o erro padrão da média.




# Qual a intensidade amostral AS ------------------------------------------

# Em um inventário florestal utilizando AS, o intervalo
# K foi igual a 200m. Qual a intensidade amostral?

k <- 177
intensidade_amostral <- (k * k)/ 10000



# Questão de intervalo de confiança com raio ------------------------------

# Foi efetuado um inventário para conhecer o volume de Pinus
# sp. de uma área de 107 hectares. O técnico mensurou 30
# parcelas circulares com raio de 13,87m distribuídas 
# aleatoriamente na área. Após o processamento do inventário,
# o técnico estimou o seguinte intervalo de confiança:(20%)
# Intervalo de confiança: 16,82 ≤ μ ≤ 20, 35m3/parcela 
# (α = 5%). Responda:

# a) o ERRO DO INVENTÁRIO EM PORCENTAGEM 
#Área da parcela em m²
# pi * r²
areaparc <- pi * 11.2838^2

#Área da fazenda 
areafaz <- 502


n <- 40
areaparc <- pi*11.2838^2
N <- areafaz*10000/areaparc
yvar <- 1.71
ymed <- 6.97
(yvarmed<-(yvar/n)*(1-n/N));
(ydesvmed<-sqrt(yvarmed))
(erroinv<-qt(0.965,n-1)*ydesvmed)
(erroinvperc<-erroinv/ymed * 100)


# b) volume estimado para a população 
# se para 20 parcelas de 600 m² tivemos 18.34 de média para 103 0000 m² teremos:
# numero de parcelas cabíveis * 18.34 m³ = volume estimado
#vol_estimado <- N * ymed
yha <- ymed*10000/areaparc

erroinvha <- erroinv*10000/areaparc
li <- yha-erroinvha
ls <- yha+erroinvha
# c) intervalo de confiança para a população. Interprete 
# se o IC da parcela é o ymed +/- erro 
# o IC da população é o vol_estimado_pop * erro em porcentagem/100
erro_populacao <- (erroinvperc/100) * vol_estimado

paste0(round(vol_estimado - erro_populacao, 0), 
       " <= ", round(vol_estimado, 2), " <= ",
       round(vol_estimado + erro_populacao, 0))

# d) calcule a intensidade amostral:
# alterar valores:
# Em um povoamento florestal de 107 ha
# foram lançadas 30 parcelas, considerando que a amostragem foi sistemática
# qual foi a intensidade amostral (01 parcela/ha) e a distância entre 
# parcelas?

n <- areafaz/30
# 1 parcela a cada 3.5 ha

# e) agora calculamos o valor do espaçamento entre as parcelas em m:
k <- sqrt(n * 10000)
# a cada 188.85 m 

# f)
# o mais barato seria avaliar se é possível estratificar a população em estratos
# mais homogêneos ou aumentar a intensidade amostral, pois quando eu aumento 
# a quantidade de amostras a tendência é reduzir o erro padrão da média.




# Toras  ------------------------------------------------------------------

#Voce precisa estimar o volume de madeira estocado no patio de uma fabrica de celulose. As toras tem 3,6 metros
#de comprimento e as pilhas de madeira tem comprimento total de 4797 metros, mas, as alturas das pilhas
#sao visiveis.Para resolver este problema voce amostrou aleatoriamente algumas alturas das pilhas e os 
#resultados das mediçoes fora: 3,95; 3,77; 4,2; 4,49; 4,31; 3,71; 4,04; 4,27; 3,92; 4,16.

#A paritr da amostra, estime:

#a) o Intervalo de Confiança para o Volume Total da madeira no patio (a = 5%)

alt <- c(3.95, 3.77, 4.2, 4.49, 4.31, 3.71, 4.04, 4.27, 3.92, 4.16);
comp <- 3.6
pilha <- 4797

(volume <- mean(alt) * comp * pilha)

#Variavel de interesse

y <- c(alt * comp * pilha)

#Media (m³/parcela)
(ym <- mean(y))

#Variancia (m^6/parcela)
(yvar <- var(y))

#Desvio Padrao (m³/parcela)
(ydesv <- sd(y))

#Tamanho da amostra
(n <- length(y))


#Erro padrao
(erropad <- sqrt(yvar/n))

#Erro do invent?rio (m³/parcela)
(erroinv <- qt(0.975, n - 1) * erropad)

#Intervalo de confiança
cat(paste(ym - erroinv), "<= total <=", (ym + erroinv));

#b) O tamanho da amostra para erro amostral igual a 10%.

N <- Inf
E <- 10
cv <- ydesv/ym * 100

#Sem recalculo
(n1 <- round((qt(0.975, n - 1)^2) * ((cv)^2)/((E)^2) + ((qt(0.975, n - 1)^2) * ((cv)^2)/N)))

#Utilizando grandes numeros e a media amostral, estime:

#a) Sabendo-se que o estoque de madeira do patio é suficiente para 01 semana, 
#qual é o consumo de madeira em toneladas/ano.
#0,5ton/m³ e 48 semanas
#(tonmadeira=((volume/1.6)*0.5)*48) #tonelada/ano (depende de quantas semanas considera que tenha 1 ano)

# volume mst ---- 7   dias
# x      mst ---- 365 dias
(x <- (volume*365)/7) #mst/ano

(z <- x/1.6) #m³

# 1 m³ ---- 0,5 ton
# z m³ ---- w
(w = z*0.5) #toneladas

#b) Qual area minima de efetivo plantio que a empresa deve possuir para abastecer 
# ininterruptamente a fabrica.
#(areaefetivoplantio=(((volume/1.6)*48)/40));

# z    m³ ---- j arv
# 0,25 m³ ---- 1 arv sem casca
(j = z/0.25)

j/1667 #ha




# CO2 ---------------------------------------------------------------------

# Em média 1 tonelada de madeira seca retira do do meio ambiente
# 1.83 toneladas de CO2 e a emissão média de CO2 de uma pessoa, 
# é de 4 toneladas anuais. Continuando com grandes números, qual 
# é o número mínimo de árvores da espécie Eucalyptus sp que você 
# deverá plantar para ser uma pessoa "carbono neutra" em um período
# de 6 anos. COnsidere apenas o estoque do fuste comercial e, 
# caso queira, pode ser outra espécie e, ou, um período maior.


# dados:
uma_tonelada_co2_madeira_seca_retira <- 1.83
uma_pessoa_emite_ton_anuais <- 4
anos <- 6
# mágicos:
densidade_ton <- 0.5
uma_arvore_tem_madeira_seca <- 0.2

toneladas_uma_pessoa_emitiu <- uma_pessoa_emite_ton_anuais * anos

toneladas_madeira_seca <- toneladas_uma_pessoa_emitiu / uma_tonelada_co2_madeira_seca_retira

volume <- toneladas_madeira_seca / densidade_ton

n_arvores <- round( volume / uma_arvore_tem_madeira_seca )





















