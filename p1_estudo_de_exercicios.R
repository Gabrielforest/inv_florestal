# Estudos exercícios P1


# Questão 1 - NOTAS--------------------------------------------------------

# Preocupado com seu rendimento em dendrometria vc perguntou aleatoriamente
# o rendimento dos colegas. 
# Sabendo que  a turma possui 198 alunos, considerando alpha 5%, responda:

notas <- c(8.62, 6.07, 6.71, 2.9, 6.3, 6.6, 9.19, 8.56,
           7.68, 9.17, 4.72, 6, 8.27, 6.39, 5.78, 6.28)



# a) Média Estimada -------------------------------------------------------

round(mean(notas),2)

# b) Erro Percentual de Amostragem ----------------------------------------

#Tamanho da população:
N <- 198

estats_acs <- as.data.frame(cmrinvflor::estats_acs(vy = notas, nt = N, sig = 5))
estats_acs$eperc

# OU

#Tamanho da amostra:
n <- length(notas)

#Variância da media:
varmed <- (var(notas) / n) * (1 - n / N)

#Erro padrão da média                -- sdymed:
desvmed <- sqrt(varmed)

#Erro                                -- eunid:
erro <- qt(0.975, n - 1) * desvmed

#Erro em porcentagem                 --  eperc:
erro_perc <- erro/mean(notas) *100


# c) Sabendo que a média pop. foi de 6,46, acurácia em % é: ---------------
## se der negativo, fica negativo (superestimei a nota):

((6.46 - mean(notas)) / 6.46) * 100


# d) Intervalo de Confiança -----------------------------------------------

estats_acs <- as.data.frame(cmrinvflor::estats_acs(vy = notas, nt = 198, sig = 5))
estats_acs$li
estats_acs$ls


# e) Você foi iludido pela sua amostra? -----------------------------------

# A média populacional se encontra dentro do intervalo de confiança
# com 95% de confiança e 5% de chance de eu ter sido iludido pela minha amostra.


# f) Considerando que as notas possuem distribuição normal truncada [0;10] -----
# e que sua nota (6.02) possui uma prob acumulada de 0.4103, vc foi um outlier?
# Justifique com alpha = 5%:
-2 * sdy + ymed_pop <= ymed_pop <= 2 * sdy + ymed_pop

paste0(-2 * sd(notas) + 6.46, 
       " <= ", 6.46, " <= ", 
       2 * sd(notas) + 6.46)
# Ou
# se a prob. acum. vai até 0.4103, numa distribuição normal
# de 0 até 1 a faixa de valores menores do que 0.025 tanto 
# negativos quanto positivos seriam outliers, logo
# nesse caso não sou um outlier por estar quase no centro
# da distribuição


# g) Qual a prob. de um aluno ter rendimento melhor que o meu? ------------

1 - 0.4103



# ------------------------ Prova teórica ----------------------------------

# Questão 1 - DADOS A PARTIR IC--------------------------------------------

# Foi executado um inventário para conhecer o volume
# de pinus de uma área de 103 ha. O técnico mensurou
# 20 parcelas circulares com raio de 13.82m distribuídas
# aleaoriamente na área. Após o processamento do inventário
# o técnico estimou o seguinte intervalo de confiança
# 16.55 <= u <= 20.13 m³/parcela (alpha = 5%). Responda:


# a) o ERRO DO INVENTÁRIO EM PORCENTAGEM ----------------------------------
#Área da parcela em m²
# pi * r²
areaparc <- pi * 13.82^2

#Área da fazenda em m²
areafaz <- 103

#Número de parcelas cabíveis
N <- areafaz * 10000/areaparc

# o intervalo de confiança é encontrado através de:
# IC = t(valor da tabela) * erro padrao da média + media de y
# IC = t * sdy + ymed

# o ymed é o valor médio entre os limites do intervalo de confiança, logo:
ymed <- (20.13 + 16.55)/2

# o erro é a diferença entre o valor médio e o li e ls:
erro <- ymed - 16.55

# o erro em porcentagem é o (erro / media de y) * 100:
erro_porcentagem <- (erro / ymed) * 100

# b) volume estimado para a população -------------------------------------
# se para 20 parcelas de 600 m² tivemos 18.34 de média para 103 0000 m² teremos:
# numero de parcelas cabíveis * 18.34 m³ = volume estimado
vol_estimado <- N * 18.34

# c) intervalo de confiança para a população. Interprete ------------------
# se o IC da parcela é o ymed +/- erro 
# o IC da população é o vol_estimado_pop * erro em porcentagem/100
erro_populacao <- (erro_porcentagem/100) * vol_estimado 

paste0(round(vol_estimado - erro_populacao, 2), 
       " <= ", round(vol_estimado, 2), " <= ",
       round(vol_estimado + erro_populacao, 2))


# Com um nível de confiança de 95% o total populacional se encontra
# entre o limite inferior de 28409.87 e o limite superior de 34555.3
# com 5% de chance de eu ter sido iludido pela minha amostra.

# d) considerando que o erro foi superior ao admissível, ------------------
# para o mesmo nível de sig. o que deve ser feito para 
# diminuí-lo? Justifique.

#Aumentar a intensidade amostral, quanto maior a amostra menor o erro, no 
#entanto, existe um limite no qual o crescimento da amostra não será mais 
#impactante no erro.

# e) qual o princípio básico da amostragem adotada? -----------------------

# Todas as amostragens têm chances iguais de sorteio, ou seja,
# não há restrição à casualização



# Questão 2 - PRECISÃO E ACURÁCIA------------------------------------------

# Em um povoamento de teca foram realizados 4 inventários florestais por                
# empresas distintas, as quais utilizavam a mesma intensidade amostral. Na 
# figura 1 é possível observar a distribuição dos resultados por parcela 
# (m³/ha) de cada inventário, onde m é a média amostral e S é o desvio padrão
# amostral


# a) QUAL INVENTÁRIO VOCÊ ESCOLHERIA? -------------------------------------

# Eu escolheria o inventário que apresenta a maior precisão (menor desvio
# padrão), consequentemente menor erro, e a maior média amostral em m³.

# b) sabendo que após a entrega da madeira na fábrica a média -------------
# observada foi de 125m³/ha você continuaria com o mesmo invenrário do item 
# anterior? Justifique.

# não, pois o volume foi abaixo do intervalo de confiança, logo, 
# seria melhor utilizar o enventário I.

# c) Construa o gráfico precisão ------------------------------------------


# Questão 3 - ACS E AS-----------------------------------------------------

# a) Faça um COMPARATIVO entre amostragem simples e amostragem sistemática ----

# Na ACS todas as unidades amostrais têm as mesmas chances de serem sorteadas.
# É aplicada quando não há qualquer restrição de serem sorteadas. É aplicada 
# quando não há retrições à casualização. As desvantagens é a dificuldade em
# acessar as unidades amostrais em campo.

# Já a Amostragem Sistemática consiste na casualização da primeira unidade 
# amostral e na alocação das outras dependentemente da posição da primeira 
# conforme um intervalo espacial K. É válido para áreas pouco conhecidas, 
# pois percorre-se a propriedade toda. As desvantagens são relacionadas as 
# probabilidades distintas de srteio das amostras e aos efeitos periódicos 
# aos quais as populações biológicas estão submetidas.    


# b) Em um povoamento florestal de 112 ha ---------------------------------
# foram lançadas 28 parcelas, considerando que a amostragem foi sistemática
# qual foi a intensidade amostral (01 parcela/ha) e a distância entre 
# parcelas?

# Para descobrirmos o valor de K, precisamos
# do quanto cada parcela ocupa em ha:
intervalo_ha_parcelas <- 112/28 

# agora calculamos o valor do espaçamento entre as parcelas em m:
k <- sqrt(intervalo_ha_parcelas * 10000)



# c) Quando usar ACS estimador de razão ou regressão ou tradicional -------

# Para usarmos ACS não tradicional é necessário que haja uma forte relação
# entre a variável de interesse e a variável auxiliar, sendo conhecido o 
# total populacional da variável auxiliar. 

# Para utilizar estimadores de razão é necessário que a relação linear entre 
# a variável de interesse e a variável auxiliar seja positiva e crescente e 
# a reta precisa passar pela origem, e além disso é necessário ser observado
# heterogeneidade da variância. 

# Já o estimador de regressão deve ser utilizado quando não é 
# observado heterogeneidade da variância, respeitando os outros 
# aspectos citados inicialmente.



# Questão 4 - PRINT -------------------------------------------------------


# como foi obtida a base e organizada  ------------------------------------

# os dados foram obtidos em campo através das medições
# e organizados em "arvores"    "dap"    "altura total" e as colunas
# de volumes que foram calculadas provavelmente através de smalian:
# "volume comercial com casca" "volume comercial sem casca"

# a) modelo vtcc ~ bo + I(dap^2 * ht) -------------------------------------

## modelo - vtcc = b0 + b1(dap^2 * ht)
## equação ajustada - vtcc = 8.237*10^-4 + 3.528*10^-5 * (dap^2*ht)

# b) o ajuste foi significativo? ------------------------------------------

# sim pois o valor de p foi menor do que 0.05.

# c) Para dap = 16cm e ht = 30 m, vtcc? -----------------------------------
#vtcc = 8.237*10^-4 + 3.528*10^-5 * (dap^2*ht)
vtcc <- 8.237*10^-4 + 3.528*(10^-5) * (16^2*30)

# d) calcule o coef. de det. ----------------------------------------------
# soma a primeira coluna e divide o primeiro valor da primeira coluna
sqres <- 0.200176
sqtot <- 0.200176 + 0.006276
r2 <- 1 - (sqres/sqtot)
r2_perc <- r2 * 100


# e) Quanto em média é o erro que se comete ao utilizar a reta de  --------
# regressão ajustada?
QMr <- 0.000086
sqrt(QMr)


# f) xmed = 0.24935, qual o erro padrão residual em %? --------------------
(sqres/0.24935)*100
