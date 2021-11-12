############# PROVA 1 - INVENTÁRIO FLORESTAL


#Foi efetuado um inventário para conhecer o volume de Pinus sp. de uma área de 107 hectares. O técnico mensurou 30
#parcelas circulares com raio de 13,87m distribuídas aleatoriamente na área. Após o processamento do inventário, o
#técnico estimou o seguinte intervalo de confiança:(20%)
#Intervalo de confiança: 16,82 ≤ μ ≤ 20, 35m3/parcela (α = 5%).
#Responda:

#(a) Erro do inventário em porcentagem. (15%)
#(b) Volume estimado para a população. (5%)

#(c) Intervalo de confiança para a população (Interprete o intervalo de confiança). (30%)

#(d) Qual foi a intensidade amostral (01 parcela / n hectares)? (5%)

#(e) Caso as parcelas fossem distribuídas de forma sistemática, qual seria a distância entre as amostras? (5%)

#(f) Considerando que o erro foi superior ao admissível, para o mesmo nível de significância, o que deve ser feito
#para diminuí-lo? Justifique. (10%)

#(g) A relação entre o nível de significância e o risco de ser "iludido"pela amostra é direta ou inversa? Justifique.
#(10%)

#(h) Qual é o princípio básico da amostragem adotada? (10%)

#(i) Durante o planejamento do inventário florestal qual é a condição determinante que faria você optar pela utilização
#da ACS com estimador de razão ou com estimador de regressão em detrimento da utilização da ACS tradicional?
#   (10%)


# a) o ERRO DO INVENTÁRIO EM PORCENTAGEM ----------------------------------
#Área da parcela em m²
# pi * r²
areaparc <- pi * 13.87^2

#Área da fazenda em m²
areafaz <- 107

#Número de parcelas cabíveis
N <- areafaz * 10000/areaparc
round(N)
# o intervalo de confiança é encontrado através de:
# IC = t(valor da tabela) * erro padrao da média + media de y
# IC = t * sdy + ymed

# o ymed é o valor médio entre os limites do intervalo de confiança, logo:
ymed <- (16.82 + 20)/2

# o erro é a diferença entre o valor médio e o li e ls:
erro <- ymed - 16.82

# o erro em porcentagem é o (erro / media de y) * 100:
erro_porcentagem <- (erro / ymed) * 100

# b) volume estimado para a população -------------------------------------
# se para 20 parcelas de 600 m² tivemos 18.34 de média para 103 0000 m² teremos:
# numero de parcelas cabíveis * 18.34 m³ = volume estimado
vol_estimado <- N * ymed

# c) intervalo de confiança para a população. Interprete ------------------
# se o IC da parcela é o ymed +/- erro 
# o IC da população é o vol_estimado_pop * erro em porcentagem/100
erro_populacao <- (erro_porcentagem/100) * vol_estimado 

paste0(round(vol_estimado - erro_populacao, 0), 
       " <= ", round(vol_estimado, 2), " <= ",
       round(vol_estimado + erro_populacao, 0))

# d) calcule a intensidade amostral:
# alterar valores:
# Em um povoamento florestal de 107 ha ---------------------------------
# foram lançadas 30 parcelas, considerando que a amostragem foi sistemática
# qual foi a intensidade amostral (01 parcela/ha) e a distância entre 
# parcelas?

n <- areafaz/30
# 1 parcela a cada 3.5 ha

# e) agora calculamos o valor do espaçamento entre as parcelas em m:
k <- sqrt(n * 10000)
# a cada 188.85 m 

# f)
#Aumentar a intensidade amostral, quanto maior a amostra menor o erro, no 
#entanto, existe um limite no qual o crescimento da amostra não será mais 
#impactante no erro.

# g)
# Quanto maior o nível de significância maior será a chance de ser iludido
# então a relação é direta.

# h)
# O princípio da ACS é de que todas as amostragens têm chances iguais de sorteio, ou seja,
# não há restrição à casualização

# i)
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



################################
# 3)

#  (a) Como foi obtida a base de dados e como a mesma foi organizada para o ajuste no software R?
#  (b) Quantas observações foram utilizadas?
#  (c) Qual é o modelo e a equação ajustada?
#  (d) O modelo é linear simples ou múltiplo? Justifique.
#  (e) Para qual nível de significância em % o ajuste foi significativo?
#  (f) Para um diâmetro de 16, 1cm e altura total de 24m qual é o volume estimado através da função de regressão
#ajustada?
#  (g) Calcule o coeficiente de determinação em porcentagem. O que esse valor significa?
#  (h) Calcule o erro padrão residual na unidade da variável dependente. O que este valor significa?
#  (i) Sabendo-se que a média é de 0, 22964m3 , qual é o erro padrão residual em porcentagem?
#   
# (j) Porque é importante fazer a análise dos resíduos? Qual é o comportamento esperado dos resíduos? (Se você
#                                                                                                        achar necessário, faça um esboço do gráfico para facilitar sua explicação.)
# (k) Caso os resíduos não possuam distribuição normal qual(is) procedimento(s) você adotaria?



# b) 37

# c)
 modelo - vcomcc = b0 + b1(dap^2 * ht)
 equação ajustada - vcomcc = 1.186e-02 + 3.196e-05 * (dap^2*ht)

# d) simples

# f) 
# f) Para dap = 16,1cm e ht = 24 m, vtcc? -----------------------------------
vcomcc = 1.186e-02 + 3.196e-05 * (16.1^2*24)

# g)
sqres <- 0.0093
sqtot <- 0.0093 + 0.4424
r2 <- 1 - (sqres/sqtot)
r2_perc <- r2 * 100

# h) 
sqres

# i) 
(sqres/0.22964)*100

#  
sqres/

