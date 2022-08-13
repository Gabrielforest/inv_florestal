#Foram utilizadas árvores cubadas em um povoamento de Eucalyptus sp. para fazer o ajuste do modelo volumétrico
#abaixo (vcomcc ∼ (dap, ht), onde: vcomcc = volume comercial com casca(m3), dap = diâmetro à altura do peito
#(cm) e ht = altura total(m)). Os resultados apresentados foram obtidos através do software R (R Core Team, 2021).
#OBS: Onde se lê “log”, leia-se “ln” (loge=ln) (15%).


# a) Quantas observações foram utilizadas?

# Df do modelo + Df dos resíduos + 1
# 1            +   36818         + 1

# b) Qual é o modelo e a equação ajustada?
"log(vcomcc) ~ log(dap) + log(ht)"
## modelo - log(vcomcc) = β0 + β1 × log(dap) + β2 × log(ht)
## equação ajustada - log(vcomcc)= -10,446 + 1,895 × log(dap) + 1,143×log(ht)

# c) O modelo é linear simples ou múltiplo? Justifique.

# multiplo, pois só tem 2 variável independente

# d) Para qual nível de significância em % o 
# ajuste foi significativo?

# Pr(> F) = 2.2e-16
# O ajuste é significativo,  95% de nível de sig.
# pois 2.2e-16 é menor do que 0.05

# e) para DAP = cm, qual altura estimada
exp(-10.446 + 1.895 * log(17.8) + 1.143 * log(25.5))

## na P2 a letra f) será:
# Quanto as variações da variável dependente
# são explicadas pelas variações da variável
# independente
# resposta é a mesma da de baixo, só um jeito novo de perguntar

# f) calcule o coef de determinação. O que significa ?
sum_sq <- 15.2998 + 0.1881
sum_sq_res <- 0.1154
sqtot <- sum_sq + sum_sq_res   
r2 <- sum_sq/sqtot
r2_perc <- round(r2 * 100, 2)

# 99,26% das variações da variável dependente
# são explicadas pelas variações da variável
# independente

## na P2 a letra g) será:
# Quanto em média os valores observados variam em relação aos estimados?
# resposta é a mesma da de baixo, só um jeito novo de perguntar

# g) Calcule o erro padrão residual na unidade da variável 
# dependente. O que este valor significa?
mean_sq_res <- 0.0035
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
