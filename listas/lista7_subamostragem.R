# Você foi contratado(a) para estimar o volume total de madeira com casca dos fragmentos de cerrado numa determinada
# bacia hidrográfica. Para reduzir os deslocamentos você optou por utilizar a amostragem em conglomerados em dois
# estágios (subamostragem). Processe o inventário florestal (α = 5%) utilizando os resultados das medições por parcela
# apresentados na planilha de dados em anexo (“subamostragem.csv”). (100%)
# Campos da planilha de dados:
# 
# • frag : Número do fragmento amostrado;
# • nfrag : Número de fragmentos existentes na bacia hidrográfica;
# • areafrag: Área do fragmento amostrado [ha]
# • parc : Número da parcela;
# • areaparc: Área da parcela [m2]
# • vtcc : Volume total com casca [m3/parcela]


# Lista 7 -----------------------------------------------------------------
###LISTA SUBAMOSTRAGEM 

dados <- read.csv2("./listas/subamostragem.csv")

dados$cluster <- dados$frag
dados$yi <- dados$vtcc

# Definicoes:
# N: numero de unidades do primeiro estagio cabiveis na populacao = 160
# n: numero de unidades do primeiro estagio da amostra = 22
# Mi: numero de unidades do segundo estagio cabiveis na iésima unidade do primeiro estagio
# mi: numero de unidades do segundo estagio selecionados na iésima unidade do primeiro estagio

#110
(N <- dados$nfrag[1])   
#21
(n <- length(unique(dados$cluster)))

# Area média das parcelas por cluster selecionado: 1000m²
selclusters <- aggregate(list (areaparc = dados$areaparc), list(cluster = dados$cluster), mean)

# Area do cluster:
calc <- aggregate(list (areafrag=dados$areafrag), list(cluster=dados$cluster),mean)

selclusters <- merge(selclusters, calc)

# Número de unidade do segundo estagio cabiveis na iesima unidade do primeiro estagio:
(selclusters$Mi <- with(selclusters, areafrag*10000/areaparc))
# Como eles tem tamanhos diferentes, cabem diferentes números de parcelas em cada um deles.

# Numero de unidades do segundo estagio selecionadas na iessima unidade do primeiro estagio:
calc <- aggregate(list (mi = dados$parc), list(cluster = dados$cluster), length)
# Agora quantas tabelas foram selecionadas!
selclusters <- merge(selclusters, calc) 


# Igual estratificada, as medias s?o tiradas de CADA cluster
# Media das parcelas na iesima unidade do primeiro estagio:
(calc <- aggregate(list(ym = dados$yi), list (cluster = dados$cluster), mean))
(selclusters <- merge(selclusters, calc))

#Variancia das parcelas na iesima unidade do primeiro estagio:
(calc <- aggregate(list(s2w = dados$yi), list (cluster = dados$cluster), var))
(selclusters <- merge(selclusters, calc))
View(selclusters)

# Numero medio de parcelas do segundo estagio cabiveis por unidade do primeiro estagio
# Saber em média qual a quantidade de parcelas que existem nos clusters de segundo est?gio
# Se todos os conglomerados tiverem o mesmo tamanho, e o mesmo numero de parcelas, ser? mais f?cil. Mas aqui n?o acontece isso, ent?o precisamos saber a media!
# Então em média, caberiam 2759.5 por conglomerado.
(Mn <- sum(selclusters$Mi)/n)           


# Agora vamos fazer o primeiro calculo das estatísticas
# Media por parcela (media aritmetica simples): 8.453054
(ym <- mean(dados$yi))


# Nao funciona EXTRAPOLAR para conglomerado ou subamostragem, então vamos direto para o estimador da população
# Estimador da população total com o valor de Mi de todos os clusters desconhecido.

#Total populacional:
# Esse é o total populacional em  2567367m³
(Y <- (N/n) * sum(selclusters$Mi * selclusters$ym))

# Qual seria a formulação se eu conhecesse a pop inteiro: ( No caso, se o Mi de toda a pop fosse conhecido)
# clusters$Mi<-clusters$area*10000/1000);
# Y<- sum(clusters$Mi)*ym;

#Variancia da populacao (primeiro calcular a variancia entre compartimentos, porque dentro nós já calculamos acima):
#Variancia entre compartimentos/clusters: 2.716852
(s2b <- (1/(n - 1)) * sum(((selclusters$Mi/Mn) * (selclusters$ym-ym))^2))


#Variancia da media da populacao: 9748647114
(s2ym <- (N/n) * ((Mn^2) * (N - n) * s2b + with(selclusters, sum(Mi * (Mi - mi) * (s2w/mi)))))

# Erro padrao da media da populacao:98735.24
(sym <- sqrt(s2ym))


# Erro do inventário:291818.7
# Erro absoluto:291818.7 
#COLOCAR O ALFA!!!!!
(erro <- qt(0.975, sum(selclusters$mi) - 1) * sym)

# Erro percentual:7.553082 %
(erroperc <- erro/Y * 100)

# Intervalo de confianca:
(li <- Y - erro)
(ls <- Y + erro)
(Y) 
#2567367

#2373451 <= 2567367 <= 2761282, considerando 95% de confiança



