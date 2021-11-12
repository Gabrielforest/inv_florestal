#AMOSTRAGEM EM SEGUNDO ESTÁGIO (SUBAMOSTRAGEM)

clusters <- read.csv2('subamostragem_fragmentos.csv')
View(clusters)

parcelas <- read.csv2('subamostragem_parcelas.csv')
View(parcelas)

library(cmrinvflor)
names(clusters) <- v_from_to(names(clusters), 'frag', 'cluster')

names(parcelas) <- v_from_to(names(parcelas),
                             c('frag', 'peso'),
                             c('cluster', 'yi'))

#Definições
#N: Número de unidades do 1º estágio (cluster) da população
#n: Número de unidades do 1º estágio da amostra
#Mi: Número de unidades do 2º estágio cabíveis na i-ésima 
#    unidade do 1º estágio
#mi:Número de unidades do 2º estágio selecionadas na i-ésima 
#    unidade do 1º estágio

N <- length(clusters$cluster)
n <- length(unique(parcelas$cluster))

#Área média das parcelas
sba <- with(parcelas,
            aggregate(list(area = area,
                           areaparc = areaparc),
                      list(cluster = cluster),
                      mean)
)

#Mi: Número de unidades do 2º estágio cabíveis na i-ésima 
#    unidade do 1º estágio
sba$Mi <- sba$area*10000/sba$areaparc

#mi:Número de unidades do 2º estágio selecionadas na i-ésima 
#    unidade do 1º estágio
calc <- with(parcelas,
             aggregate(list(mi = parc),
                       list(cluster = cluster),
                       length)
)

sba <- merge(sba, calc)

#Média das parcelas na i-ésima unidade do primeiro estágio
calc <- with(parcelas,
           aggregate(list(ym = yi),
                     list(cluster = cluster),
                     mean)
)

sba <- merge(sba, calc)

#Variância das parcelas na i-ésima unidade do primeiro estágio
calc <- with(parcelas,
             aggregate(list(s2w = yi),
                       list(cluster = cluster),
                       var)
)
sba <- merge(sba, calc)

#Número médio de parcelas do 2º estágio cabíveis por unidade do
#1º estágio

Mm <- sum(sba$Mi)/n

#Média por parcela
ym <- mean(parcelas$yi)

#Estimador da população total com o valor de Mi de todos os 
#clusters NÃO conhecido

#Total populacional

Y <- N * (sum(sba$Mi*sba$ym)/n)

#Estimador da população total com o valor de Mi de todos os 
#clusters conhecido

#Total populacional
#M<-sum(clusters$area)*10000/mean(sba$areaparc);
#Y<-M*ym;

#Variância entre clusters
s2b <- (1/(n-1))*sum(((sba$Mi/Mm)*(sba$ym-ym))^2)

#Variância populacional
s2ym <- (N/n)*((Mm^2)*(N-n)*s2b+
               with(sba,sum(Mi*(Mi-mi)*(s2w/mi))))

#Desvio padrão populacional
sym <- sqrt(s2ym)

#Erro do inventário
##Erro absoluto
#erroabs<-qt(0.975,sum(sba$mi)-1)*sym;
erroabs <- qt(0.975, n - 1) * sym
#Erro relativo
erroperc <- erroabs/Y*100

#a.5) Intervalo de confiança
li <- Y - erroabs
ls <- Y + erroabs

cat(paste0("IC: ", round(li), " <= tau <= ", round(ls), " m³"))



