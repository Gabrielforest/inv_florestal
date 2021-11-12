
# Preocupado com o seu rendimento na disciplina de “Dendrometria” você perguntou aleatoriamente a nota obtida por
# outros alunos que frequentaram a disciplina, as quais foram: 9,17; 7,85; 6,00; 8,65; 6,00; 5,13; 7,78; 7,45; 7,68;
# 6,54; 7,89; 3,04; 7,29; 3,40; 2,20; 7,31; 6,00; 1,40.
# Sabendo-se que 351 alunos frequentaram esta disciplina e, considerando α = 3%, responda os itens abaixo: (20%)
# (a) A nota média estimada para a disciplina Dendrometria da UFSCar.
# (b) Calcule o erro percentual da amostragem (precisão).
# (c) Sabendo-se que a média populacional das notas foi igual a 6, 28, qual foi a diferença percentual entre a média
# populacional e a média estimada (acurácia).
# (d) Qual o intervalo de confiança para a média das notas? Interprete.
# (e) Você foi "iludido"pela sua amostra? Justifique.
# 
# (f) Considerando que as notas possuem distribuição normal truncada [0;10] e que a sua nota (8) possui uma proba-
#   bilidade acumulada igual a 0, 8415, você foi um "outlier"? Justifique. (Considere α = 5%).
# 
# (g) Continuando a considerar que as notas possuem distribuição normal, qual é a probabilidade [%] de outro aluno
# ter tido rendimento superior ao seu?




# Preocupado com seu rendimento em dendrometria vc perguntou aleatoriamente
# o rendimento dos colegas. 
# Sabendo que  a turma possui 198 alunos, considerando alpha 5%, responda:
notas <- c(9.17, 7.85, 6.00, 8.65, 6.00, 5.13, 7.78, 7.45, 7.68,
           6.54, 7.89, 3.04, 7.29, 3.40, 2.20, 7.31, 6.00, 1.40)



# a) Média Estimada -------------------------------------------------------

round(mean(notas),2)

# b) Erro Percentual de Amostragem ----------------------------------------

#Tamanho da população:
N <- 351

estats_acs <- as.data.frame(cmrinvflor::estats_acs(vy = notas, nt = N, sig = 3))
estats_acs$eperc

# OU

#Tamanho da amostra:
n <- length(notas)

#Variância da media:
varmed <- (var(notas) / n) * (1 - n / N)

#Erro padrão da média                -- sdymed:
desvmed <- sqrt(varmed)

#Erro                                -- eunid:
erro <- qt(0.985, n - 1) * desvmed

#Erro em porcentagem                 --  eperc:
erro_perc <- erro/mean(notas) *100


# c) Sabendo que a média pop. foi de 6,46, acurácia em % é: ---------------
## se der negativo, fica negativo (superestimei a nota):

((6.28 - mean(notas)) / 6.28) * 100


# d) Intervalo de Confiança -----------------------------------------------

estats_acs <- as.data.frame(cmrinvflor::estats_acs(vy = notas, nt = N, sig = 3))
estats_acs$li
estats_acs$ls


# e) Você foi iludido pela sua amostra? -----------------------------------

# Não pois a média populacional se encontra dentro do intervalo de confiança.


# f) Considerando que as notas possuem distribuição normal truncada [0;10] -----
# e que sua nota (6.02) possui uma prob acumulada de 0.8415, vc foi um outlier?
# Justifique com alpha = 5%:
-2 * sdy + ymed_pop <= ymed_pop <= 2 * sdy + ymed_pop

paste0(-2 * sd(notas) + 6.46, 
       " <= ", 6.46, " <= ", 
       2 * sd(notas) + 6.46)
# Ou
# se a prob. acum. vai até 0.8415, numa distribuição normal
# de 0 até 1 a faixa de valores menores do que 0.025 tanto 
# negativos quanto positivos seriam outliers, logo
# nesse caso não sou um outlier por estar quase no centro
# da distribuição


# g) Qual a prob. de um aluno ter rendimento melhor que o meu? ------------

1 - 0.8415





#Você precisa estimar o volume de madeira estocado no pátio de uma fábrica de celulose. A profundidade da pilha é
#definida pelo comprimento das toras, as quais possuem 6 metros. O comprimento total da pilha é de 5430 metros, por
#sua vez, ao longo da pilha as alturas são variáveis. Para resolver este problema você amostrou aleatoriamente algumas
#alturas (metros) ao longo da pilha e os resultados das medições foram: 3,66; 3,75; 3,75; 3,73; 3,46; 4,03; 4,00;
#4,05; 3,44; 3,68; 3,85; 3,80; 4,04; 3,95; 4,09 (20%)
#A partir da amostra, estime:

#(a) O intervalo de confiança para o volume estereo de madeira estocado no pátio (α = 6%).

#(b) O tamanho da amostra para erro amostral igual a 2%
#De posse do total amostral e dados os seguintes grandes números (f ator_empilhamento = 1, 73, dbm = 0, 51g/cm3
#                                                                 e ima = 43m3/ha.ano), estime:
  
#(c) O consumo de madeira em toneladas/ano, sabendo-se que o estoque do pátio é suficiente para 01 semana 
#(Considere que um ano possui 52 semanas).

#(d) A área mínima de efetivo plantio que a empresa deve possuir para abastecer ininterruptamente a fábrica.




alturas <- c(3.66, 3.75, 3.75, 3.73, 3.46, 4.03, 4.00, 4.05, 3.44, 
             3.68, 3.85, 3.80, 4.04, 3.95, 4.09)


# Toras empilhadas --------------------------------------------------------

#Você precisa estimar o volume de madeira estocado no pátio de uma fábrica de celulose. As toras 
#tem 3,6m de comprimento e as pilhas de madeira tem comprimento total de 4797m, mas as alturas das
#das pilhas variam. Para resolver este problema você amostrou aleatoriamente algumas alturas das 
#pilhas e o resultado das medidas foram: 3,95; 3,77; 4,2; 4,49; 4,31; 3,71; 4,04; 4,27; 3,92; 
#4,16. 
#A partir da amostra estime:

#a)Intervalo de confiança para o volume total de madeira no pátio (alfa=5%)

#Exerc 2:
ht=c(3.66, 3.75, 3.75, 3.73, 3.46, 4.03, 4.00, 4.05, 3.44, 
     3.68, 3.85, 3.80, 4.04, 3.95, 4.09);

#a)Nota m?dia:
nm=mean(ht);

#b)Intervalo de confian?a:
n=length(ht);
N=Inf;
tvalor=qt(0.970, n-1);

snm=sqrt((var(ht)/n)*(1-n/N)); #Erro padr?o da m?dia
erroinv=tvalor*snm;

li=(nm-erroinv)*5430*3.6;
ls=(nm+erroinv)*5430*3.6;


nacs=cmrinvflor::estats_acs(vy = ht,emperc =2)$sa;

#c)
tot=nm*5430*3.6; #Total populacional, para 1 semana
tot_ano_mst=tot*52; #em mst
tot_ano_m3=tot_ano_mst/1.73;
tot_ano_ton=tot_ano_m3*0.51; #densidade=0.5ton/m3

#d)
tot_ano_m3/43; #IMA=40

#OU
area_ano=tot_ano_m3/258; #240=IMA*6 anos
ciclo=6;
area_total=area_ano*ciclo
