# -------------------- CALCULOS ---------------------- #
# Entrando com os valores constantes
comp_frag <- 2760
area_frag <- 284
larg_frag <- 5

volumeporfaixa<-aggregate(list(volume=parc$volume), list(faixa=parc$faixa), sum); View(volumeporfaixa)
teste<-aggregate(list(parcelas=parc$parcela),list(faixa=parc$faixa), length); View(teste)
base<-merge(volumeporfaixa,teste); View(base)
base$areafrag<-areafrag
base$comprimento<-comprimento
base$largura<-largura
base$areafaixa<-base$parcelas*70*5/10000

x<-base$areafaixa
y<-base$volume #vari?vel de interesse

nt <- comp_frag/larg_frag
nt


# Plotando os gr?ficos da rela??o entre ?rea de parcela e volume
plot(x, y, xlab='Área da parcela', ylab='vtcc', pch='*', col='purple')

# Ajuste linear entre as duas vari?veis
aj <- lm(y~x)

# Adicionando ao gr?fico, linha da rela??o linear entre parcela e volume
lines(x, predict(aj))

# M?dia estimada de x (em ha)
xme <- mean(x)
xme

# M?dia populacional da vari?vel auxiliar
xm <- area_frag/nt
xm

# M?dia estimada de y (em m?)
yme <- mean(y)
yme

# Quantidade de amostras
n <- length(y)
n

# Estimador de regress?o (j? em m?/ha)
b <- sum((y-yme)*(x-xme))/sum((x-xme)^2)
b
# Mesma coisa, muito mais f?cil:
b <- as.numeric(coef(aj)[2])
b

# M?dia estimada pelo estimador de regress?o
ymreg <- yme+b*(xm-xme)
ymreg

# Por??o da vari?ncia de Y que n?o pode ser explicada pela rela??o linear com X (S?yx)
varyx <- sum((y-yme-b*(x-xme))^2)/(n-2)
varyx

# Vari?ncia da m?dia estimada [m^6]
varrmedia <- (1-n/nt)*(1/n)*varyx
varrmedia

# Erro padr?o da m?dia estimada [m?]
erro_pad_med <- sqrt(varrmedia)
erro_pad_med

# Erro invent?rio em m?
erro_inv <- qt(0.975, n-1)*erro_pad_med
erro_inv

# Erro do invent?rio em porcentagem
erro_inv_perc <- (erro_inv/ymreg)*100
erro_inv_perc

# Passando para hectare (utilizar m?dia , utilizada para estimar)
ymreg_ha <- ymreg/xm
ymreg_ha

# Sem corre??o
yme/xm

# ------------------------------ SA?DA ----------------------------- #
cat(paste('Media(m?/parc) = ', round(ymreg,2), sep=''),'\n');
  # cat -> para mudar de linha
  # paste -> concatenar informa??es

total_fazenda <- ymreg_ha*area_frag
total_fazenda # Em m?

erro_inv_fazenda <- erro_inv*area_frag
erro_inv_fazenda # Em m?

paste('IC:',(total_fazenda-erro_inv_fazenda),'m?/parcela a',(total_fazenda+erro_inv_fazenda),'m?/parcela') #Intevalo de confian?a








