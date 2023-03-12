#Trabalho Final, Inventário fazenda 759564

# variável de interesse é o volume comercial 
# sem casca para um diâmetro mínimo de aproveitamento igual a 6cm sem casca, 
# processe o inventário florestal (α = 5%).

# Hipsometria  ---------------------------------------------------------

arv <- read.csv2("./trabalho_final/fustes.csv")

#Hipsometria por parcela

hipso <- data.frame(
  parcela = unique(arv$parcela),
  b0 = NA,
  b1 = NA
)

for(i in 1:nrow(hipso)){
  selarv <- subset(arv,
                   parcela == hipso$parcela[i] &
                     dap > 0 & !is.na(dap) &
                     ht > 0 & !is.na(ht)   
  )
  ajhipso<-lm("log(ht) ~ I(1/dap)", selarv)
  bs <- as.vector(coef(ajhipso))
  hipso$b0[i] <- bs[1]
  hipso$b1[i] <- bs[2]
}
View(hipso)

arv <- merge(arv, hipso, by = "parcela")

#Estimando as alturas totais
arv$htest <- with(arv, exp(b0+b1/dap))

arv$htre <- arv$ht
ii <- is.na(arv$ht) | arv$ht==0
arv$htre[ii] <- arv$htest[ii]

names(arv);

# volumetria --------------------------------------------------------------
cub <- read.csv2("./trabalho_final/cubagem.csv")
selarv <- subset(cub, !ht < hi)
selarv$dicc <- (cub$dicc1 + cub$dicc2)/2
selarv$dicc1 <- NULL
selarv$dicc2 <- NULL
selarv$matgen <- NULL
selarv <- selarv[, c("arv", "dap", "ht", "hi", "dicc", "espcasca")]

library (cmrinvflor)
vsmal <- smalian(selarv, dcoms = 6, htoco = 10, comcasca = FALSE, di_ou_ci = "di", dbase_ponta = 6)

cubagem <- vsmal[, c("arv", "dap", "ht", "vprod_6")]

matgen <- aggregate(list(matgen = cub$matgen),
                    list(arv = cub$arv 
                    ), mean)
cubagem <- merge(matgen, cubagem)

# Modelos -----------------------------------------------------------------
#Modelo de Berkhout (v=b0*dap^b1)
modelo <- "vprod_6 ~ b0 * dap^b1"
#ajmb = ajuste do modelo de berkhout
(ajmb <- nls(modelo, cubagem, start = list(b0 = 0.00032, b1 = 2.37201))) 
coef(ajmb)
sumario <- summary(ajmb)
#erro padrão residual modelo de berkhout
syx <- sumario$sigma
# erro padrão residual em porcentagem modelo de berkhout
syxperc <- syx/mean(cubagem$vprod_6) * 100

#Modelo de Spurr (v=b0+b1*dap^2ht)
modeloS <- "vprod_6 ~ I(dap^2*ht)"
#ajms= ajuste modelo de spurr
ajms <- lm(formula = modeloS, data = cubagem)
coef(ajms)
spurr <- summary(ajms)
#sys= erro padrão residual modelo de spurr (m³)
sys <- spurr$sigma
#sysp= erro padrão residual do modelo de spurr em porcentagem
sysp <- round(sys/mean(cubagem$vprod_6)*100,2)
#coeficiente de determinação
R2adj <- spurr$adj.r.squared 
#coeficiente de determinação
R2adjp <- round(R2adj * 100, 2) 

#Modelo de Spurr logaritmico (ln(v)=b0+b1*ln(dap^2*ht))
modeloSL <- "log(vprod_6) ~ log(dap^2 * ht)"
#ajmsl=ajuste modelo spurr logaritmico
ajmsl <- lm(modeloSL, cubagem)
print(summary(ajmsl))
anova(ajmsl)
##Índice de Furnival 
##Para o cálculo do IF deve-se calcular a inversa da média geométrica da derivada da variável dependente e, em seguida,
##multiplicar pelo erro padrão residual obtido no ajuste com a variável transformada

y <- cubagem$vprod_6
D(expression(log(y)), "y")
dy <- 1/y

# Média geométrica# Média geométrica
(medgeo <- exp(mean(log(dy), na.rm = TRUE))) 
spurrlog <- summary(ajmsl)
#sysl-erro padrão da média com escala convertida de lnm³ para  m³ 
sysl <- 1/medgeo * spurrlog$sigma
#syslp- erro padrão spurr log corrigido
syslp <- sysl/mean(y) * 100
#coeficiente de determinação spurr log
R2adjsl <- spurrlog$adj.r.squared 
#coeficiente de determinação spurr log
R2adjpsl <- round(R2adjsl*100, 2) 

#Modelo de Schumacher & Hall
modeloSH <- "vprod_6 ~ b0 * dap^b1 * ht^b2"
(ajsh <- nls(modeloSH, cubagem, start = list(b0 = pi/40000 * 0.45, b1 = 2, b2 = 1)))
coef(ajsh)
(schumacher <- summary(ajsh))
#Erro padrão residual
(sysh <- schumacher$sigma) 
#Syx percentual
(syshp<-sysh/mean(cubagem$vprod_6) * 100)

#Modelo de Schumacher & Hall (Logaritmico); ln(vicc) = b0+b1*ln(dap)+b2*ln(ht)
modeloSHL <- "log(vprod_6) ~ log(dap) + log(ht)"

###Ajuste de modelos lineares múltiplos
ajshl <- lm(formula = modeloSHL, data = cubagem)
schumacherl <- summary(ajshl)
coef(ajshl)
#sysl-erro padrão da média com escala convertida de lnm³ para  m³ 
syshl <- 1/medgeo * schumacherl$sigma
#syslp- erro padrão spurr log corrigido
syshlp <- syshl/mean(y) * 100
#coeficiente de determinação spurr log
R2adjsl <- schumacherl$adj.r.squared
#coeficiente de determinação spurr log
R2adjpsl <- round(R2adjsl*100,2) 

#Modelo de Takata v=dap²ht/b0+b1*dap
modeloT <- "vprod_6 ~ dap^2 * ht/ (b0 + b1 * dap)"
(ajt <- nls(modeloT,cubagem, start = list(b0 = 22667.6, b1 = 426.264)))
coef(ajt)
(takata <- summary(ajt))
#Erro padrão residual
(syt <- takata$sigma) 
#Syx percentual
(sytp <- syt/mean(cubagem$vprod_6) * 100)

#calculando os volumes preditos
#berkhout est
(cubagem$vprod_6estberk <- predict(ajmb))
#spurr est
(cubagem$vprod_6estspurr <- predict(ajms))
# fator de meyer
(fc <- exp(0.5 * spurrlog$sigma^2))
#spurr log est
cubagem$vprod_6estspurrlog <- exp(predict(ajmsl))*fc
#schumacher est
(cubagem$vprod_6estschum <- predict(ajsh))

#fator de meyer
(fm <- exp(0.5 * schumacherl$sigma^2))
#schumacher log est
cubagem$vprod_6estchumlog <- exp(predict(ajshl))*fm
#takata est
(cubagem$vprod_6esttakata <- predict(ajt))

#calculando os resíduos
cubagem$resberk <- residuals(ajmb);# res berkhout
cubagem$resspurr <- residuals(ajms);# res spurr
cubagem$resspurrlog <- cubagem$vprod_6-cubagem$vprod_6estspurrlog;#res spurr log
cubagem$resschumacher <- residuals(ajsh);# res schumacher log
cubagem$resschumacherlog <- cubagem$vprod_6-cubagem$vprod_6estchumlog;#res schumacher log
cubagem$restakata <- residuals(ajt);# res takata

library(fBasics);
par(mfrow=c(3,2))
qqnormPlot((cubagem$resberk),title = FALSE, main = "Berkhout");
qqnormPlot((cubagem$resspurr),title = FALSE, main = "Spurr");
qqnormPlot((cubagem$resspurrlog),title = FALSE, main = "Spurr log");
qqnormPlot((cubagem$resschumacher),title = FALSE, main = "Schumacher & Hall");
qqnormPlot((cubagem$resschumacherlog),title = FALSE, main = "Schumacher & Hall log");
qqnormPlot((cubagem$restakata),title = FALSE, main = "Takata");

#grafico de residuos

with(cubagem,plot(vprod_6estberk,resberk,pch="*",#berkhout
                  main="Berkhout",
                  xlab="Volume estimado(m³)",
                  ylab="Resíduos (m³)",
                  col="red",ylim=c(-0.05,0.05)));
abline(h=0);

with(cubagem,plot(vprod_6estspurr,resspurr,pch="*",
                  main="Spurr",
                  xlab="Volume estimado(m³)",
                  ylab="Resíduos (m³)",
                  col="red",ylim=c(-0.05,0.05)));
abline(h=0);

with(cubagem,plot(vprod_6estspurrlog,resspurrlog,pch="*",
                  main="Spurr log",
                  xlab="Volume estimado(m³)",
                  ylab="Resíduos (m³)",
                  col="red",ylim=c(-0.05,0.05)));
abline(h=0);

with(cubagem,
     plot(vprod_6estschum,resschumacher,pch="*",
          main="Schumacher",
          xlab="Volume estimado(m³)",
          ylab="Resíduos (m³)",
          col="red",ylim=c(-0.05,0.05)));
abline(h=0);

with(cubagem,
     plot(vprod_6estchumlog,resschumacherlog,pch="*",
          main="Schumacher log",
          xlab="Volume estimado(m³)",
          ylab="Resíduos (m³)",
          col="red",ylim=c(-0.05,0.05)));
abline(h=0);

with(cubagem,plot(vprod_6esttakata,restakata,pch="*",#berkhout
                  main="Takata",
                  xlab="Volume estimado(m³)",
                  ylab="Resíduos (m³)",
                  col="red",ylim=c(-0.05,0.05)));
abline(h=0);

selarv <- subset(arv,!is.na(dap) & !is.na(htre) 
                 & dap>0 & htre>0)

# selecionado o modelo de schumacher
(bs<-as.vector(coef(ajsh)))

#Schumacher & Hall            Foi o melhor
selarv$vprod_6 <- (bs[1] * (selarv$dap ^ bs[2]) * (selarv$htre^bs[3]))

# ace ---------------------------------------------------------------------
sig <- 0.05

inv <- aggregate(list(vary = selarv$vprod_6),
                 list(fazenda = selarv$codfazenda,
                      codplantio = selarv$codplantio,
                      areaplantio = selarv$areaplantio,
                      estrato = selarv$matgen,
                      parcela = selarv$parcela,
                      areaparc = selarv$areaparc
                 ),sum)
estrato <- aggregate(list(areaest = inv$areaplantio),
                     list(estrato = inv$estrato)
                     ,sum)
amostra <- inv[,c('estrato','parcela','areaparc','vary')]

ace <- as.data.frame(estats_ace(estrato, amostra, sig = sig*100, fc_dim=1/1000))
totace <- with(ace, ymstr * np)
errototace <- with(ace, eunid * np)
litotace <- totace - errototace
lstotace <- totace + errototace

print(paste('IC:',round(litotace),'<=T<=',round(lstotace),'m³',sep=''))
