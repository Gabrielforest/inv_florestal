#AJUSTE E SELEÇÃO DE MODELOS LINEARES MÚLTIPLOS COM VARIÁVEL DEPENDENTE TRANSFORMADA

dados<-read.csv2('cubagem.csv');
#View(dados);

modelo<-'log(vicc)~log(dap)+log(ht)';

###Ajuste de modelos lineares múltiplos
ajsch<-lm(modelo,dados);
print(summary(ajsch));
anova(ajsch);


##Índice de Furnival 
##Para o cálculo do IF deve-se calcular a inversa da média geométrica da derivada da variável dependente e, em seguida,
##multiplicar pelo erro padrão residual obtido no ajuste com a variável transformada

y<-dados$vicc;
D(expression(log(y)),'y');
dy<-1/y;

(medgeo<-exp(mean(log(dy), na.rm=T))); # Média geométrica
#OU
#(medgeo<-prod(dy)^(1/length(dy))); # Média geométrica
#OU
#library(psych);
#(medgeo<-geometric.mean(dy,na.rm=TRUE));

(IF<-1/medgeo*sumario$sigma);

##Fator de correção da discrepância logarítmica de Meyer para as estimativas das variáveis dependentes
(fc<-exp(0.5*sumario$sigma^2));
viccest<-exp(predict(ajsch))*fc;

##Análise gráfica dos resíduos

x11();
par(mfrow=c(1,2));

residuos<-dados$vicc-viccest;

#vest<-exp(predict(ajsch));
#residuos<-dados$vicc-vest;

plot(vest,residuos,
     pch='*',col='blue',
     xlab='vicc estimado [m³]',ylab='erro[m³]'
     );
abline(h=0,lty=2);

plot(vest,rstudent(ajsch),
     pch='*',col='blue',
     xlab='vicc estimado [m³]',
     ylab='erro studentizado');
abline(h=0,lty=2);

(tinf<-qt(0.025,nrow(dados)-1));
(tsup<-qt(0.975,nrow(dados)-1));

abline(h=tinf,lty=3,col='red');
abline(h=tsup,lty=3,col='red');