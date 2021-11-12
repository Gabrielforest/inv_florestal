#AJUSTE E SELEÇÃO DE MODELOS LINEARES SEM TRANSFORMAÇÃO DA VARIÁVEL DEPENDENTE

dados=read.csv2('cubagem.csv');
View(dados);
names(dados);

x11(); #Abrir tela de exibição das figuras do R

#Correlacao
cor(dados);
cor(dados[,c('dap','vol')])
cor(dados$dap,dados$vol);

#Modelo de Spurr
modelo<-'vicc~I(dap^2*ht)';

ajlin=lm(modelo,dados);
coef(ajlin);
sumario<-summary(ajlin);
anova(ajlin);


#Calculando os valores preditos
dados$vicc_est=predict(ajlin);
#OU
#(bs=as.vector(coef(ajlin)));
#dados$vicc_est=with(dados,bs[1]+bs[2]*dap^2*ht);
#*****

#Calculando os residuos
dados$res=residuals(ajlin);
#OU
#dados$res=dados$vicc-dados$vicc_est;
#*****

plot(dados$dap,dados$vicc, xlab='dap(cm)',ylab='vicc(m³)', pch='*',col='green');

points(dados$dap,dados$vicc_est, pch='*',col='red');

legend('topleft',legend=c('vicc_obs','vicc_est'),
       pch=c('*','*'),col=c('green','red'),bty='n',
       text.col=c('green','red'));


x11();
par(mfrow=c(2,2)); #Dividir janela gráfica

with(dados,plot(vicc_est,res,pch='*',
                xlab='Volume estimado(m³)',
                ylab='Resíduos (m³)',
                col='red'));
abline(h=0);

library(fBasics);
qqnormPlot(dados$res);
#qqnorm(dados$res);

# Normalidade: Anderson-Darling
# nortest::ad.test(dados$res)$p.value;

with(dados,plot(vicc_est,rstudent(ajlin),pch='*',
                xlab='Volume estimado(m³)',
                ylab='Resíduos studentizados',
                col='red',ylim=c(-2.5,2.5)));

abline(h=0);

(tinf<-qt(0.025,nrow(dados)-1)); #Inversa da distribuição de t
(tsup<-qt(0.975,nrow(dados)-1)); #t(alpha=5 e [n-1] gl)  

abline(h=tinf,lty=2);
abline(h=tsup,lty=2);

(tinf<-qt(0.005,nrow(dados)-1)); #Inversa da distribuição de t
(tsup<-qt(0.995,nrow(dados)-1)); #t(alpha=5 e [n-1] gl)  

abline(h=tinf,lty=2, col='blue');
abline(h=tsup,lty=2, col='blue');

hist(dados$res, freq = F,
     main='',ylab='densidade',xlab='resíduos(m³)',
     col='red');

mres<-mean(dados$res);
sdres<-sd(dados$res);
x<-dados$res;
curve(dnorm(x,mres,sdres),col='darkgreen',add=T);

###Opções para remoção de outliers

dados$rstudent<-rstudent(ajspurr);

View(dados)
View(subset(dados,rstudent<tinf|rstudent>tsup));

#dados<-subset(dados,arv!=13 & arv!=34);

identify(dados$vicc_est,rstudent(ajspurr));

