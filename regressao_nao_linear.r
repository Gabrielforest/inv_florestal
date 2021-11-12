#AJUSTE E SELEÇÃO DE MODELOS NÃO LINEARES

dados=read.csv2('cubagem.csv');
View(dados);
names(dados);

x11(); #Abrir tela de exibição das figuras do R

#Modelo de Schumacher & Hall
modelo<-'vicc~b0*dap^b1*ht^b2';
  
(ajnlin=nls(modelo,dados,
            start=list(b0=pi/40000*0.45,b1=2,b2=1)));
coef(ajnlin);
(sumario<-summary(ajnlin));

(syx<-sumario$sigma); #Erro padrão residual
(syxperc<-syx/mean(dados$vicc)*100); #Syx percentual

cat(paste('Erro padrão residual: ', round(syxperc,2),'%',sep=''));

plot(dados$dap,dados$vicc,
     xlab='dap(cm)',ylab='vicc(m³)',
     pch='*',col='green');

points(dados$dap,predict(ajnlin),
       pch='*',col='red');

legend('topleft',legend=c('vicc_obs','vicc_est'),
       pch=c('*','*'),col=c('green','red'),bty='n',
       text.col=c('green','red'));

#Calculando os valores preditos
(dados$vicc_est=predict(ajnlin));
#OU
(bs=as.vector(coef(ajnlin)));
dados$vicc_est=with(dados,bs[1]*dap^bs[2]*ht^bs[3]);
#*****

#Calculando os residuos
dados$res=residuals(ajnlin);
#OU
dados$res=dados$vicc-dados$vicc_est;
#*****

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

dados$res_padronizado<-dados$res/sumario$sigma;
  
with(dados,plot(vicc_est,res_padronizado,pch='*',
                xlab='Volume estimado(m³)',
                ylab='Resíduos padronizados',
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

dados$res_padronizado<-dados$res/sumario$sigma;

View(dados)
View(subset(dados,res_padronizado<tinf|res_padronizado>tsup));

#dados<-subset(dados,arv!=13 & arv!=34);


with(dados,plot(vicc_est,res_padronizado,pch='*',
                xlab='Volume estimado(m³)',
                ylab='Resíduos padronizados',
                col='red',ylim=c(-2.5,2.5)));
abline(h=0);
identify(dados$vicc_est,dados$res_padronizado);



