#ACS - ESTIMADOR DE RAZÃO PARA A MEDIA

parc<-read.csv2('dados_raz_reg.csv');
View(parc);

x=parc$areaparc/10000;
y=parc$vtcc;

x11();
plot(x,y,xlab='área da parcela',
     ylab='vtcc',pch='*',col='red',
     xlim=c(0,max(x)),ylim=c(0,max(y)));

aj=lm(y~x);
summary(aj);

(xme=mean(x)); #Média estimada de x (variável independente)
(nt=parc$compfrag[1]/parc$larg_parc[1]);
(xm=parc$areafrag[1]/nt);#Média populacional de x

(yme=mean(y)); #Média estimada de y (variável dependente)

(R=yme/xme); #Estimador de razão
(n=length(y)); #Número de amostras

#varr=Variancia entre valores observados e os estimados pelo
#estimador de razão
(varr=(sum(y^2)-2*R*sum(y*x)+R^2*sum(x^2))/(n-1));

(ymraz=R*xm); #Média por parcela (m3/parc);

(varmedia=(1-n/nt)*(1/n)*varr); #Variância da média estimada

(erro_pad_media=sqrt(varmedia)); #Erro padrão da média

(erro_inv=qt(0.975,n-1)*erro_pad_media); #Erro do inventário (m3/parc)

(erro_inv_perc=erro_inv/ymraz*100); #Erro do inventário (%)

(ytraz=ymraz*nt); #Total populacional (m3)
(ymraz_ha=ytraz/parc$areafrag[1]); #(m3/ha)

