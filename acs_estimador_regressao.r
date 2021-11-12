#ACS - ESTIMADOR DE REGRESSÃO

parc<-read.csv2('dados_raz_reg.csv');
View(parc);
names(parc);

(comp_frag<-mean(parc$compfrag));
(larg_parc<-mean(parc$larg_parc));
(area_frag<-mean(parc$areafrag));

#Número de parcelas cabíveis
(nt<-comp_frag/larg_parc);

(x<-parc$areaparc/10000);
(y<-parc$vtcc);

x11();
plot(x,y,xlim=c(0,max(x)),ylim=c(0,max(y)),
     xlab='área da parcela(ha)',ylab='Vtcc (m³)',pch='*', col='darkblue');

aj<-lm(y~x);
lines(x,predict(aj));
summary(aj);

(xme<-(mean(x))); #Média estimada de x
(xm<-area_frag/nt); #Média populacional da variável auxiliar
(yme<-mean(y)); #Média estimada de y
(n<-length(y)); #Número de amostras

(b<-sum((y-yme)*(x-xme))/sum((x-xme)^2));
#OU
(b<-as.vector(coef(lm(y~x))[2]));

#varyx: Porção da variância de y que não pode ser explicada pela relação
#linear com x
(varyx=sum((y-yme-b*(x-xme))^2)/(n-2));

(ymreg<-yme+b*(xm-xme)); #Média da parcela

(varmedia<-(1-n/nt)*(1/n)*varyx); #Variância da média estimada
(erro_pad_media<-sqrt(varmedia)); #Erro padrão da média

(erro_inv<-qt(0.975,n-1)*erro_pad_media); #Erro do inventário (m³/parcela)
(erro_invperc<-round(erro_inv/ymreg*100,2)); #Erro do inventário (m³/parcela)

(ytreg<-ymreg*nt); #m³/população
(yhareg<-ytreg/area_frag); #m³/ha




