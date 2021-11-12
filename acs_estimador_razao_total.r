#ACS - ESTIMADOR DE RAZÃO PARA O TOTAL POPULACIONAL

#Estimador de razão para o total da população
parc=read.csv2('dados_raz_reg.csv');
View(parc);
comp_frag<-parc$compfrag[1];
larg_parc<-parc$larg_parc[1];
area_frag<-parc$areafrag[1];
(nt<-comp_frag/larg_parc); #Número de parcelas cabíveis

x<-parc$areaparc/10000;
y<-parc$vtcc;

plot(x,y,xlab='área da parcela(ha)',
     ylab='vtcc',pch='*',col='red')
aj<-lm(y~x)
lines(x,predict(aj));

xme=mean(x); #média estimada de x
xm=area_frag/nt; #Média populacional da variável auxiliar
yme=mean(y); #média estimada de y

R=yme/xme; R; #Estimador de razão
n=length(y);n; #Número de amostras

#varr=Variância entre valores observados e os estimados
#     pelo estimador de razão
varr=(sum(y^2)-2*R*sum(y*x)+R^2*sum(x^2))/(n-1);varr;

(ytraz<-R*area_frag); #Total(m³) 
varrtotal<-(1-(n/nt))*(1/n)*(area_frag^2)*(1/xm^2)*varr; #Variância do total
(erro_pad_total=sqrt(varrtotal)); #Erro padrão do total
(erro_inv= qt(0.975,n-1)*erro_pad_total);  #Erro do inventário (m³)
(erro_inv_perc=erro_inv/ytraz*100); #Erro do inventário(%)
