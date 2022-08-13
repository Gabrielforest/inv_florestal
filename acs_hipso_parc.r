#AMOSTRAGEM CASUAL SIMPLES - HIPSOMETRIA POR PARCELA

arv<-read.csv2('tbl.csv');
View(arv);
names(arv);

arv$dap <- arv$CAP/pi
names(arv) <- c("populacao", "parcela", "arvore", "fuste", "cap", "ht", "esp_casca", "obs", "dap")


#Hipsometria por parcela

hipso<-data.frame(
  parcela=unique(arv$parcela),
  b0=NA,
  b1=NA
)


for(i in 1:nrow(hipso)){
  selarv<-subset(arv,
                 parcela==hipso$parcela[i] &
                 dap>0 & !is.na(dap) &
                 ht>0 & !is.na(ht)   
                 );
  ajhipso<-lm('log(ht)~I(1/dap)',selarv);
  bs<-as.vector(coef(ajhipso));
  hipso$b0[i]<-bs[1];
  hipso$b1[i]<-bs[2];
}
View(hipso)

arv<-merge(arv,hipso,by='parcela');


#Estimando as alturas totais
arv$htest<-with(arv,exp(b0+b1/dap));

x11();
#hist(arv$htest[arv$htest>0]);
#hist(arv$ht[arv$ht>0],col='red',add=T);

with(arv,plot(ht~dap,xlab='dap(cm)',
              ylab='ht(m)',pch='*',col='green'))
with(arv,points(htest~dap,pch='*',col='red'));

#arv$htest[arv$dap==0|is.na(dap)]<-0

arv$htre<-arv$ht;
ii<-is.na(arv$ht) | arv$ht==0;
arv$htre[ii]<-arv$htest[ii];

arv$vicc<-with(arv,6.436159e-05*dap^1.852143*htre^9.530665e-01);

names(arv);

arv$arvtal <- 1
arv$areatal <- 17
arv$areaparc <- 314

arv <- subset(arv, !is.na(vicc))

parc<-aggregate(list(vol=arv$vicc),
                list(talhao=arv$arvtal,
                     areatal=arv$areatal,
                     parcela=arv$parcela,
                     areaparc=arv$areaparc
                     ),sum);
View(parc);

#Variável de interesse
y<-parc$vol;

#Média
(ymed<-mean(y));

#Variância
(yvar<-var(y));

#Desvio padrão
(ydesv<-sd(y));
#ou
(ydesv<-sqrt(yvar));

#Tamanho da amostra
(n<-length(y));

#Área da parcela
(areaparc<-mean(parc$areaparc));

#Área da fazenda
talhao<-subset(parc,!duplicated(talhao),c('talhao','areatal'));
View(talhao);
(areafaz<-sum(talhao$areatal));

#Intensidade amostral
(ia<-areafaz/n);

#Número de parcelas cabíveis
(N<-areafaz*10000/areaparc);

#Variância da media
(yvarmed<-(yvar/n)*(1-n/N));

#Erro padrão da média
(ydesvmed<-sqrt(yvarmed));

#Erro do inventário
(erroinv<-qt(0.975,n-1)*ydesvmed)

#Erro do inventário em porcentagem

(erroinvperc<-erroinv/ymed *100);

#Média por ha
(yha<-ymed*(10000/areaparc));
#Erro inventário por ha
(erroinvha<-erroinv*(10000/areaparc));

#Total populacional
(ytot<-yha*areafaz);
#Erro inventário para a população
(erroinvtot<-erroinvha*areafaz);

#Intervalo de confiança para a população

cat(paste(round(ytot-erroinvtot,0),'<= total <=',
          round(ytot+erroinvtot,0),'m³\n'));



