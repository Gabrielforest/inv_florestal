dados<-read.csv2('conglomerado.csv');
View(dados);

dados$mi=dados$nsubunid;
dados$yi=dados$vol;

#Definições
#N = Número de clusters cabíveis na população
#n = Número de clusters amostrados pela ACS
#M = Número de elementos cabíveis na população
#mi= Número de elementos no iésimo cluster
#mm= Tamanho médio dos clusters que compõem a amostra
#Mm= Tamanho médio dos clusters que compõem a população

N=506;
M=2706;
(n=nrow(dados));

#a) Estimador da média da população

#a.1)Média

(ym=sum(dados$yi)/sum(dados$mi));

#a.2)Variância da média

#Obs: Se M não é conhecido o Mm da equação da variância
#da média pode ser estimado

#Caso o M não for conhecido
(Mm=sum(dados$mi)/n);

#Caso o M for conhecido
(Mm=M/N);

(s2ym=((N-n)/N)*(1/n)*(1/Mm^2)*
      sum((dados$yi-dados$mi*ym)^2)/(n-1)); #(m6/unidade)

#a.3) Erro padrão da média
(sym=sqrt(s2ym)); #(m3/unidade)

#a.4) Erro do inventário
#a.4.1 Erro absoluto
(erroabs<-qt(0.975,n-1)*sym)

#a.4.2 Erro em porcentagem
(erroperc<-erroabs/ym*100);

#Intervalo de confiança
(li=ym-erroabs); #li: limite inferior do IC
(ls=ym+erroabs); #ls: limite superior do IC



#b) Estimador da população total quando o número
#de elementos (M) é conhecido

#b.1) Total populacional
(Y=M*sum(dados$yi)/sum(dados$mi)); #(m3/povoamento);

#b.2) Variância da população

(Mm=M/N);

(s2Mym=(N^2)*((N-n)/N)*(1/n)*
       sum((dados$yi-dados$mi*ym)^2)/(n-1));

#b.3) Erro padrão da população
(sMym=sqrt(s2Mym));

#b.4) Erro do inventário

#b.4.1) Erro absoluto
(erroabs=qt(0.975,n-1)*sMym);

#b.4.2) Erro em porcentagem
(erroperc=erroabs/Y*100);

#b.5)Intervalo de confiança
(li=Y-erroabs); #li: limite inferior do IC
(ls=Y+erroabs); #ls: limite superior do IC

#c) Estimador da população total quando o número
#de elementos (M) NÃO é conhecido

#c.1) Total populacional
(Y=(N/n)*sum(dados$yi)); #(m3/povoamento);

#c.2) Variância da população

(ymed<-mean(dados$yi));

(s2Nym=(N^2)*((N-n)/N)*(1/n)*
       sum((dados$yi-ymed)^2)/(n-2));

#c.3) Erro padrão da população
(sNym=sqrt(s2Nym));

#c.4) Erro do inventário

#c.4.1) Erro absoluto
(erroabs=qt(0.975,n-1)*sNym);

#c.4.2) Erro em porcentagem
(erroperc=erroabs/Y*100);

#c.5)Intervalo de confiança
(li=Y-erroabs); #li: limite inferior do IC
(ls=Y+erroabs); #ls: limite superior do IC
