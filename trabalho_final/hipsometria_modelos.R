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
                     dap > 5 & !is.na(dap) &
                     ht > 0 & !is.na(ht)   
  )
  ajhipso <- lm("log(ht) ~ I(1/dap)", selarv)
  bs <- as.vector(coef(ajhipso))
  hipso$b0[i] <- bs[1]
  hipso$b1[i] <- bs[2]
}
View(hipso)

arv <- merge(arv, hipso, by = "parcela")

#Estimando as alturas totais
arv$htest <- with(arv, exp(b0+b1/dap))

#x11();
#hist(arv$htest[arv$htest>0]);
#hist(arv$ht[arv$ht>0],col="red",add=T);


# deixar melhor esse gráfico ----------------------------------------------

with(arv, plot(ht ~ dap, xlab = "dap(cm)",
               ylab = "ht(m)", pch = "*", col = "green"))
with(arv, points(htest ~ dap, pch = "*", col = "red"))

#arv$htest[arv$dap==0|is.na(dap)]<-0

arv$htre <- arv$ht
ii <- is.na(arv$ht) | arv$ht==0
arv$htre[ii] <- arv$htest[ii]

names(arv)

#lendo os cubagem de cubagem e cubagem rigorosa:
cub <- read.csv2("./trabalho_final/cubagem.csv")

cubagem <- read.csv2("./trabalho_final/cubagem_rigorosa.csv")

# agrupando os cubagem de interesse:
cubagem <- cubagem[, c("arv", "dap", "ht", "vprod_6")]

matgen <- aggregate(list(matgen = cub$matgen),
                    list(arv = cub$arv 
                    ), mean)
cubagem <- merge(matgen, cubagem)



# Modelos -----------------------------------------------------------------


# formulas que serão usadas: ----------------------------------------------

formulas <- list(
  "vprod_6 ~ I(b0 * dap^b1)",                     #1 não linear
  "vprod_6 ~ I(dap^2 * ht)",                      #2 linear
  "log(vprod_6) ~ I(log(dap^2 * ht))",            #3 linear
  "vprod_6 ~ I(b0 * dap^b1 * ht^b2)",             #4 não linear
  "log(vprod_6) ~ log(dap) + log(ht)",            #5 linear
  "vprod_6 ~ I((dap^2 * ht) / (b0 + b1 * dap))")  #6 não linear

names(formulas) <-  list( 
  a = "berkhout",
  b = "spurr",
  c = "spurr_log",
  d = "schumacher",
  e = "schumacher_log",
  f = "takata"
)

formulas


# ajustando os modelos: --------------------------------------------------


### não lineares:
berkhout <- nls(data = cubagem,
                formula = formulas$berkhout,
                start = list(b0 = 0.000098, b1 = 2.48389)
)

schumacher <- nls(data = cubagem,
                  formula = formulas$schumacher,
                  start = list(b0 = pi/(40000*0.45), b1 = 2, b2 = 1)
)

takata <- nls(data = cubagem,
              formula = formulas$takata,
              start = list(b0 = 29466.2422, b1 = 10.9759)
)

#lista com modelos não lineares: berkhout[[1]], schumacher[[2]], takata[[3]]
lista_nls <- list(berkhout, schumacher, takata)
names(lista_nls) <- list(
  a = "Berkhout",
  b = "Schumacher",
  c = "Takata"
)


### lineares:
spurr <- lm(data = cubagem,
            formula = formulas$spurr
)

spurr_log <- lm(data = cubagem,
                formula = formulas$spurr_log
)

schumacher_log <- lm(data = cubagem,
                     formula = formulas$schumacher_log
)

#lista com modelos lineares: spurr[[1]], spurr_log[[2]], schumacher_log[[3]]
lista_lm <- list(spurr, spurr_log, schumacher_log)
names(lista_lm) <- list(
  a = "Spurr",
  b = "Spurr log.",
  c = "Schumacher log."
)

# valores estimados -------------------------------------------------------

cubagem$vprod_6_est_berkhout <- predict(berkhout)

cubagem$vprod_6_est_spurr <- predict(spurr)

cubagem$vprod_6_est_spurr_log <- predict(spurr_log)

cubagem$vprod_6_est_schumacher <- predict(schumacher)

cubagem$vprod_6_est_schumacher_log <- predict(schumacher_log)

cubagem$vprod_6_est_takata <- predict(takata)


# berkhout == 6, schumacher == 9 e takata == 11:
cubagem_nls <- cubagem[, c(6, 9, 11)]

# spurr == 7, spurr_log == 8 e schumacher_log == 10:
cubagem_lm <- cubagem[, c(7, 8, 10)]


# gráficos ----------------------------------------------------------------

library(ggplot2)
library(fBasics)
library(ggpubr)

par(mfrow = c(3,2))

# gráficos de normalidade:
qqnorm_berkhout <- qqnormPlot(residuals(berkhout), title = FALSE, main = "Berkhout")

qqnorm_spurr <- qqnormPlot(residuals(spurr), title = FALSE, main = "Spurr")

qqnorm_spurr_log <- qqnormPlot(residuals(spurr_log), title = FALSE, main = "Spurr log.")

qqnorm_schumacher <- qqnormPlot(residuals(schumacher), title = FALSE, main = "Schumacher & Hall")

qqnorm_schumacher_log <- qqnormPlot(residuals(schumacher_log), title = FALSE, main = "Schumacher & Hall log.")

qqnorm_takata <- qqnormPlot(residuals(takata), title = FALSE, main = "Takata")

par(mfrow = c(1,1))


# gráficos de resíduos:

#Inversa da distribuição de t
tinf <- qt(0.025, nrow(cubagem) - 1)

#t(alpha=5 e [n-1] gl)  
tsup <- qt(0.975, nrow(cubagem) - 1)



# cubagem_nls == vprod_6 estimados para os modelos não lineares
# lista_nls == lista dos modelos não lineares ajustados (Berkhout Schumacher e Takata)

nls_plots <- vector("list", 3)

names(nls_plots) <- list(a = "Berkhout", 
                         b = "Schumacher", 
                         c = "Takata")

i <- 1

# modelos_ajustados == lista dos modelos não lineares ajustados,
# vcom_estimado == :
for (i in 1:length(lista_nls)){
  
  message(i)
  
  nls_plots[[i]] <- local({
    
    i <- i
    
    sumario <- summary(lista_nls[[i]])
    
    ggplot(
      cubagem, 
      aes(x = cubagem_nls[[i]], 
          y = residuals(lista_nls[[i]]) / sumario$sigma) 
      ) +
      geom_point() +
      geom_hline(yintercept = c(tinf, tsup), 
                 color = "red") +
      geom_hline(yintercept = 0, 
                 linetype = "dotted") +
      labs(x = "vcom estimado [m³]",
           y = "resíduos studentizados") +
      scale_y_continuous(limits = c(-8, 8),
                         breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
      scale_x_continuous(limits = c(0.0, 0.6),
                         breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
      theme_bw()
    
  })
} 

# cubagem_lm == vprod_6 estimados para os modelos lineares
# lista_lm == lista dos modelos lineares ajustados

lm_plots <- vector("list", 3)

names(lm_plots) <- list(a = "Spurr", 
                        b = "Spurr logarítmico", 
                        c = "Schumacher logarítmico")

i <- 1

for (i in 1:length(lista_lm)){
  
  message(i)
  
  lm_plots[[i]] <- local({
    
    i <- i
    
    # o modelo de spurr (1) não utiliza a exponencial:
    
    if (i == 1) { 
      ggplot(
        cubagem, 
        aes(x = cubagem_lm[[i]], 
            y = rstudent(lista_lm[[i]])
        )
      ) +
        geom_point() +
        geom_hline(yintercept = c(tinf, tsup), 
                   color = "red") +
        geom_hline(yintercept = 0, 
                   linetype = "dotted") +
        labs(x = "vprod_6 estimado [m³]",
             y = "resíduos studentizados") +
        scale_y_continuous(limits = c(-8, 8),
                           breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
        scale_x_continuous(limits = c(0.0, 0.6),
                           breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
        theme_bw()
      
    } else{
      
      ggplot(
        cubagem, 
        aes(x = exp(cubagem_lm[[i]]), 
            y = rstudent(lista_lm[[i]])
        )
      ) +
        geom_point() +
        geom_hline(yintercept = c(tinf, tsup), 
                   color = "red") +
        geom_hline(yintercept = 0, 
                   linetype = "dotted") +
        labs(x = "vprod_6 estimado [m³]",
             y = "resíduos studentizados") +
        scale_y_continuous(limits = c(-8, 8),
                           breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
        scale_x_continuous(limits = c(0.0, 0.6),
                           breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
        theme_bw()
    }
  })
  
  
}


graficos <- ggarrange(ncol = 2, nrow = 3,
                      plotlist = list(
                        lm_plots$Spurr                    + ggtitle("Spurr"), 
                        lm_plots$`Spurr logarítmico`      + ggtitle("Spurr log."), 
                        lm_plots$`Schumacher logarítmico` + ggtitle("Schumacher & Hall log."),
                        
                        nls_plots$Berkhout                + ggtitle("Berkhout"), 
                        nls_plots$Schumacher              + ggtitle("Schumacher & Hall"), 
                        nls_plots$Takata                  + ggtitle("Takata"))
                      
)

par(mfrow = c(1, 2))
qqnorm_schumacher <- qqnormPlot(residuals(schumacher), title = FALSE, main = "Schumacher & Hall")


# Coeficientes ------------------------------------------------------------

coef(berkhout)

coef(schumacher)

coef(takata)

coef(spurr)

coef(spurr_log)

coef(schumacher_log)

# Quanto os valores obs variam dos estimados ------------------------------


# erro padrão residual (m^3) - syx

sumario_berkhout <- summary(berkhout)
sumario_schumacher <- summary(schumacher)
sumario_takata <- summary(takata)
sumario_spurr <- summary(spurr)
sumario_spurr_log <- summary(spurr_log)
sumario_schumacher_log <- summary(schumacher_log)

sumario_berkhout$sigma

sumario_schumacher$sigma

sumario_takata$sigma

sumario_spurr$sigma

sumario_spurr_log$sigma

sumario_schumacher_log$sigma


# erro padrão residual em porcentagem (%) - syxp

sumario_berkhout$sigma / mean(cubagem$vprod_6) * 100

sumario_schumacher$sigma / mean(cubagem$vprod_6) * 100

sumario_takata$sigma / mean(cubagem$vprod_6) * 100

sumario_spurr$sigma / mean(cubagem$vprod_6) * 100

sumario_spurr_log$sigma / mean(cubagem$vprod_6) * 100

sumario_schumacher_log$sigma / mean(cubagem$vprod_6) * 100



# R2 ajustado -------------------------------------------------------------

#spurr e spurr_log e schumacher_log


# coeficiente de determinação [m³]
R2adj_spurr <- sumario_spurr$adj.r.squared

R2adj_spurr_log <- sumario_spurr_log$adj.r.squared

R2adj_schumacher_log <- sumario_schumacher_log$adj.r.squared


# coeficiente de determinação (%)
R2adjp_spurr <- round(R2adj_spurr * 100, digits = 2)

R2adjp_spurr_log <- round(R2adj_spurr_log * 100, digits = 2)

R2adjp_schumacher_log <- round(R2adj_schumacher_log * 100, digits = 2)



# Índice de Furnival ------------------------------------------------------

# spurr_log e schumacher_log

y <- cubagem$vprod_6

# derivada de y em função de y
D(expression(log(y)), 'y')
dy <- 1/y

# Média geométrica
medgeo <- exp(mean(log(dy), na.rm = T))

# escala convertida de lnm^3 para m^3:
furnival_spurr_log <- 1/medgeo * sumario_spurr_log$sigma

furnival_schumacher_log <- 1/medgeo * sumario_schumacher_log$sigma


# furnival em %:
furnival_perc_spurr_log <- round(furnival_spurr_log/mean(y) * 100, digits = 2)


furnival_perc_schumacher_log <- round(furnival_schumacher_log/mean(y) * 100, digits = 2)




#Schumacher & Hall                v = β0 × dapβ1 × htβ2
selarv$vprod_6 <- (bs[1] * (selarv$dap ^ bs[2]) * (selarv$htre^bs[3]))

#Schumacher & Hall log            ln(v) = β0 + β1 × ln (dap) + β2 × ln (ht)
#selarv$vtsc<- with(selarv, exp(bs[1] + bs[2] * log(selarv$dap) + bs[3] * (log(selarv$htre))))

parc <- with(selarv,
             aggregate(list(vprod_6 = vprod_6),
                       list(codplantio = codplantio,
                            areaplantio = areaplantio,
                            parcela = parcela,
                            areaparc = areaparc,
                            espacamento = espacamento,
                            matgen = matgen, idade = idade, x = x, y = y),
                       sum))
#criar coluna somando vprod_6 e vicc, de acordo com os agentes agregadores fazenda, talh?o, area etc; #sum= somar volumes por parcela

write.csv2(parc, './trabalho_final/hipso_parcela.csv')

