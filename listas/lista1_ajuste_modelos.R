#### Gabriel de Freitas Pereira
# lista 1

#A planilha de dados em anexo (“cubagem.csv”) contém os resultados por árvore de uma cubagem rigorosa realizada
#em um povoamento clonal de Eucalyptus grandis no Sul do Estado de São Paulo. Utilizando os modelos 
#volumétricos apresentados na tabela 1, ajuste e selecione o melhor modelo para a estimativa do volume
#comercial com casca (vcomcc) das árvores deste povoamento. Justifique a sua escolha. (100%)

#Berkhout                         v = β0 × dap^β1

#Spurr                            v = β0 + β1 (dap^2ht)

#Spurr(logarítmica)               ln(v) = β0 + β1 × ln (dap^2ht)

#Schumacher & Hall                v = β0 × dapβ1 × htβ2

#Schumacher & Hall(logarítmica)   ln(v) = β0 + β1 × ln (dap) + β2 × ln (ht)

#Takata                           v =  dap^2ht / (β0 + β1 × dap)


# lendo os dados de inventário: -------------------------------------------

dados <- read.csv2("./listas/cubagem_lista1.csv")
lenght(dados)
# arv   dap   ht    vcomcc(volume comercial com casca)   vcomsc(volume comercial sem casca) 

names(dados)


# formulas que serão usadas: ----------------------------------------------

formulas <- list(
  "vcomcc ~ I(b0 * dap^b1)",                     #1 não linear
  "vcomcc ~ I(dap^2 * ht)",                      #2 linear
  "log(vcomcc) ~ I(log(dap^2 * ht))",            #3 linear
  "vcomcc ~ I(b0 * dap^b1 * ht^b2)",             #4 não linear
  "log(vcomcc) ~ log(dap) + log(ht)",            #5 linear
  "vcomcc ~ I((dap^2 * ht) / (b0 + b1 * dap))")  #6 não linear

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
berkhout <- nls(data = dados,
                formula = formulas$berkhout,
                start = list(b0 = 0.000098, b1 = 2.48389)
)

schumacher <- nls(data = dados,
                  formula = formulas$schumacher,
                  start = list(b0 = pi/(40000*0.45), b1 = 2, b2 = 1)
)

takata <- nls(data = dados,
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
spurr <- lm(data = dados,
            formula = formulas$spurr
)

spurr_log <- lm(data = dados,
                formula = formulas$spurr_log
)

schumacher_log <- lm(data = dados,
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

dados$vcomcc_est_berkhout <- predict(berkhout)

dados$vcomcc_est_spurr <- predict(spurr)

dados$vcomcc_est_spurr_log <- predict(spurr_log)

dados$vcomcc_est_schumacher <- predict(schumacher)

dados$vcomcc_est_schumacher_log <- predict(schumacher_log)

dados$vcomcc_est_takata <- predict(takata)


# berkhout == 6, schumacher == 9 e takata == 11:
dados_nls <- dados[, c(6, 9, 11)]

# spurr == 7, spurr_log == 8 e schumacher_log == 10:
dados_lm <- dados[, c(7, 8, 10)]


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
tinf <- qt(0.025, nrow(dados) - 1)

#t(alpha=5 e [n-1] gl)  
tsup <- qt(0.975, nrow(dados) - 1)



# dados_nls == vcomcc estimados para os modelos não lineares
# lista_nls == lista dos modelos não lineares ajustados (Berkhout Schumacher e Takata)


# modelos_ajustados == lista dos modelos não lineares ajustados,
# vcom_estimado == :
graficos <- purrr::map2(.x = lista_nls, .y = dados_nls,
                        .f = function(modelos_ajustados, vcom_estimado){
                          sumario <- summary(modelos_ajustados)
                          
                          p1 <- ggplot(
                            dados, 
                            aes(x = vcom_estimado, 
                                y = residuals(modelos_ajustados) / sumario$sigma
                            )
                          ) +
                            geom_point() +
                            geom_hline(yintercept = c(tinf, tsup), 
                                       color = "red") +
                            geom_hline(yintercept = 0, 
                                       linetype = "dotted") +
                            labs(x = "vcom estimado [m³]",
                                 y = "resíduos padronizados") +
                            scale_y_continuous(limits = c(-3, 3),
                                               breaks = c(-3, -2, -1, 0 , 1, 2, 3)) +
                            scale_x_continuous(limits = c(0.0, 0.5),
                                               breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
                            theme_bw()
                          
                          return(list(modelos_ajustados, p1))
                          
                        })



# dados_lm == vcomcc estimados para os modelos lineares
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
        dados, 
        aes(x = dados_lm[[i]], 
            y = rstudent(lista_lm[[i]])
        )
      ) +
        geom_point() +
        geom_hline(yintercept = c(tinf, tsup), 
                   color = "red") +
        geom_hline(yintercept = 0, 
                   linetype = "dotted") +
        labs(x = "vcomcc estimado [m³]",
             y = "resíduos studentizados") +
        scale_y_continuous(limits = c(-3, 3),
                           breaks = c(-3, -2, -1, 0 , 1, 2, 3)) +
        scale_x_continuous(limits = c(0.0, 0.5),
                           breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
        theme_bw()
      
    } else{
      
      ggplot(
        dados, 
        aes(x = exp(dados_lm[[i]]), 
            y = rstudent(lista_lm[[i]])
        )
      ) +
        geom_point() +
        geom_hline(yintercept = c(tinf, tsup), 
                   color = "red") +
        geom_hline(yintercept = 0, 
                   linetype = "dotted") +
        labs(x = "vcomcc estimado [m³]",
             y = "resíduos studentizados") +
        scale_y_continuous(limits = c(-3, 3),
                           breaks = c(-3, -2, -1, 0 , 1, 2, 3)) +
        scale_x_continuous(limits = c(0.0, 0.5),
                           breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
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

annotate_figure(graficos,
                bottom = text_grob("Figura 2. Gráfico de resíduos dos modelos volumétricos",
                                    face = "bold")
)

# Coeficientes ------------------------------------------------------------

round(coef(berkhout), digits = 8)

round(coef(schumacher), digits = 8)

round(coef(takata), digits = 9)

round(coef(spurr), digits = 8)

round(coef(spurr_log), digits = 9)

round(coef(schumacher_log), digits = 8)

# Quanto os valores obs variam dos estimados ------------------------------


# erro padrão residual (m^3) - syx

sumario_berkhout <- summary(berkhout)
sumario_schumacher <- summary(schumacher)
sumario_takata <- summary(takata)
sumario_spurr <- summary(spurr)
sumario_spurr_log <- summary(spurr_log)
sumario_schumacher_log <- summary(schumacher_log)

round(sumario_berkhout$sigma, digits = 2)

round(sumario_schumacher$sigma, digits = 2)

round(sumario_takata$sigma, digits = 2)


round(sumario_spurr$sigma, digits = 2)

round(sumario_spurr_log$sigma, digits = 2)

round(sumario_schumacher_log$sigma, digits = 2)


# erro padrão residual em porcentagem (%) - syxp

round(sumario_berkhout$sigma / mean(dados$vcomcc) * 100, digits = 2)

round(sumario_schumacher$sigma / mean(dados$vcomcc) * 100, digits = 2)

round(sumario_takata$sigma / mean(dados$vcomcc) * 100, digits = 2)


round(sumario_spurr$sigma / mean(dados$vcomcc) * 100, digits = 2)

round(sumario_spurr_log$sigma / mean(dados$vcomcc) * 100, digits = 2)

round(sumario_schumacher_log$sigma / mean(dados$vcomcc) * 100, digits = 2)



# R2 ajustado -------------------------------------------------------------

#spurr e spurr_log e schumacher_log


# coeficiente de determinação [m³]
R2adj_spurr <- round(sumario_spurr$adj.r.squared, digits = 2)

R2adj_spurr_log <- round(sumario_spurr_log$adj.r.squared, digits = 2)

R2adj_schumacher_log <- round(sumario_schumacher_log$adj.r.squared, digits = 2)


# coeficiente de determinação (%)
R2adjp_spurr <- round(R2adj_spurr * 100, digits = 2)

R2adjp_spurr_log <- round(R2adj_spurr_log * 100, digits = 2)

R2adjp_schumacher_log <- round(R2adj_schumacher_log * 100, digits = 2)



# Índice de Furnival ------------------------------------------------------

# spurr_log e schumacher_log

y <- dados$vcomcc

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













