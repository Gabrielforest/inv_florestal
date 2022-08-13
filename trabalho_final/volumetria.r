# volumetria --------------------------------------------------------------
cub <- read.csv2("./trabalho_final/cubagem.csv")
selarv <- cub
selarv$dicc <- (cub$dicc1 + cub$dicc2)/2
selarv$dicc1 <- NULL
selarv$dicc2 <- NULL
selarv$matgen <- NULL
selarv <- selarv[, c("arv", "dap", "ht", "hi", "dicc", "espcasca")]

# eliminando dados:
# atribuindo d2 == d1 caso d2<d1 

fdim <- function(x, di){
  x <- x[order(x$hi),]
  repeat{
    ii <- (1:nrow(x))[c(0, diff(x[[di]])) > 0]
    if(length(ii) > 0){
      x[[di]][ii] <- x[[di]][ii - 1]
    }else{
      break
    }
  }  
  return(x)
}
selarv <- Reduce("rbind", lapply(split(selarv, selarv$arv), fdim, 'dicc'))
selarv$disc <- selarv$dicc - 2*selarv$espcasca
selarv <- Reduce("rbind", lapply(split(selarv, selarv$arv), fdim, 'disc'))
selarv$espcasca <- with(selarv, (dicc - disc)/2)
selarv$disc <- NULL
#seqmed <- cmrinvflor::parear_seqmed(selarv[, c("arv", "hi", "dicc")])

vsmal <- cmrinvflor::smalian(selarv, dcoms = 6, htoco = 10, comcasca = FALSE, di_ou_ci = "di", dbase_ponta = 6)

# comportamento dos dados:
with(vsmal, plot(log(dap^2*ht), log(vprod_6)))
with(vsmal, plot(dap^2*ht, vprod_6))

# removendo árvore 57, outlier
#vsmal <- vsmal[-57,]

vsmal$ffcom <- with(vsmal, vprod_6/vcilcc)

write.csv2(vsmal, "./trabalho_final/cubagem_rigorosa.csv")
