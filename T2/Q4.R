# Joana da Matta Furtado Ferreira - 2110255


# p(x)/h(x) = (20*x*(1-x)^3) / (1/(1-0)) = (20*x*(1-x)^3)
# agora, temos que encontrar o valor de x que maximiza p(x)/h(x).
# derivando essa função e igualando a zero, achamos o máximo em x = 1/4, e p(1/4)/h(1/4) = 2.109375

p <- function(x){
  return(20*x*(1-x)^3)
}

# inversa da uniforme, para gerar Y (funcao de densidade uniforme)

h.inversa <- function(x){
  return(0+(1-0)*x)
}

gera.variaveis <- function(nsamples){
  X <- rep(0,nsamples)
  c <- 2.109375
  for(i in 1:nsamples){
    aux = TRUE
    while(aux){
      u1 <- runif(1)
      y <- h.inversa(u1)
      u2 <- runif(1)
      if(u2 <= (p(y)/c*1)){
        X[i] = y
        aux = FALSE
      }
    }
  }
  return(X)
}

vars <- gera.variaveis(10000)
hist(vars)
