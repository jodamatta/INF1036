# Joana da Matta Furtado Ferreira - 2110255

gera.variaveis <- function(funcao, nsamples){
  X <- rep(0,nsamples)
  U <- runif(nsamples)
  
  # caso 1: f(x)
  # a funcao de distribuicao acumulada de f(x) é F(X) = Int(3k^2, 0..x) = x^3, 0 <= x <= 1
  # a inversa de F(x) é F^(-1)(x) = x^(1/3)
  
  if(funcao == 1){
    for(i in 1:nsamples){
      X[i] <- U[i]^(1/3)
    }
  }
  
  # caso 2: g(x)
  # a funcao de distribuicao acumulada de g(x) é Int(4k dk,0..x), 0 <= x <= 0.5 e Int(4x, 0..0.5)+Int(4(1-k) dk,0.5..x), 0.5<=x<=1
  # então, G(x) é (4x^2)/2 = 2x^2, 0<=x<=0.5, e 0.5 + (-2x^2 + 4x - 1.5) = -2x^2 + 4x - 1, 0.5 < x <= 1
  # as inversas de G(x) sao: sqrt(x/2), 0 <= x <= 0.5 e 1 + (sqrt(-2x+2))/2
  
  else if(funcao == 2){
    for(i in 1:nsamples){
      if(U[i] <= 0.5){
        X[i] <- sqrt(U[i]/2)
      }else{
        X[i] <- 1 - sqrt(-2*U[i] +2)/2
      }
    }
  }
  return(X)
}

var.fx <- gera.variaveis(1,10000)
hist(var.fx)

var.gx <- gera.variaveis(2,10000)
hist(var.gx)
