# Joana da Matta Furtado Ferreira - 2110255

gera.variaveis <- function(funcao, nsamples){
  X <- rep(0,nsamples)
  U <- runif(nsamples)
  
  # caso 1: f(x)
  # a funcao de distribuicao acumulada de f(x) é F(X) = Int(3x^2, 0..x) = x^3, 0 <= x <= 1
  # a inversa de F(x) é F^(-1)(x) = x^(1/3)
  
  if(funcao == 1){
    for(i in 1:nsamples){
      X[i] <- U[i]^(1/3)
    }
  }
  
  # caso 2: g(x)
  # a funcao de distribuicao acumulada de g(x) é Int(4x,0..x), 0 <= x <= 0.5 e Int(4x, 0..0.4)+Int(4(1-x),0.4..x), 0.4<=x<=1
  # então, F(x) é (4x^2)/2 = 2x^2, 0<=x<=0.4, e 0.32 + (-2x^2 + 4x - 1.28) = -2x^2 + 4x - 0.96, 0.4 < x <= 1
  # as inversas de F(x) sao: sqrt(x/2), 0 <= x <= 0.4 e - (-400+sqrt(-80000x+83200))/400
  
  else if(funcao == 2){
    for(i in 1:nsamples){
      if(U[i] <= 0.4){
        X[i] <- sqrt(U[i]/2)
      }else{
        X[i] <- -(-400+sqrt(-80000*U[i]+83200))/400
      }
    }
  }
  return(X)
}

var.fx <- gera.variaveis(1,10000)
hist(var.fx)

var.gx <- gera.variaveis(2,10000)
hist(var.gx)
