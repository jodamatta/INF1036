# Joana da Matta Furtado Ferreira - 2110255

gera.variaveis <- function(funcao, nsamples){
  u = runif(1)
  
  # caso 1: f(x)
  # a funcao de distribuicao acumulada de f(x) é F(X) = Int(3x^2, 0..x) = x^3, 0 <= x <= 1
  # a inversa de F(x) é F^(-1)(x) = x^(1/3)
  
  if(funcao == 1){
    for(i in 1:nsamples){
      return(u^(1/3))
    }
  }
  
  # caso 2: g(x)
  # a funcao de distribuicao acumulada de g(x) é Int(4x,0..x), 0 <= x <= 0.5 e Int(4x, 0..0.4)+Int(4(1-x),0.4..x), 0.4<=x<=1
  # então, F(x) é (4x^2)/2 = 2x^2, 0<=x<=0.4, e 0.32 + (-2x^2 + 4x - 1.28) = -2x^2 + 4x - 0.96, 0.4 < x <= 1
  # as inversas de F(x) sao: sqrt(x/2), 0 <= x <= 0.4 e 
  
  else if(funcao == 2){
    if(U[i] <= 0.4){
      return(sqrt(u/2))
    }else{
        #X[i] <-
    }
  }
}


