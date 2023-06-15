# Joana da Matta Furtado Ferreira - 2110255


# usamos a função de integrar para aproximar o valor de c
# para isso, fazemos a soma de n variaveis aleatorias aplicadas em f, divididas por n

integra.f <- function(c,nsamples){
  sum = 0
  for(i in 1:nsamples){
    x = runif(1)
    sum = sum + c*(x-x^2)
  }
  return(sum/nsamples)
}

# para achar c, temos que ter um valor tal que int(c(x-x^2)), 0 <= x <= 1, = 1.
# assim, chamamos a função integra para ver qual valor no intervalo [-1,12] tem o valor proximo de 1, com erro de (-0.5,0.5)

valor_c <- function(nsamples){
  for(c in -1:12){
    int = integra.f(c,nsamples)
    if(int >= 0.95 & int <= 1.05){
      return(c)
    }
  }
}

c = valor_c(10000)
print(c)

# para calcular a esperança, temos que calcular a integral de f(x)*x, 0 <= x <= 1
# fazemos isso calculando a soma de n variaveis aplicadas em f(x)*x, e depois dividimos elas por n

esperanca <- function(c, nsamples){
  sum = 0
  for(i in 1:nsamples){
    x = runif(1)
    sum = sum + c*(x-x^2)*x
  }
  return(sum/nsamples)
}

# para calcular a variancia, calculamos a integral de (x-esperanca)^2 * f(x), 0 <= x <= 1
# usando a mesma logica da esperança, calculamos a integral a partir da soma dividida por nsamples

varx <- function(c, esperanca, nsamples){
  sum = 0
  for(i in 1:nsamples){
    x = runif(1)
    sum = sum + (x - esperanca)^2 * c*(x-x^2) 
  }
  return(sum/nsamples)
}
ex <- esperanca(c,10000)
print(ex)

print(varx(c,ex,10000))
