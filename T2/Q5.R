# Joana da Matta Furtado Ferreira - 2110255

# para gerar as variáveis, utilizarei o método polar
# o ajuste para média = 15 e desvio padrão = 6 será feito somando mu e multiplicando sigma

gera.variaveis <- function(mu, sigma){
  u1 <- runif(1)
  u2 <- runif(1)
  
  r <- sqrt(-2*log(u1))
  theta <- 2*pi*u2
  
  x <- r * cos(theta)* sigma + mu
  y <- r * sin(theta)* sigma + mu
  
  return (list(x=x,y=y))
}

variaveis <- gera.variaveis(15,6)
print(variaveis)
