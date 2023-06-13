# Joana da Matta Furtado Ferreira - 2110255

# Questão 1:

nsamples <- 10000
count.dentro.base <- 0

for(i in 1:nsamples){
  valor.x <- runif(1)
  valor.y <- runif(1)
  if(valor.x < 0.5){
    if((valor.y^2) >= (1-valor.x^2)){
      count.dentro.base = count.dentro.base + 1
    }
  }
  if(valor.x >= 0.5 & valor.x < 0.8666){
    if(valor.y >= 0.5 & valor.y <= (1-valor.x^2)){
      count.dentro.base = count.dentro.base + 1
    }
  }
  if(valor.x >=0.8666){
    if((valor.y^2) >= (1-valor.x^2)){
      count.dentro.base = count.dentro.base + 1
    }
  }
} 
area.aproximada = count.dentro.base /nsamples
volume <- area.aproximada * 5
preco <- 95 * volume
print(preco)

