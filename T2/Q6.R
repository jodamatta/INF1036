# Joana da Matta Furtado Ferreira - 2110255

simula.atendimento <- function(tempo.max){
  qtd.atendidos <- 0
  qtd.n.atendidos <- 0
  tempo.chegada <- 0
  
  # vou armazenar no vetor caixas o tempo que os caixas serao liberados.
  # por exemplo, um caixa que ainda nao atendeu ninguem tem tempo 0
  # um caixa que atendeu um cliente que chegou com tempo 2 por 5 minutos tera o valor caixa[i] = 7
  # assim, consigo ver quais caixas estao liberados quando um cliente chega
  caixas <- rep(0,4)
  
  
  while(tempo.chegada < tempo.max){
    tempo.chegada <- tempo.chegada + rexp(1,rate = 1)
    aux = FALSE
    for(i in 1:4){
      if(caixas[i] < tempo.chegada){
        caixas[i] <- tempo.chegada + rexp(1,rate = 1/3) # o caixa estara ocupado ate o fim do atendimento
        qtd.atendidos <- qtd.atendidos + 1
        aux <- TRUE
        break
      }
    }
    if(aux == FALSE){
      # nao foram achados caixas disponiveis
      qtd.n.atendidos <- qtd.n.atendidos + 1
    }
  }

  return(c(qtd.atendidos, qtd.n.atendidos))
}

# letra a)
tmax <- 12000
atendimento <- simula.atendimento(tmax)
# realizados:
print(atendimento[1])
# nao realizados:
print(atendimento[2])

# letra b)
nsamples <- 30

atendimentos.realizados <- 0
atendimentos.n.realizados <- 0
for (i in 1:nsamples){
  atendimento <- simula.atendimento(tmax)
  atendimentos.realizados <- atendimentos.realizados + atendimento[1]
  atendimentos.n.realizados <- atendimentos.n.realizados + atendimento[2]
}

media.realizados <- atendimentos.realizados/nsamples
media.n.realizados <- atendimentos.n.realizados/nsamples

print(media.realizados)
print(media.n.realizados)
