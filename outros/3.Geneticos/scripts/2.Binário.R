#Formacao Inteligencia Artificial e Machine Learning. Fernando Amaral
#cria data frame com os itens da mochila 
mochila <- data.frame(item = c("canivete", "feijao", "batatas", "lanterna", 
    "saco de dormir", "corda", "bussula"), pontos = c(10, 20, 15, 2, 30, 
    10, 30), peso = c(1, 5, 10, 1, 7, 5, 1))

#funcao de adaptacao
f <-function(x)
{
  pontos = 0
  peso = 0
  for (i in 1:7)
  {
	#1001011
    if (x[ i ] != 0)
    {
 
      pontos = pontos + mochila[i,2]
      peso = peso +  mochila[i,3]
      
    }
  }
  #Limite de 15 Kg
  if (peso > 15)
    pontos = 0
  
  return( pontos)
}

#algoritmo genetico - popSize maior sempre repete o resultado porém, se menor o resultado varia.
resultado = ga("binary", fitness = f, nBits = 7,popSize = 50, maxiter = 15,  names= c("canivete", "feijão", "batatas", "lanterna", "saco de dormir", "corda", "bussula"))

#resultados
summary(resultado)
mochila

#solucao
summary(resultado)$solution

#grafico de evolucao
plot(resultado)























