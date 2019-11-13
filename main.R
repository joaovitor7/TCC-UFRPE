library(dplyr)
library(forecast)
library(xts)
library(urca)
library(tseries)
library(ggplot2)
library(astsa)
library(expand)
#csv to ts
dt <- read.csv(file="SM.csv", header=TRUE, sep=",")
ts <- ts(dt$data,frequency=4)
treino=window(ts,start=c(1,1),end=c(12,1))
teste=window(ts,start=c(12,2),end=c(14,1))
#testes de estacionaridade  
print(adf.test(treino))
111111
010010
110010

acf(treino)
treinoDiff = diff(treino)
acf(treinoDiff)
treinoDiff = diff(treinoDiff)
acf(treinoDiff)
treinoDiff = diff(treinoDiff)
acf(treinoDiff)
treinoDiff = diff(treinoDiff)
acf(treinoDiff)
#mudanca do FAC 4, ordem de dff igual a frequencia
treinoDiff = diff(treinoDiff)
acf(treinoDiff)
acf(ts)
tsDiff = diff(ts)
acf(tsDiff)
tsDiff = diff(tsDiff)
acf(tsDiff)
tsDiff = diff(tsDiff)
acf(tsDiff)
tsDiff = diff(tsDiff)
acf(tsDiff)
#mudanca do FAC 4, ordem de dff igual a frequencia
tsDiff = diff(tsDiff)
acf(tsDiff)
#tests
resultados = data.frame("p"=numeric(),"d"=numeric(),"q"=numeric(),"P"=numeric(),"D"=numeric(),"Q"=numeric(), "RMSE"=character(), "AICC"=character())
combinacoes = expand.grid(rep(list(0:5), 4))
for (i in 1:nrow(combinacoes)) {
  
  
  row = y[i,]
  modelo = tryCatch({
    
    Arima(treino, order=c(row$Var1,1,row$Var2), seasonal=list(order = c(row$Var3,1,row$Var4), period = 4) , method = 'ML')
    RSS = c(crossprod(modelo$residuals))
    MSE = RSS / length(modelo$residuals)
    
    p = row$Var1
    d = 1
    q = row$Var2
    P = row$Var3
    D = 1
    Q = row$Var4
    
    
    
    
    RMSE = toString(sqrt(MSE))
    
    AICC = toString(modelo$aicc)
    linha = data.frame("p"=p,"d"=d,"q"=q,"P"=P,"D"= D,"Q"= Q, "RMSE"=RMSE, "AICC"=AICC)
    print(linha)
    
    resultados = rbind(resultados,linha)
    
    
  }, warning = function(warning_condition) {}, error = function(error_condition) {}, finally={})
  
  #resultados = rbind(resultados,list(p,d,q,P,D,Q,RMSE,AICC))


}

print(resultados)


resultados1 = resultados[3:4,]
resultados2 = resultados[9:10,]
resultados3 = resultados[21:22,]
resultados4 = resultados[27,]
resultados5 = resultados[39,]
resultados6 = resultados[45:46,]
resultados7 = resultados[57,]
resultados8 = resultados[75,]
resultados9 = resultados[117,]
resultadoFinal = rbind(resultados1,resultados2)
resultadoFinal = rbind(resultadoFinal,resultados3)
resultadoFinal = rbind(resultadoFinal,resultados4)
resultadoFinal = rbind(resultadoFinal,resultados5)
resultadoFinal = rbind(resultadoFinal,resultados6)
resultadoFinal = rbind(resultadoFinal,resultados7)
resultadoFinal = rbind(resultadoFinal,resultados8)
resultadoFinal = rbind(resultadoFinal,resultados9)

print(resultadoFinal)

write.csv(resultadoFinal,"MelhoresModelosByAICC.csv")


for (i in 1:nrow(resultadoFinal)) {
  
  
  row = resultadoFinal[i,]
  
  
  modelo = Arima(treino, order=c(row[[1]],row[[2]],row[[3]]), seasonal=list(order = c(row[[4]],row[[5]],row[[6]]), period = 4) , method = 'ML')
  prev =forecast(modelo,h=8)
  plot(prev)
  print(modelo)
  print(accuracy(prev,teste))
  print(shapiro.test(modelo$residuals))
  
}
get30porcent = function(x){
  
  y = (30*x)/100
  return(-y)
  
}
modelo = Arima(treino, order=c(0,1,1), seasonal=list(order = c(0,1,0), period = 4) , method = 'ML')
prev =forecast(modelo,h=8)

for (i in prev$mean){
  
  cdf = ecdf(sample(modelo$residuals,1000,replace = T))
  cdf2 = ecdf(sample(modelo$residuals+4500,1000,replace = T))
  print(i)
  print(cdf(get30porcent(i))*100)#probablidade da produção cair 30% do previsto
  print(cdf2(i)*100)#probabilidade de funcionamento perfeito

}
#bias
#residuals
#Decomposicao
dec = decompose(ts)
autoplot(dec)


