testador de modelos hahaha
melhor_AICc =Inf
for(p in 0:2){
  for(q in 0:2){
    for(Q in 0:2){
      for(P in 0:2){
        fit = Arima(treino_box,order=c(p,1,q),seasonal=c(P,0,Q))
        if(fit$aicc < melhor_AICc){
          melhor_AICc = fit$aicc
          cat("p =" ,p, ", q =" ,q, ", P = " ,P,  ",Q = " ,Q, ", AICc =", fit$aicc, "\n")
        } 
      }
    }
  }
}
Arima(treino,order=c(0,1,2),seasonal=c(1,0,1))

auto.arima(treino_box)

