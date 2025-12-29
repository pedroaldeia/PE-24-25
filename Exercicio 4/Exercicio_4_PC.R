E_analitico = 25*gamma(1+ 1/5)
set.seed(1742)  # fixar semente
amostra <- rweibull(7500, shape = 5, scale = 25)
E_montecarlo <- mean(amostra)
E_montecarlo
E_analitico
abs(E_analitico - E_montecarlo)

