# Parâmetros
set.seed(1254)
m <- 1000
n <- 12
mu <- 0.9
sigma <- 0.9
gamma <- 0.91

# Valor crítico da Normal padrão (como sigma é conhecido)
z <- qnorm(1 - (1 - gamma)/2)

# Simulação
conta <- 0
for (i in 1:m) {
  amostra <- rnorm(n, mean = mu, sd = sigma)
  media_amostra <- mean(amostra)
  erro <- z * sigma / sqrt(n)
  li <- media_amostra - erro
  ls <- media_amostra + erro
  
  if (li <= mu && mu <= ls) {
    conta <- conta + 1
  }
}

# Proporção de ICs que contêm a média verdadeira
prop <- conta / m

# Quociente
quociente <- round(prop / gamma, 4)

# Exibir resultado
cat("Proporção de ICs que contêm a média:", prop, "\n")
cat("Quociente entre proporção e gamma:", quociente, "\n")
