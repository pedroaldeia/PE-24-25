# Dados
n <- 13
sum_x <- 72.96
sum_log_x <- 22.4

# Função a ser zerada
f <- function(alpha) {
  log(alpha) - digamma(alpha) - (log(sum_x / n) - (sum_log_x / n))
}

# Usando uniroot para encontrar alpha
alpha_hat <- uniroot(f, interval = c(0.001, 265.5))$root

# Estimando lambda
lambda_hat <- alpha_hat / (sum_x / n)

# Moda
mode <- (alpha_hat - 1) / lambda_hat

# Resultado final arredondado
mode_rounded <- round(mode, 2)
cat("Comprimento modal estimado:", mode_rounded, "cm\n")
